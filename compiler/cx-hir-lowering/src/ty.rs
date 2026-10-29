use cx_hir::ast::{
    function::HIRFunctionPrototype,
    modifiers::{HIR_CONST, HIR_VOLATILE},
    types::{
        ANONYMOUS_MEMBER_PREFIX, HIRAggregateAttributes, HIRField, HIRMoveSemantics, HIRTagKind,
        HIRType, HIRTypeKind,
    },
};
use cx_hmir::{
    HMIRContract, HMIRFnSignature, HMIRFunctionStage,
    expr::{HMIRExprID, HMIRExprKind},
    ty::{
        HMIRExprTypeKind,
        aggregate::{HMIRDeclaredMove, HMIRFieldDef, HMIRMoveSemantics, HMIRTypeSemantics},
    },
    unit::function::HMIRFnParam,
};
use cx_log::{CXResult, catalogue::typecheck};
use cx_namespace::module::QualifiedName;
use cx_tokens::TokenRange;
use cx_util::identifier::CXIdent;

use crate::{
    body::{BodyLowering, Symbol},
    def::declared_params,
    expr::{call::lower_arguments, lower_expr},
    log::log_hir_error,
};

pub(crate) fn lower_type(context: &mut BodyLowering<'_>, ty: &HIRType) -> CXResult<HMIRExprID> {
    let lowered = lower_unqualified_type(context, ty)?;
    if matches!(ty.kind, HIRTypeKind::Auto) {
        return Ok(lowered);
    }

    let semantics = HMIRTypeSemantics {
        constant: ty.specifiers & HIR_CONST != 0,
        volatile: ty.specifiers & HIR_VOLATILE != 0,
        ..HMIRTypeSemantics::default()
    };
    if !semantics.constant && !semantics.volatile {
        return Ok(lowered);
    }
    let kind = HMIRExprTypeKind::Nominal(lowered);
    Ok(context.expr_type(kind, semantics, None, &ty.range))
}

fn lower_unqualified_type(context: &mut BodyLowering<'_>, ty: &HIRType) -> CXResult<HMIRExprID> {
    let span = &ty.range;
    if let Some((params, result)) = staged_signature(ty) {
        return lower_staged_type(context, params, result, span);
    }

    let kind = match &ty.kind {
        HIRTypeKind::Identifier { name, args, .. } => {
            let callee = lower_type_name(context, name, ty.tag_kind(), span)?;
            let Some(args) = args else {
                return Ok(callee);
            };

            let (args, named) = lower_arguments(context, args)?;
            return Ok(context.push(HMIRExprKind::Call { callee, args, named }, span));
        }
        HIRTypeKind::Universe => return Ok(context.type_of_types(span)),
        HIRTypeKind::Auto => return Ok(context.hole(span)),
        // Handled as a staged signature above
        HIRTypeKind::Expr(result) => return lower_staged_type(context, Vec::new(), result, span),
        HIRTypeKind::ExplicitSizedArray(element, length) => {
            let element = lower_type(context, element)?;
            let length = Some(lower_expr(context, length)?);
            HMIRExprTypeKind::Array { element, length }
        }
        HIRTypeKind::ImplicitSizedArray(element) => {
            let element = lower_type(context, element)?;
            let length = None;
            HMIRExprTypeKind::Array { element, length }
        }
        HIRTypeKind::MemoryReference { inner_type, .. } => HMIRExprTypeKind::ReferenceTo {
            inner: lower_type(context, inner_type)?,
            bitfield: None,
        },
        HIRTypeKind::PointerTo { inner_type } => {
            HMIRExprTypeKind::PointerTo(lower_type(context, inner_type)?)
        }
        HIRTypeKind::Structured {
            name,
            attributes,
            fields,
        } => {
            let fields = lower_fields(context, fields)?;
            let kind = HMIRExprTypeKind::Struct { fields };
            return lower_aggregate_type(context, name.as_ref(), kind, attributes, span);
        }
        HIRTypeKind::Union { name, fields } => {
            let fields = lower_fields(context, fields)?;
            let kind = HMIRExprTypeKind::Union { fields };
            let attributes = HIRAggregateAttributes::default();
            return lower_aggregate_type(context, name.as_ref(), kind, &attributes, span);
        }
        HIRTypeKind::TaggedUnion {
            name,
            attributes,
            variants,
        } => {
            let fields = lower_fields(context, variants)?;
            let kind = HMIRExprTypeKind::TaggedUnion { fields };
            return lower_aggregate_type(context, name.as_ref(), kind, attributes, span);
        }
        HIRTypeKind::FunctionPointer { prototype } => {
            HMIRExprTypeKind::Function(lower_function_type(context, prototype)?)
        }
    };

    Ok(context.expr_type(kind, HMIRTypeSemantics::default(), None, span))
}

fn lower_type_name(
    context: &mut BodyLowering<'_>,
    name: &QualifiedName,
    tag: Option<HIRTagKind>,
    span: &TokenRange,
) -> CXResult<HMIRExprID> {
    Ok(match context.lookup(name, tag) {
        Symbol::Local(binding) => context.push(HMIRExprKind::Local(binding.local()), span),
        Symbol::Def(def) => context.push(HMIRExprKind::Def(def), span),
        Symbol::Name => context.push(
            HMIRExprKind::Name {
                name: name.clone(),
                tag,
            },
            span,
        ),
    })
}

// The shape of a staged value: 'expr(T)', or a comptime function over code such as
// '@fn(expr(A)) -> expr(T)'. Code parameters are listed by the type of code they carry.
pub(crate) fn staged_signature(ty: &HIRType) -> Option<(Vec<&HIRType>, &HIRType)> {
    if let HIRTypeKind::Expr(result) = &ty.kind {
        return Some((Vec::new(), result));
    }

    let HIRTypeKind::PointerTo { inner_type } = &ty.kind else {
        return None;
    };
    let HIRTypeKind::FunctionPointer { prototype } = &inner_type.kind else {
        return None;
    };
    let result = match &prototype.return_type.kind {
        HIRTypeKind::Expr(result) => result.as_ref(),
        _ => return None,
    };
    let params = prototype
        .params
        .iter()
        .map(|param| match &param.ty.kind {
            HIRTypeKind::Expr(carried) => carried.as_ref(),
            _ => &param.ty,
        })
        .collect();
    Some((params, result))
}

fn lower_staged_type(
    context: &mut BodyLowering<'_>,
    params: Vec<&HIRType>,
    result: &HIRType,
    span: &TokenRange,
) -> CXResult<HMIRExprID> {
    let staged = |context: &mut BodyLowering<'_>, carried: &HIRType| {
        let kind = HMIRExprTypeKind::Staged(lower_type(context, carried)?);
        Ok(context.expr_type(kind, HMIRTypeSemantics::default(), None, span))
    };
    let result = staged(context, result)?;
    if params.is_empty() {
        return Ok(result);
    }
    let params = params
        .into_iter()
        .map(|param| Ok(HMIRFnParam::new(false, staged(context, param)?, false)))
        .collect::<CXResult<_>>()?;
    let stage = HMIRFunctionStage::Comptime;
    let signature = HMIRFnSignature::new(stage, params, result, false, HMIRContract::default());

    let kind = HMIRExprTypeKind::Function(signature);
    Ok(context.expr_type(kind, HMIRTypeSemantics::default(), None, span))
}

fn lower_function_type(
    context: &mut BodyLowering<'_>,
    prototype: &HIRFunctionPrototype,
) -> CXResult<HMIRFnSignature<HMIRExprID>> {
    let params = declared_params(prototype)
        .iter()
        .map(|param| {
            if let Some(default) = &param.default {
                return log_hir_error(
                    &default.range,
                    &typecheck::INVALID_CONTEXT,
                    ("a default".into(), "a function type".into()),
                );
            }
            let ty = lower_type(context, &param.ty)?;
            Ok(HMIRFnParam::new(param.comptime, ty, false))
        })
        .collect::<CXResult<_>>()?;
    let return_type = lower_type(context, &prototype.return_type)?;
    let stage = if prototype.comptime {
        HMIRFunctionStage::Comptime
    } else {
        HMIRFunctionStage::Runtime
    };

    Ok(HMIRFnSignature::new(
        stage,
        params,
        return_type,
        prototype.var_args,
        HMIRContract::default(),
    ))
}

fn lower_fields(
    context: &mut BodyLowering<'_>,
    fields: &[HIRField],
) -> CXResult<Vec<HMIRFieldDef<HMIRExprID>>> {
    fields
        .iter()
        .map(|field| {
            Ok(match field {
                // An anonymous member has no name of its own; its members are found through it
                HIRField::Standard { name, ty } => HMIRFieldDef::new(
                    (!name.starts_with(ANONYMOUS_MEMBER_PREFIX))
                        .then(|| CXIdent::from(name.as_str())),
                    lower_type(context, ty)?,
                    None,
                ),
                HIRField::Bitfield {
                    name,
                    integer_type,
                    width,
                } => HMIRFieldDef::new(
                    name.as_deref().map(CXIdent::from),
                    lower_type(context, integer_type)?,
                    Some(*width),
                ),
            })
        })
        .collect()
}

// Each declaration is a type of its own, which the expression it lowers to identifies
fn lower_aggregate_type(
    context: &mut BodyLowering<'_>,
    name: Option<&CXIdent>,
    kind: HMIRExprTypeKind,
    attributes: &HIRAggregateAttributes,
    span: &TokenRange,
) -> CXResult<HMIRExprID> {
    let move_semantics = match &attributes.copy_traits {
        Some(source) => {
            let source = QualifiedName::new_raw(CXIdent::from(source.as_str()));
            HMIRDeclaredMove::CopyTraits(lower_type_name(context, &source, None, span)?)
        }
        None => HMIRDeclaredMove::Explicit(match attributes.semantics {
            HIRMoveSemantics::POD => HMIRMoveSemantics::POD,
            HIRMoveSemantics::Nocopy => HMIRMoveSemantics::Nocopy,
            HIRMoveSemantics::Nodrop => HMIRMoveSemantics::Nodrop,
        }),
    };
    let semantics = HMIRTypeSemantics {
        constant: false,
        volatile: false,
        unsafe_move: attributes.unsafe_move,
        move_semantics,
    };

    Ok(context.expr_type(kind, semantics, name, span))
}
