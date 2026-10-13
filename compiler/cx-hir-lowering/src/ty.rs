use cx_hir::ast::{
    function::HIRFunctionPrototype,
    modifiers::HIR_CONST,
    types::{
        ANONYMOUS_MEMBER_PREFIX, HIRAggregateAttributes, HIRField, HIRMoveSemantics, HIRType,
        HIRTypeKind,
    },
};
use cx_hmir::{
    HMIRContract, HMIRDefRef, HMIRFnSignature, HMIRFunctionStage,
    expr::{HMIRExprID, HMIRExprKind},
    ty::{
        HMIRExprTypeKind,
        aggregate::{HMIRFieldDef, HMIRMoveSemantics, HMIRTypeSemantics},
    },
    unit::function::HMIRFnParam,
};
use cx_log::{CXResult, catalogue::typecheck};
use cx_tokens::TokenRange;
use cx_util::identifier::CXIdent;

use crate::{
    body::{BodyLowering, Symbol},
    expr::lower_expr,
    log::log_hir_error,
    resolve::GlobalSymbol,
};

pub(crate) fn lower_type(context: &mut BodyLowering<'_>, ty: &HIRType) -> CXResult<HMIRExprID> {
    let lowered = lower_unqualified_type(context, ty)?;
    // The type 'auto' stands for brings its own qualifiers
    if ty.specifiers & HIR_CONST == 0 || matches!(ty.kind, HIRTypeKind::Auto) {
        return Ok(lowered);
    }

    let semantics = HMIRTypeSemantics {
        constant: true,
        ..HMIRTypeSemantics::default()
    };
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
            let callee = match context.lookup(name, ty.tag_kind(), span)? {
                Symbol::Local(binding) => context.push(HMIRExprKind::Local(binding.local()), span),
                Symbol::Global(GlobalSymbol::Primitive(kind)) => context.type_constant(kind, span),
                Symbol::Global(
                    GlobalSymbol::Def(def)
                    | GlobalSymbol::Function(def)
                    | GlobalSymbol::ComptimeFunction(def, ..),
                ) => context.push(HMIRExprKind::Def(def), span),
                Symbol::Global(GlobalSymbol::Constructor(..)) => {
                    return log_hir_error(
                        span,
                        &typecheck::EXPECTED_TYPE,
                        format!("constructor '{name}'"),
                    );
                }
            };
            let Some(args) = args else {
                return Ok(callee);
            };

            let args = args
                .iter()
                .map(|arg| lower_expr(context, arg))
                .collect::<CXResult<_>>()?;
            return Ok(context.push(HMIRExprKind::Call { callee, args }, span));
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
        HIRTypeKind::MemoryReference { inner_type, .. } => {
            HMIRExprTypeKind::ReferenceTo(lower_type(context, inner_type)?)
        }
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
    let params = params
        .into_iter()
        .map(|param| lower_type(context, param))
        .collect::<CXResult<_>>()?;
    let result = lower_type(context, result)?;

    let kind = HMIRExprTypeKind::StagedExpr { params, result };
    Ok(context.expr_type(kind, HMIRTypeSemantics::default(), None, span))
}

// The sum a constructor builds, as the def of its type
pub(crate) fn lower_constructor_sum(
    context: &mut BodyLowering<'_>,
    union_type: &HIRType,
    span: &TokenRange,
) -> CXResult<HMIRExprID> {
    let HIRTypeKind::Identifier { name, .. } = &union_type.kind else {
        return lower_type(context, union_type);
    };
    let def = match context.lookup(name, union_type.tag_kind(), span)? {
        Symbol::Global(GlobalSymbol::Def(def)) => def,
        _ => HMIRDefRef::External(name.clone()),
    };

    Ok(context.push(HMIRExprKind::Def(def), span))
}

fn lower_function_type(
    context: &mut BodyLowering<'_>,
    prototype: &HIRFunctionPrototype,
) -> CXResult<HMIRFnSignature<HMIRExprID>> {
    let params = prototype
        .params
        .iter()
        .map(|param| {
            let ty = lower_type(context, &param.ty)?;
            Ok(HMIRFnParam::new(param.comptime, ty))
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
    if attributes.copy_traits.is_some() {
        return log_hir_error(span, &typecheck::UNSUPPORTED_FEATURE, "@copy_traits".into());
    }

    let semantics = HMIRTypeSemantics {
        constant: false,
        unsafe_move: attributes.unsafe_move,
        move_semantics: match attributes.semantics {
            HIRMoveSemantics::POD => HMIRMoveSemantics::POD,
            HIRMoveSemantics::Nocopy => HMIRMoveSemantics::Nocopy,
            HIRMoveSemantics::Nodrop => HMIRMoveSemantics::Nodrop,
        },
    };

    Ok(context.expr_type(kind, semantics, name, span))
}
