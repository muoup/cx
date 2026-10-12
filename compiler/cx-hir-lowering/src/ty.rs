use cx_hir::ast::{
    function::HIRFunctionPrototype,
    modifiers::HIR_CONST,
    types::{
        ANONYMOUS_MEMBER_PREFIX, HIRAggregateAttributes, HIRField, HIRMoveSemantics, HIRType,
        HIRTypeKind,
    },
};
use cx_hmir::{
    HMIRDefRef,
    expr::{HMIRExprID, HMIRExprKind, constant::HMIRConstant, op::HMIRTypeOp},
    ty::{
        HMIRTypeKind,
        aggregate::{HMIRFieldDef, HMIRMoveSemantics},
    },
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

    Ok(context.type_op(HMIRTypeOp::Const(lowered), &ty.range))
}

fn lower_unqualified_type(context: &mut BodyLowering<'_>, ty: &HIRType) -> CXResult<HMIRExprID> {
    let span = &ty.range;
    if let Some((params, result)) = staged_signature(ty) {
        return lower_staged_type(context, params, result, span);
    }

    Ok(match &ty.kind {
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
            context.push(HMIRExprKind::Call { callee, args }, span)
        }
        HIRTypeKind::Universe => context.type_of_types(span),
        HIRTypeKind::Auto => context.hole(span),
        // Handled as a staged signature above
        HIRTypeKind::Expr(result) => lower_staged_type(context, Vec::new(), result, span)?,
        HIRTypeKind::ExplicitSizedArray(element, length) => {
            let element = lower_type(context, element)?;
            let length = Some(lower_expr(context, length)?);
            context.type_op(HMIRTypeOp::Array { element, length }, span)
        }
        HIRTypeKind::ImplicitSizedArray(element) => {
            let element = lower_type(context, element)?;
            let length = None;
            context.type_op(HMIRTypeOp::Array { element, length }, span)
        }
        HIRTypeKind::MemoryReference { inner_type, .. } => {
            let inner = lower_type(context, inner_type)?;
            context.type_op(HMIRTypeOp::Reference(inner), span)
        }
        HIRTypeKind::PointerTo { inner_type } => {
            let inner = lower_type(context, inner_type)?;
            context.type_op(HMIRTypeOp::Pointer(inner), span)
        }
        HIRTypeKind::Structured {
            name,
            attributes,
            fields,
        } => {
            let fields = lower_fields(context, fields)?;
            let kind = HMIRTypeKind::Struct { fields };
            lower_aggregate_type(context, name.as_ref(), kind, attributes, span)?
        }
        HIRTypeKind::Union { name, fields } => {
            let fields = lower_fields(context, fields)?;
            let kind = HMIRTypeKind::Union { fields };
            let attributes = HIRAggregateAttributes::default();
            lower_aggregate_type(context, name.as_ref(), kind, &attributes, span)?
        }
        HIRTypeKind::TaggedUnion {
            name,
            attributes,
            variants,
        } => {
            let fields = lower_fields(context, variants)?;
            let kind = HMIRTypeKind::TaggedUnion { fields };
            lower_aggregate_type(context, name.as_ref(), kind, attributes, span)?
        }
        HIRTypeKind::FunctionPointer { prototype } => lower_function_type(context, prototype)?,
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
    let params = params
        .into_iter()
        .map(|param| lower_type(context, param))
        .collect::<CXResult<_>>()?;
    let result = lower_type(context, result)?;

    Ok(context.type_op(HMIRTypeOp::Expr { params, result }, span))
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
) -> CXResult<HMIRExprID> {
    let params = prototype
        .params
        .iter()
        .map(|param| lower_type(context, &param.ty))
        .collect::<CXResult<_>>()?;
    let ret = lower_type(context, &prototype.return_type)?;

    Ok(context.type_op(
        HMIRTypeOp::Function {
            params,
            ret,
            variadic: prototype.var_args,
        },
        &prototype.range,
    ))
}

fn lower_fields(
    context: &mut BodyLowering<'_>,
    fields: &[HIRField],
) -> CXResult<Vec<HMIRFieldDef>> {
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

// Each declaration is a type of its own, whose fields are typed by expressions of this body
fn lower_aggregate_type(
    context: &mut BodyLowering<'_>,
    name: Option<&CXIdent>,
    kind: HMIRTypeKind,
    attributes: &HIRAggregateAttributes,
    span: &TokenRange,
) -> CXResult<HMIRExprID> {
    if attributes.copy_traits.is_some() {
        return log_hir_error(span, &typecheck::UNSUPPORTED_FEATURE, "@copy_traits".into());
    }

    let semantics = match attributes.semantics {
        HIRMoveSemantics::POD => HMIRMoveSemantics::POD,
        HIRMoveSemantics::Nocopy => HMIRMoveSemantics::Nocopy,
        HIRMoveSemantics::Nodrop => HMIRMoveSemantics::Nodrop,
    };
    let ty = context.declare_type(
        name.map(ToString::to_string),
        kind,
        semantics,
        attributes.unsafe_move,
        span,
    );

    Ok(context.push(HMIRExprKind::Constant(HMIRConstant::Type(ty)), span))
}
