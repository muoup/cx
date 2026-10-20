use cx_hmir::{
    HMIRBody,
    expr::{HMIRExprID, op::HMIRTypeOp},
    ty::{
        HMIRExprType, HMIRExprTypeKind, HMIRType, HMIRTypeID, HMIRTypeKind,
        aggregate::HMIRTypeSemantics, context::HMIRTypeContext,
    },
};
use cx_tokens::TokenRange;

use crate::{
    env::function::LocalLoweringContext,
    eval::{EvalResult, eval_comptime, eval_type},
    value::constant::HMIRConstant,
};

pub(crate) fn eval_type_op(
    _context: &mut LocalLoweringContext,
    _body: &HMIRBody,
    _span: &TokenRange,
    op: &HMIRTypeOp,
) -> EvalResult<HMIRTypeID> {
    todo!("HMIRTypeOp: {}", op.path())
}

pub(crate) fn eval_declared_type(
    context: &mut LocalLoweringContext,
    body: &HMIRBody,
    declaration: HMIRExprID,
    ty: &HMIRExprType,
) -> EvalResult<HMIRTypeID> {
    match ty.kind() {
        HMIRExprTypeKind::Nominal(inner) => {
            let inner = eval_type(context, body, *inner)?;
            Ok(specify(context, inner, *ty.semantics(), ty.decl_span()))
        }
        HMIRExprTypeKind::Struct { .. }
        | HMIRExprTypeKind::Union { .. }
        | HMIRExprTypeKind::TaggedUnion { .. } => eval_aggregate(context, body, declaration, ty),
        _ => {
            let ty = evaluate(context, body, ty)?;
            Ok(context.env_mut().resolver_mut().realize_type(ty))
        }
    }
}

// The fields of an aggregate may refer back to it, which finds it incomplete until they are all evaluated
fn eval_aggregate(
    context: &mut LocalLoweringContext,
    body: &HMIRBody,
    declaration: HMIRExprID,
    ty: &HMIRExprType,
) -> EvalResult<HMIRTypeID> {
    if let Some(id) = context.env().resolver().declared_type(declaration) {
        return Ok(id);
    }

    let incomplete = HMIRType::new(
        HMIRTypeKind::Opaque {
            size: 0,
            alignment: 1,
        },
        *ty.semantics(),
        ty.decl_span().clone(),
        ty.debug_name().map(str::to_string),
    );
    let id = context
        .env_mut()
        .resolver_mut()
        .declare_type(declaration, incomplete);
    let complete = evaluate(context, body, ty)?;
    context.env_mut().resolver_mut().complete_type(id, complete);

    Ok(id)
}

fn evaluate(
    context: &mut LocalLoweringContext,
    body: &HMIRBody,
    ty: &HMIRExprType,
) -> EvalResult<HMIRType> {
    ty.map(
        context,
        |context, inner| eval_type(context, body, *inner),
        |context, length| array_length(context, body, *length),
    )
}

// Specifiers only accumulate, so a type that already carries them stands for itself
pub(crate) fn specify(
    context: &mut LocalLoweringContext,
    ty: HMIRTypeID,
    specifiers: HMIRTypeSemantics,
    span: &TokenRange,
) -> HMIRTypeID {
    let env = context.env_mut();
    let current = env.semantics(ty);
    let semantics = current.apply(specifiers);
    if semantics == current {
        return ty;
    }

    let kind = HMIRTypeKind::Nominal(env.underlying(ty));
    env.resolver_mut().realize_type(HMIRType::new(
        kind,
        semantics,
        span.clone(),
        None
    ))
}

fn array_length(
    context: &mut LocalLoweringContext,
    body: &HMIRBody,
    length: HMIRExprID,
) -> EvalResult<u64> {
    match eval_comptime(context, body, length)? {
        HMIRConstant::Int { value, .. } if value >= 0 => Ok(value as u64),
        _ => todo!("Error: array length is not a non-negative integer constant"),
    }
}
