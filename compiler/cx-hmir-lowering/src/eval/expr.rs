pub(crate) mod aggregate;
pub(crate) mod binding;
pub(crate) mod call;
pub(crate) mod coerce;
pub(crate) mod comptime;
pub(crate) mod control;
pub(crate) mod name;
pub(crate) mod op;
pub(crate) mod place;
pub(crate) mod staged;
pub(crate) mod types;

use cx_hmir::{
    HMIRBody,
    expr::{HMIRExprID, HMIRIntrinsic, literal::HMIRLiteral},
    ty::{HMIRTypeKind, context::HMIRTypeContext},
};
use cx_intrinsics::{Intrinsic, VAIntrinsic};
use cx_log::{CXResult, catalogue::typecheck};
use cx_tokens::TokenRange;

use crate::{
    emit,
    env::function::{LocalLoweringContext, Permission},
    eval::{EvalResult, eval, eval_type, flow::Evaluation},
    log::hmir_error,
    symbol::composite::alike,
    value::{HMIRValue, constant::HMIRConstant},
};

pub(crate) fn eval_literal(
    context: &mut LocalLoweringContext,
    span: &TokenRange,
    literal: &HMIRLiteral,
) -> HMIRValue {
    match literal {
        HMIRLiteral::Unit => unit(context, span),
        HMIRLiteral::Str(value) => {
            let ty = types::realize(context.env_mut(), HMIRTypeKind::Str, span);
            HMIRValue::constant(ty, HMIRConstant::Str(value.clone()))
        }
        HMIRLiteral::Int { value, ty } => {
            let ty = types::int(context.env_mut(), ty.width(), ty.signed(), span);
            HMIRValue::constant(ty, HMIRConstant::Int(*value))
        }
        HMIRLiteral::Float { value, ty } => {
            let ty = types::realize(context.env_mut(), HMIRTypeKind::Float(*ty), span);
            HMIRValue::constant(ty, HMIRConstant::Float(*value))
        }
    }
}

pub(crate) fn unit(context: &mut LocalLoweringContext, span: &TokenRange) -> HMIRValue {
    let ty = types::realize(context.env_mut(), HMIRTypeKind::Void, span);
    HMIRValue::constant(ty, HMIRConstant::Unit)
}

pub(crate) fn eval_intrinsic(
    context: &mut LocalLoweringContext,
    body: &HMIRBody,
    span: &TokenRange,
    intrinsic: &HMIRIntrinsic,
) -> EvalResult<Evaluation<HMIRValue>> {
    let Intrinsic::VA(intrinsic) = intrinsic else {
        unreachable!("only variadic intrinsics are lowered to HMIR")
    };
    let intrinsic = match intrinsic {
        VAIntrinsic::Start { list, last } => {
            let list = (va_list(context, body, *list))?.ready()?;
            let last = (eval(context, body, *last))?.ready()?;
            let last = coerce::std_rvalue(context, last, span)?;
            VAIntrinsic::Start { list, last }
        }
        VAIntrinsic::End { list } => VAIntrinsic::End {
            list: (va_list(context, body, *list))?.ready()?,
        },
        VAIntrinsic::Arg { list, ty } => VAIntrinsic::Arg {
            list: (va_list(context, body, *list))?.ready()?,
            ty: (eval_type(context, body, *ty))?.ready()?,
        },
    };
    let value = match emit::variadic(context, &intrinsic, span)? {
        Some(value) => value,
        None => unit(context, span),
    };
    Ok(Evaluation::Ready(value))
}

fn va_list(
    context: &mut LocalLoweringContext,
    body: &HMIRBody,
    list: HMIRExprID,
) -> EvalResult<Evaluation<HMIRValue>> {
    let span = body.expr(list).span();
    let value = (eval(context, body, list))?.ready()?;
    let expected = name::intrinsic_type(context, "__builtin_va_list", span)
        .expect("va_list is an intrinsic type");
    let actual = context.env().reference_inner(value.ty());
    if !actual.is_some_and(|actual| alike(context.env(), actual, expected, false)) {
        let mismatch = (
            "va_list expression".into(),
            types::name(context.env(), expected),
            types::name(context.env(), value.ty()),
        );
        return Err(hmir_error(span, &typecheck::TYPE_MISMATCH, mismatch).into());
    }
    let ty = types::pointer(context.env_mut(), expected, span);
    Ok(Evaluation::Ready(emit::address(context, &value, ty, span)?))
}

pub(crate) fn require_nonsafe(
    context: &LocalLoweringContext,
    span: &TokenRange,
    operation: &str,
) -> CXResult<()> {
    if !context.is_unevaluated() && context.permission() < Permission::Nonsafe {
        return Err(hmir_error(span, &typecheck::UNSAFE_OPERATION, operation.into()).into());
    }
    Ok(())
}

pub(crate) fn require_pure(
    context: &LocalLoweringContext,
    span: &TokenRange,
    operation: &str,
) -> CXResult<()> {
    if !context.is_unevaluated() && !context.expectations().allows_impure() {
        return Err(hmir_error(span, &typecheck::IMPURE_OPERATION, operation.into()).into());
    }
    Ok(())
}
