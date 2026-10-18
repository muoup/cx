// Mirrors the coercion model of the legacy typechecker: a coercion is a chain of rules, each of which either
// applies one step or leaves the value untouched, applied until the value has the type asked for.

pub(crate) mod arithmetic;
mod compatible;
mod explicit;
mod implicit;
mod pending;
mod promotion;
mod truthy;

pub(crate) use promotion::{decay, operand, rvalue};
pub(crate) use truthy::truthy;

use cx_hmir::{
    HMIRBody, HMIRCoerceMode,
    expr::HMIRExprID,
    ty::{HMIRTypeID, context::HMIRTypeContext},
    value::HMIRValue,
};
use cx_log::{CXResult, catalogue::typecheck};
use cx_tokens::TokenRange;

use crate::{
    env::{HMIREnvironment, function::LocalLoweringContext},
    eval::{EvalResult, aggregate::eval_initializer, eval, eval_type, types},
    log::hmir_error,
};

use self::pending::Coercion;

type ConversionRule = fn(&HMIREnvironment, &HMIRValue, HMIRTypeID) -> Coercion;

pub(crate) fn eval_coerce(
    context: &mut LocalLoweringContext,
    body: &HMIRBody,
    span: &TokenRange,
    mode: HMIRCoerceMode,
    value: HMIRExprID,
    target: HMIRExprID,
) -> EvalResult<HMIRValue> {
    if mode == HMIRCoerceMode::Truthy {
        let value = eval(context, body, value)?;
        return Ok(truthy(context, value, span)?);
    }

    let target = eval_type(context, body, target)?;
    let value = eval_initializer(context, body, value, target)?;
    match mode {
        HMIRCoerceMode::Implicit => Ok(implicit(context, value, target, span)?),
        _ => Ok(explicit(context, value, target, span)?),
    }
}

pub(crate) fn implicit(
    context: &mut LocalLoweringContext,
    value: HMIRValue,
    target: HMIRTypeID,
    span: &TokenRange,
) -> CXResult<HMIRValue> {
    coerce(context, value, target, span, implicit::rule)
}

// Coerces an expression to a type it is expected to have, which an initializer list without a type of its own takes
pub(crate) fn implicit_expr(
    context: &mut LocalLoweringContext,
    body: &HMIRBody,
    value: HMIRExprID,
    target: HMIRTypeID,
) -> EvalResult<HMIRValue> {
    let span = body.expr(value).span();
    let value = eval_initializer(context, body, value, target)?;

    Ok(implicit(context, value, target, span)?)
}

pub(crate) fn explicit(
    context: &mut LocalLoweringContext,
    value: HMIRValue,
    target: HMIRTypeID,
    span: &TokenRange,
) -> CXResult<HMIRValue> {
    coerce(context, value, target, span, explicit::rule)
}

// The value a reference refers to, or the value itself
pub(crate) fn value(
    context: &mut LocalLoweringContext,
    value: HMIRValue,
    span: &TokenRange,
) -> CXResult<HMIRValue> {
    match context.env().reference_inner(value.ty()) {
        Some(inner) => implicit(context, value, inner, span),
        None => Ok(value),
    }
}

fn coerce(
    context: &mut LocalLoweringContext,
    mut value: HMIRValue,
    target: HMIRTypeID,
    span: &TokenRange,
    rule: ConversionRule,
) -> CXResult<HMIRValue> {
    let source = value.ty();

    while !complete(context.env(), value.ty(), target) {
        match rule(context.env(), &value, target) {
            Coercion::Pending(pending) => value = pending.invoke(context, value, span)?,
            Coercion::Unapplied(_) => {
                return Err(hmir_error(
                    span,
                    &typecheck::INVALID_CONVERSION,
                    (
                        types::name(context.env(), source),
                        types::name(context.env(), target),
                    ),
                ));
            }
        }
    }

    // The value is the one to say how long an array without a length is
    match types::unsized_array(context.env(), target)
        && !types::unsized_array(context.env(), value.ty())
    {
        true => Ok(value),
        false => Ok(value.with_ty(target)),
    }
}

// An array is only ever named by another type for it, which may leave out its length
fn complete(env: &HMIREnvironment, ty: HMIRTypeID, target: HMIRTypeID) -> bool {
    env.same_type(ty, target) || env.is_array(target) && compatible::compatible(env, ty, target)
}
