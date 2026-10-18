pub(crate) mod decay;
pub(crate) mod reference;

use cx_hmir::value::HMIRValue;
use cx_log::CXResult;
use cx_tokens::TokenRange;

use crate::{
    env::{HMIREnvironment, function::LocalLoweringContext},
    eval::coerce::{arithmetic, implicit, pending::Coercion},
};

type PromotionRule = fn(&HMIREnvironment, &HMIRValue) -> Coercion;

// The promotions any use of a value as an rvalue undergoes: references are followed to their storage, and
// arrays, strings and functions decay to pointers
pub(crate) fn rvalue_rule(env: &HMIREnvironment, value: &HMIRValue) -> Coercion {
    reference::try_referent(env, value).or_else(|| decay::try_decay(env, value))
}

pub(crate) fn rvalue(
    context: &mut LocalLoweringContext,
    value: HMIRValue,
    span: &TokenRange,
) -> CXResult<HMIRValue> {
    repeat(context, value, span, rvalue_rule)
}

// An operand of an arithmetic operator is also promoted to at least an 'int'
pub(crate) fn operand(
    context: &mut LocalLoweringContext,
    value: HMIRValue,
    span: &TokenRange,
) -> CXResult<HMIRValue> {
    let value = rvalue(context, value, span)?;
    let promoted = arithmetic::integer(context, value.ty(), span);

    implicit(context, value, promoted, span)
}

// Decays a single value without following references, as taking an address does
pub(crate) fn decay(
    context: &mut LocalLoweringContext,
    value: HMIRValue,
    span: &TokenRange,
) -> CXResult<HMIRValue> {
    match decay::try_decay(context.env(), &value) {
        Coercion::Pending(pending) => pending.invoke(context, value, span),
        Coercion::Unapplied(_) => Ok(value),
    }
}

// Applies a promotion until it no longer does anything. A promotion that meets an obstacle simply stops here;
// whatever needs the value to be promoted further reports the error.
fn repeat(
    context: &mut LocalLoweringContext,
    mut value: HMIRValue,
    span: &TokenRange,
    rule: PromotionRule,
) -> CXResult<HMIRValue> {
    loop {
        match rule(context.env(), &value) {
            Coercion::Pending(pending) => value = pending.invoke(context, value, span)?,
            Coercion::Unapplied(_) => return Ok(value),
        }
    }
}
