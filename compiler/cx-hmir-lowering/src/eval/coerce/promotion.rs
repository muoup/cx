pub mod arithmetic;
mod decay;
mod lvalue;

use cx_log::CXResult;
use cx_tokens::TokenRange;

use crate::{
    env::function::LocalLoweringContext, eval::coerce::result::Coercion, value::HMIRValue,
};

pub fn step(
    context: &mut LocalLoweringContext,
    value: HMIRValue,
    span: &TokenRange,
) -> CXResult<Coercion> {
    decay::apply(context, value, span)?.or_else(|value| lvalue::apply(context, value, span))
}

pub fn std_rvalue(
    context: &mut LocalLoweringContext,
    mut value: HMIRValue,
    span: &TokenRange,
) -> CXResult<HMIRValue> {
    loop {
        match step(context, value, span)? {
            Coercion::Applied(promoted) => value = promoted,
            Coercion::Unapplied(value) => return Ok(value),
        }
    }
}
