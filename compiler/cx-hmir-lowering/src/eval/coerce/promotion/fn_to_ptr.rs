use cx_hmir::ty::{HMIRTypeKind, context::HMIRTypeContext};
use cx_log::CXResult;
use cx_tokens::TokenRange;

use crate::{
    env::function::LocalLoweringContext,
    eval::coerce::{
        promotion::{address, designated},
        result::Coercion,
    },
    value::HMIRValue,
};

pub fn apply(
    context: &mut LocalLoweringContext,
    value: HMIRValue,
    span: &TokenRange,
) -> CXResult<Coercion> {
    let subject = designated(context, &value);
    if !matches!(
        context.env().resolve_kind(subject),
        HMIRTypeKind::Function(_)
    ) {
        return Ok(Coercion::Unapplied(value));
    }

    address(context, value, subject, span)
}
