use cx_hmir::ty::{HMIRTypeKind, context::HMIRTypeContext};
use cx_log::CXResult;
use cx_tokens::TokenRange;

use crate::{
    env::function::LocalLoweringContext,
    eval::{
        coerce::{
            promotion::{address, designated},
            result::Coercion,
        },
        types,
    },
    value::HMIRValue,
};

pub fn apply(
    context: &mut LocalLoweringContext,
    value: HMIRValue,
    span: &TokenRange,
) -> CXResult<Coercion> {
    let subject = designated(context, &value);
    let function = matches!(context.env().resolve_kind(subject), HMIRTypeKind::Function(_));
    if !function || types::comptime_only(context.env(), subject) {
        return Ok(Coercion::Unapplied(value));
    }

    address(context, value, subject, span)
}
