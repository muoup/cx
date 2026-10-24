use cx_hmir::ty::{HMIRTypeKind, context::HMIRTypeContext, intrinsic::HMIRIntWidth};
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
    if !matches!(context.env().resolve_kind(subject), HMIRTypeKind::Str) {
        return Ok(Coercion::Unapplied(value));
    }

    let char = types::int(context.env_mut(), HMIRIntWidth::I8, false, span);
    address(context, value, char, span)
}
