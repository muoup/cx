use cx_hmir::ty::context::HMIRTypeContext;
use cx_log::CXResult;
use cx_tokens::TokenRange;

use crate::{
    env::function::LocalLoweringContext,
    eval::{
        coerce::{
            promotion::{address, designated},
            result::Coercion,
        },
        storage, types,
    },
    value::HMIRValue,
};

pub fn apply(
    context: &mut LocalLoweringContext,
    value: HMIRValue,
    span: &TokenRange,
) -> CXResult<Coercion> {
    let subject = designated(context, &value);
    let Some(element) = types::element(context, subject, span) else {
        return Ok(Coercion::Unapplied(value));
    };

    let value = match context.env().is_reference(value.ty()) {
        true => value,
        false => storage::temporary(context, value, span)?,
    };

    address(context, value, element, span)
}
