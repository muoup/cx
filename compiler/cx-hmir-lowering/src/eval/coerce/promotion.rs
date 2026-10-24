pub mod arithmetic;
mod array_to_ptr;
mod fn_to_ptr;
mod lvalue;
mod str_to_char_ptr;

use cx_hmir::ty::{HMIRTypeID, context::HMIRTypeContext};
use cx_log::CXResult;
use cx_tokens::TokenRange;

use crate::{
    emit,
    env::function::LocalLoweringContext,
    eval::{coerce::result::Coercion, types},
    value::HMIRValue,
};

pub fn step(
    context: &mut LocalLoweringContext,
    value: HMIRValue,
    span: &TokenRange,
) -> CXResult<Coercion> {
    array_to_ptr::apply(context, value, span)?
        .or_else(|value| str_to_char_ptr::apply(context, value, span))?
        .or_else(|value| fn_to_ptr::apply(context, value, span))?
        .or_else(|value| lvalue::apply(context, value, span))
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

fn designated(context: &LocalLoweringContext, value: &HMIRValue) -> HMIRTypeID {
    context
        .env()
        .reference_inner(value.ty())
        .unwrap_or(value.ty())
}

fn address(
    context: &mut LocalLoweringContext,
    value: HMIRValue,
    pointee: HMIRTypeID,
    span: &TokenRange,
) -> CXResult<Coercion> {
    let storage = match context.env().reference_inner(value.ty()) {
        Some(inner) => HMIRValue::indirect(inner, value),
        None => value,
    };
    let pointer = types::pointer(context.env_mut(), pointee, span);

    Ok(Coercion::Applied(emit::address(
        context, &storage, pointer, span,
    )?))
}
