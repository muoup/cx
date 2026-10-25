use cx_hmir::ty::{HMIRTypeID, HMIRTypeKind, context::HMIRTypeContext, intrinsic::HMIRIntWidth};
use cx_log::{CXResult, catalogue::typecheck};
use cx_tokens::TokenRange;

use crate::{
    env::function::LocalLoweringContext,
    eval::{coerce::result::Coercion, types},
    log::hmir_error,
    value::{HMIRValue, HMIRValueKind, constant::HMIRConstant},
};

pub fn array(
    context: &mut LocalLoweringContext,
    value: HMIRValue,
    target: HMIRTypeID,
    span: &TokenRange,
) -> CXResult<Coercion> {
    let HMIRValueKind::Constant(HMIRConstant::Str(string)) = value.kind() else {
        return Ok(Coercion::Unapplied(value));
    };
    let HMIRTypeKind::Array { element, length } = *context.env().resolve_kind(target) else {
        unreachable!("a string array initializer has an array target")
    };
    if !matches!(context.env().resolve_kind(element), HMIRTypeKind::Int(int) if int.width() == HMIRIntWidth::I8)
    {
        return Ok(Coercion::Unapplied(value));
    }
    let length = length.unwrap_or(string.len() as u64 + 1);
    if string.len() as u64 > length {
        let limit = ("string".into(), usize::try_from(length).ok());
        return Err(hmir_error(span, &typecheck::INITIALIZER_LIMIT, limit));
    }
    let mut items = vec![HMIRConstant::Int(0); length as usize];
    for (item, byte) in items.iter_mut().zip(string.bytes()) {
        *item = HMIRConstant::Int(byte as i128);
    }
    let ty = match types::unsized_array(context.env(), target) {
        true => types::array(context.env_mut(), target, length, span),
        false => target,
    };

    Ok(Coercion::Applied(HMIRValue::constant(
        ty,
        HMIRConstant::Aggregate(items),
    )))
}
