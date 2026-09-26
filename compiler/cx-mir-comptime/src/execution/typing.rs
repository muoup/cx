use cx_log::{CXResult, catalogue::mir};
use cx_mir::{
    MIRComptimeBody, MIRFloatType, MIRIntType, MIRTarget, MIRTypeID, MIRTypeKind,
    ty::interface::MTRegistry,
};
use cx_tokens::TokenRange;

use crate::log::comptime_error;

pub(super) fn target_type(
    body: &MIRComptimeBody<'_>,
    target: MIRTarget,
    range: &TokenRange,
) -> CXResult<MIRTypeID> {
    let ty = match target {
        MIRTarget::Place(id) => body.place(id).map(|place| place.ty),
        MIRTarget::Register(id) => body.register(id).map(|register| register.ty),
        MIRTarget::Global(reference) => Some(reference.ty),
        MIRTarget::Indirect(_) => None,
    };
    match ty {
        Some(ty) => Ok(ty),
        None => comptime_error(
            range.clone(),
            (
                &mir::COMPTIME_INVALID_OPERATION,
                "unknown intrinsic output type".into(),
            ),
        ),
    }
}

pub(super) fn integer_type<R: MTRegistry>(
    registry: &R,
    body: &MIRComptimeBody<'_>,
    target: MIRTarget,
    range: &TokenRange,
) -> CXResult<MIRIntType> {
    let ty = target_type(body, target, range)?;
    match registry.definition(ty).map(|definition| definition.kind()) {
        Some(MIRTypeKind::Integer { ty, .. }) => Ok(*ty),
        _ => comptime_error(
            range.clone(),
            (
                &mir::COMPTIME_INVALID_OPERATION,
                "integer result has a non-integer type".into(),
            ),
        ),
    }
}

pub(super) fn float_type<R: MTRegistry>(
    registry: &R,
    body: &MIRComptimeBody<'_>,
    target: MIRTarget,
    range: &TokenRange,
) -> CXResult<MIRFloatType> {
    let ty = target_type(body, target, range)?;
    match registry.definition(ty).map(|definition| definition.kind()) {
        Some(MIRTypeKind::Float { ty }) => Ok(*ty),
        _ => comptime_error(
            range.clone(),
            (
                &mir::COMPTIME_INVALID_OPERATION,
                "float result has a non-float type".into(),
            ),
        ),
    }
}
