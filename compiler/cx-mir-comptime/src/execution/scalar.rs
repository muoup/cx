use cx_log::{CXResult, catalogue::mir};
use cx_mir::{MIRConstant, MIRFloatType, MIRIntType};
use cx_tokens::TokenRange;

use crate::log::comptime_error;

pub(super) fn truthy(value: &MIRConstant) -> bool {
    match value {
        MIRConstant::Integer { value, .. } => *value != 0,
        MIRConstant::Float { value, .. } => f64::from(value) != 0.0,
        MIRConstant::Nullptr { .. } | MIRConstant::Undefined => false,
        _ => true,
    }
}

pub(super) fn bits(ty: MIRIntType) -> u32 {
    match ty {
        MIRIntType::I1 => 1,
        _ => (ty.bytes() * 8) as u32,
    }
}

pub(super) fn mask(value: u128, width: u32) -> u128 {
    if width == 128 {
        value
    } else {
        value & ((1u128 << width) - 1)
    }
}

pub(super) fn signed(value: u128, width: u32) -> i128 {
    if width == 128 {
        value as i128
    } else {
        let shift = 128 - width;
        ((value << shift) as i128) >> shift
    }
}

pub(super) fn integer(value: &MIRConstant, range: &TokenRange) -> CXResult<(u128, MIRIntType)> {
    match value {
        MIRConstant::Integer { value, ty } => Ok((mask(*value as u128, bits(*ty)), *ty)),
        _ => comptime_error(
            range.clone(),
            (
                &mir::COMPTIME_INVALID_OPERATION,
                "non-integer arithmetic operand".into(),
            ),
        ),
    }
}

pub(super) fn integer_value(value: MIRConstant, range: &TokenRange) -> CXResult<i128> {
    match value {
        MIRConstant::Integer { value, .. } => Ok(value),
        _ => comptime_error(
            range.clone(),
            (
                &mir::COMPTIME_INVALID_OPERATION,
                "non-integer operand".into(),
            ),
        ),
    }
}

pub(super) fn float_value(value: MIRConstant, range: &TokenRange) -> CXResult<f64> {
    match value {
        MIRConstant::Float { value, .. } => Ok(f64::from(&value)),
        _ => comptime_error(
            range.clone(),
            (&mir::COMPTIME_INVALID_OPERATION, "non-float operand".into()),
        ),
    }
}

pub(super) fn int_const(value: u128, ty: MIRIntType) -> MIRConstant {
    MIRConstant::Integer {
        value: mask(value, bits(ty)) as i128,
        ty,
    }
}

pub(super) fn bool_const(value: bool) -> MIRConstant {
    MIRConstant::Integer {
        value: value as i128,
        ty: MIRIntType::I1,
    }
}

pub(super) fn float_const(value: f64, ty: MIRFloatType) -> MIRConstant {
    let value = if ty == MIRFloatType::F32 {
        (value as f32) as f64
    } else {
        value
    };
    MIRConstant::Float {
        value: value.into(),
        ty,
    }
}

pub(super) fn mask_integer(value: i128, ty: MIRIntType) -> i128 {
    mask(value as u128, bits(ty)) as i128
}
