use std::cmp::Ordering;

use cx_hmir::{
    HMIRBinaryOp, HMIRUnaryOp,
    ty::intrinsic::{HMIRFloatWidth, HMIRIntType, HMIRIntWidth},
};

use crate::value::scalar::ScalarFault;

pub fn normalize(value: i128, ty: HMIRIntType) -> i128 {
    let bits = bits(ty);
    if bits == 128 {
        return value;
    }

    let pattern = value as u128 & ((1u128 << bits) - 1);
    match signed(ty) {
        true => ((pattern << (128 - bits)) as i128) >> (128 - bits),
        false => pattern as i128,
    }
}

pub fn bits(ty: HMIRIntType) -> u32 {
    match ty.width() {
        HMIRIntWidth::I1 => 1,
        width => width.bytes() as u32 * 8,
    }
}

pub fn signed(ty: HMIRIntType) -> bool {
    ty.signed() && ty.width() != HMIRIntWidth::I1
}

fn ordering(lhs: i128, rhs: i128, ty: HMIRIntType) -> Ordering {
    match signed(ty) {
        true => lhs.cmp(&rhs),
        false => (lhs as u128).cmp(&(rhs as u128)),
    }
}

pub fn compare(op: HMIRBinaryOp, lhs: i128, rhs: i128, ty: HMIRIntType) -> Option<bool> {
    let ordering = ordering(normalize(lhs, ty), normalize(rhs, ty), ty);

    Some(match op {
        HMIRBinaryOp::Eq => ordering.is_eq(),
        HMIRBinaryOp::Neq => ordering.is_ne(),
        HMIRBinaryOp::Lt => ordering.is_lt(),
        HMIRBinaryOp::Le => ordering.is_le(),
        HMIRBinaryOp::Gt => ordering.is_gt(),
        HMIRBinaryOp::Ge => ordering.is_ge(),
        _ => return None,
    })
}

pub fn arithmetic(
    op: HMIRBinaryOp,
    lhs: i128,
    rhs: i128,
    ty: HMIRIntType,
) -> Result<i128, ScalarFault> {
    let (lhs, rhs) = (normalize(lhs, ty), normalize(rhs, ty));
    let value = match op {
        HMIRBinaryOp::Add => lhs.wrapping_add(rhs),
        HMIRBinaryOp::Sub => lhs.wrapping_sub(rhs),
        HMIRBinaryOp::Mul => lhs.wrapping_mul(rhs),
        HMIRBinaryOp::Div | HMIRBinaryOp::Mod => divide(op, lhs, rhs, ty)?,
        HMIRBinaryOp::BAnd => lhs & rhs,
        HMIRBinaryOp::BOr => lhs | rhs,
        HMIRBinaryOp::BXor => lhs ^ rhs,
        HMIRBinaryOp::LShift | HMIRBinaryOp::RShift => shift(op, lhs, rhs, ty)?,
        _ => unreachable!("'{}' has no integer arithmetic form", op.path()),
    };

    Ok(normalize(value, ty))
}

fn divide(op: HMIRBinaryOp, lhs: i128, rhs: i128, ty: HMIRIntType) -> Result<i128, ScalarFault> {
    if rhs == 0 {
        return Err(ScalarFault::ZeroDivisor);
    }

    if !signed(ty) {
        let (lhs, rhs) = (lhs as u128, rhs as u128);
        return Ok(match op {
            HMIRBinaryOp::Div => lhs / rhs,
            _ => lhs % rhs,
        } as i128);
    }

    let min = i128::MIN >> (128 - bits(ty));
    if lhs == min && rhs == -1 {
        return Err(ScalarFault::Undefined);
    }

    Ok(match op {
        HMIRBinaryOp::Div => lhs / rhs,
        _ => lhs % rhs,
    })
}

fn shift(op: HMIRBinaryOp, lhs: i128, rhs: i128, ty: HMIRIntType) -> Result<i128, ScalarFault> {
    let count = match signed(ty) {
        true => u128::try_from(rhs).ok(),
        false => Some(rhs as u128),
    };
    let Some(count) = count.filter(|count| *count < bits(ty) as u128) else {
        return Err(ScalarFault::Undefined);
    };
    let count = count as u32;

    Ok(match op {
        HMIRBinaryOp::LShift => lhs << count,
        _ if signed(ty) => lhs >> count,
        _ => ((lhs as u128) >> count) as i128,
    })
}

pub fn unary(op: HMIRUnaryOp, value: i128, ty: HMIRIntType) -> i128 {
    let value = normalize(value, ty);

    normalize(
        match op {
            HMIRUnaryOp::Neg => value.wrapping_neg(),
            HMIRUnaryOp::BNot => !value,
            HMIRUnaryOp::LNot => (value == 0) as i128,
            _ => unreachable!("'{}' has no integer form", op.path()),
        },
        ty,
    )
}

pub fn to_float(value: i128, ty: HMIRIntType, width: HMIRFloatWidth) -> f64 {
    let value = normalize(value, ty);

    match (signed(ty), width) {
        (true, HMIRFloatWidth::F32) => value as f32 as f64,
        (true, HMIRFloatWidth::F64) => value as f64,
        (false, HMIRFloatWidth::F32) => value as u128 as f32 as f64,
        (false, HMIRFloatWidth::F64) => value as u128 as f64,
    }
}
