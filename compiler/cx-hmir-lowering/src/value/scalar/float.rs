use cx_hmir::{
    HMIRBinaryOp,
    ty::intrinsic::{HMIRFloatWidth, HMIRIntType},
};

use crate::value::scalar::{ScalarFault, int};

pub fn round(value: f64, width: HMIRFloatWidth) -> f64 {
    match width {
        HMIRFloatWidth::F32 => value as f32 as f64,
        HMIRFloatWidth::F64 => value,
    }
}

pub fn compare(op: HMIRBinaryOp, lhs: f64, rhs: f64) -> Option<bool> {
    Some(match op {
        HMIRBinaryOp::Eq => lhs == rhs,
        HMIRBinaryOp::Neq => lhs != rhs,
        HMIRBinaryOp::Lt => lhs < rhs,
        HMIRBinaryOp::Le => lhs <= rhs,
        HMIRBinaryOp::Gt => lhs > rhs,
        HMIRBinaryOp::Ge => lhs >= rhs,
        _ => return None,
    })
}

pub fn arithmetic(op: HMIRBinaryOp, lhs: f64, rhs: f64, width: HMIRFloatWidth) -> f64 {
    match width {
        HMIRFloatWidth::F32 => {
            let (lhs, rhs) = (lhs as f32, rhs as f32);
            (match op {
                HMIRBinaryOp::Add => lhs + rhs,
                HMIRBinaryOp::Sub => lhs - rhs,
                HMIRBinaryOp::Mul => lhs * rhs,
                HMIRBinaryOp::Div => lhs / rhs,
                _ => unreachable!("'{}' has no floating arithmetic form", op.path()),
            }) as f64
        }
        HMIRFloatWidth::F64 => match op {
            HMIRBinaryOp::Add => lhs + rhs,
            HMIRBinaryOp::Sub => lhs - rhs,
            HMIRBinaryOp::Mul => lhs * rhs,
            HMIRBinaryOp::Div => lhs / rhs,
            _ => unreachable!("'{}' has no floating arithmetic form", op.path()),
        },
    }
}

pub fn to_int(value: f64, ty: HMIRIntType) -> Result<i128, ScalarFault> {
    let truncated = value.trunc();
    let bits = int::bits(ty) as i32;
    let (low, high) = match int::signed(ty) {
        true => (-(2f64.powi(bits - 1)), 2f64.powi(bits - 1)),
        false => (0.0, 2f64.powi(bits)),
    };
    if truncated.is_nan() || truncated < low || truncated >= high {
        return Err(ScalarFault::Undefined);
    }

    Ok(match int::signed(ty) {
        true => truncated as i128,
        false => truncated as u128 as i128,
    })
}
