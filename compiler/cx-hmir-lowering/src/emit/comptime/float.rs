use cx_hmir::ty::intrinsic::{HMIRFloatWidth, HMIRIntType};
use cx_intrinsics::FloatBinaryOp;

use crate::emit::comptime::{ScalarFault, int};

pub fn round(value: f64, width: HMIRFloatWidth) -> f64 {
    match width {
        HMIRFloatWidth::F32 => value as f32 as f64,
        HMIRFloatWidth::F64 => value,
    }
}

pub fn compare(op: FloatBinaryOp, lhs: f64, rhs: f64) -> Option<bool> {
    Some(match op {
        FloatBinaryOp::Eq => lhs == rhs,
        FloatBinaryOp::Neq => lhs != rhs,
        FloatBinaryOp::Lt => lhs < rhs,
        FloatBinaryOp::Le => lhs <= rhs,
        FloatBinaryOp::Gt => lhs > rhs,
        FloatBinaryOp::Geq => lhs >= rhs,
        _ => return None,
    })
}

pub fn arithmetic(op: FloatBinaryOp, lhs: f64, rhs: f64, width: HMIRFloatWidth) -> f64 {
    match width {
        HMIRFloatWidth::F32 => {
            let (lhs, rhs) = (lhs as f32, rhs as f32);
            (match op {
                FloatBinaryOp::Add => lhs + rhs,
                FloatBinaryOp::Sub => lhs - rhs,
                FloatBinaryOp::Mul => lhs * rhs,
                FloatBinaryOp::Div => lhs / rhs,
                _ => unreachable!("'{}' has no floating arithmetic form", op.path()),
            }) as f64
        }
        HMIRFloatWidth::F64 => match op {
            FloatBinaryOp::Add => lhs + rhs,
            FloatBinaryOp::Sub => lhs - rhs,
            FloatBinaryOp::Mul => lhs * rhs,
            FloatBinaryOp::Div => lhs / rhs,
            _ => unreachable!("'{}' has no floating arithmetic form", op.path()),
        },
    }
}

pub fn to_int(value: f64, ty: HMIRIntType, signed: bool) -> Result<i128, ScalarFault> {
    let truncated = value.trunc();
    let bits = int::bits(ty) as i32;
    let (low, high) = match signed {
        true => (-(2f64.powi(bits - 1)), 2f64.powi(bits - 1)),
        false => (0.0, 2f64.powi(bits)),
    };
    if truncated.is_nan() || truncated < low || truncated >= high {
        return Err(ScalarFault::Undefined);
    }

    Ok(match signed {
        true => truncated as i128,
        false => truncated as u128 as i128,
    })
}
