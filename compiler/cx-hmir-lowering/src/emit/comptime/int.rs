use cx_hmir::ty::intrinsic::{HMIRFloatWidth, HMIRIntType, HMIRIntWidth};
use cx_intrinsics::{IntBinaryOp, IntUnaryOp};

use crate::emit::comptime::ScalarFault;

pub fn normalize(value: i128, ty: HMIRIntType) -> i128 {
    extend(value, bits(ty), signed(ty))
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

// The low 'bits' of a value, extended as signed or unsigned
pub fn extend(value: i128, bits: u32, signed: bool) -> i128 {
    if bits == 128 {
        return value;
    }
    let pattern = value as u128 & ((1u128 << bits) - 1);
    match signed {
        true => ((pattern << (128 - bits)) as i128) >> (128 - bits),
        false => pattern as i128,
    }
}

// Operands are read at the width of 'ty' with the signedness the operation names; the result is not normalized
pub fn binary(op: IntBinaryOp, lhs: i128, rhs: i128, ty: HMIRIntType) -> Result<i128, ScalarFault> {
    let bits = bits(ty);
    let (sl, sr) = (extend(lhs, bits, true), extend(rhs, bits, true));
    let (ul, ur) = (extend(lhs, bits, false) as u128, extend(rhs, bits, false) as u128);

    Ok(match op {
        IntBinaryOp::Add => lhs.wrapping_add(rhs),
        IntBinaryOp::Sub => lhs.wrapping_sub(rhs),
        IntBinaryOp::UMul | IntBinaryOp::SMul => lhs.wrapping_mul(rhs),
        IntBinaryOp::UDiv | IntBinaryOp::UMod if ur == 0 => return Err(ScalarFault::ZeroDivisor),
        IntBinaryOp::SDiv | IntBinaryOp::SMod if sr == 0 => return Err(ScalarFault::ZeroDivisor),
        IntBinaryOp::SDiv | IntBinaryOp::SMod if sl == i128::MIN >> (128 - bits) && sr == -1 => {
            return Err(ScalarFault::Undefined);
        }
        IntBinaryOp::UDiv => (ul / ur) as i128,
        IntBinaryOp::UMod => (ul % ur) as i128,
        IntBinaryOp::SDiv => sl / sr,
        IntBinaryOp::SMod => sl % sr,

        IntBinaryOp::Eq => (ul == ur) as i128,
        IntBinaryOp::Neq => (ul != ur) as i128,
        IntBinaryOp::ULt => (ul < ur) as i128,
        IntBinaryOp::ULe => (ul <= ur) as i128,
        IntBinaryOp::UGt => (ul > ur) as i128,
        IntBinaryOp::UGe => (ul >= ur) as i128,
        IntBinaryOp::SLt => (sl < sr) as i128,
        IntBinaryOp::SLe => (sl <= sr) as i128,
        IntBinaryOp::SGt => (sl > sr) as i128,
        IntBinaryOp::SGe => (sl >= sr) as i128,

        IntBinaryOp::LAnd => (ul != 0 && ur != 0) as i128,
        IntBinaryOp::LOr => (ul != 0 || ur != 0) as i128,
        IntBinaryOp::BAnd => lhs & rhs,
        IntBinaryOp::BOr => lhs | rhs,
        IntBinaryOp::BXor => lhs ^ rhs,

        IntBinaryOp::LShift | IntBinaryOp::ARShift | IntBinaryOp::LRShift => {
            let Some(count) = u32::try_from(rhs).ok().filter(|count| *count < bits) else {
                return Err(ScalarFault::Undefined);
            };
            match op {
                IntBinaryOp::LShift => lhs << count,
                IntBinaryOp::ARShift => sl >> count,
                _ => (ul >> count) as i128,
            }
        }
    })
}

pub fn unary(op: IntUnaryOp, value: i128, ty: HMIRIntType) -> i128 {
    let value = normalize(value, ty);

    match op {
        IntUnaryOp::Neg => value.wrapping_neg(),
        IntUnaryOp::BNot => !value,
        IntUnaryOp::LNot => (value == 0) as i128,
    }
}

pub fn to_float(value: i128, ty: HMIRIntType, signed: bool, width: HMIRFloatWidth) -> f64 {
    let value = extend(value, bits(ty), signed);

    match (signed, width) {
        (true, HMIRFloatWidth::F32) => value as f32 as f64,
        (true, HMIRFloatWidth::F64) => value as f64,
        (false, HMIRFloatWidth::F32) => value as u128 as f32 as f64,
        (false, HMIRFloatWidth::F64) => value as u128 as f64,
    }
}
