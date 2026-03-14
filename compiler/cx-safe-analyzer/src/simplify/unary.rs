use cx_safe_ir::intrinsic::{FMIRCastIntrinsic, FMIRUnaryIntrinsic};

use super::{ConstValue, int_to_bool};

pub fn eval_unary(op: &FMIRUnaryIntrinsic, arg: ConstValue) -> Option<ConstValue> {
    use FMIRUnaryIntrinsic as Op;
    
    match (op, arg) {
        (Op::NEG,  ConstValue::Int(v)) | (Op::INEG, ConstValue::Int(v)) => Some(ConstValue::Int(-v)),
        (Op::FNEG, ConstValue::Float(v)) => Some(ConstValue::Float(-v)),
        (Op::BNOT, ConstValue::Int(v))   => Some(ConstValue::Int(!v)),
        (Op::LNOT, ConstValue::Bool(v))  => Some(ConstValue::Bool(!v)),
        (Op::LNOT, ConstValue::Int(v))   => Some(ConstValue::Bool(!int_to_bool(v))),
        _ => None,
    }
}

pub fn eval_cast(op: &FMIRCastIntrinsic, value: ConstValue) -> Option<ConstValue> {
    use FMIRCastIntrinsic as Op;
    
    match (op, value) {
        (Op::IntToBool, ConstValue::Bool(v)) => Some(ConstValue::Bool(v)),
        (Op::IntToBool, ConstValue::Int(v))  => Some(ConstValue::Bool(int_to_bool(v))),
        (Op::Integral { sextend, to_bits }, ConstValue::Int(v)) => {
            // Bit-width narrowing is not tracked in ConstValue; preserve value with sign handling.
            let result = if *sextend { v } else { v & ((1u64 << to_bits) - 1) as i64 };
            Some(ConstValue::Int(result))
        }
        (Op::FloatCast { .. }, ConstValue::Float(v)) => Some(ConstValue::Float(v)),
        (Op::IntToFloat { sextend, .. }, ConstValue::Int(v)) => {
            let int_val = if *sextend { v } else { v & i64::MAX };
            Some(ConstValue::Float(int_val as f64))
        }
        (Op::FloatToInt { sextend, .. }, ConstValue::Float(v)) => {
            let int_val = v as i64;
            let result = if *sextend { int_val } else { int_val & i64::MAX };
            Some(ConstValue::Int(result))
        }
        _ => None,
    }
}
