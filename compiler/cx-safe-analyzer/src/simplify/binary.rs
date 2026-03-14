use cx_safe_ir::intrinsic::{
    FMIRBinaryIntrinsic, FMIRIntrinsicFBinOp, FMIRIntrinsicIBinOp, FMIRIntrinsicPtrDiffBinop,
    FMIRPointerBinaryIntrinsicOp,
};

use super::{ConstValue, bool_to_int, int_to_bool};

pub fn eval_int_binop(op: &FMIRIntrinsicIBinOp, left: i64, right: i64) -> Option<ConstValue> {
    use FMIRIntrinsicIBinOp as Op;
    match op {
        Op::ADD => Some(ConstValue::Int(left + right)),
        Op::SUB => Some(ConstValue::Int(left - right)),
        Op::MUL | Op::IMUL => Some(ConstValue::Int(left * right)),
        Op::DIV | Op::IDIV => (right != 0).then(|| ConstValue::Int(left / right)),
        Op::MOD | Op::IMOD => (right != 0).then(|| ConstValue::Int(left % right)),
        Op::EQ => Some(ConstValue::Bool(left == right)),
        Op::NE => Some(ConstValue::Bool(left != right)),
        Op::ILT => Some(ConstValue::Bool(left < right)),
        Op::ILE => Some(ConstValue::Bool(left <= right)),
        Op::IGT => Some(ConstValue::Bool(left > right)),
        Op::IGE => Some(ConstValue::Bool(left >= right)),
        Op::LT => Some(ConstValue::Bool((left as u64) < (right as u64))),
        Op::LE => Some(ConstValue::Bool((left as u64) <= (right as u64))),
        Op::GT => Some(ConstValue::Bool((left as u64) > (right as u64))),
        Op::GE => Some(ConstValue::Bool((left as u64) >= (right as u64))),
        Op::LAND => Some(ConstValue::Bool(int_to_bool(left) && int_to_bool(right))),
        Op::LOR => Some(ConstValue::Bool(int_to_bool(left) || int_to_bool(right))),
        Op::BAND => Some(ConstValue::Int(left & right)),
        Op::BOR => Some(ConstValue::Int(left | right)),
        Op::BXOR => Some(ConstValue::Int(left ^ right)),
    }
}

pub fn eval_float_binop(op: &FMIRIntrinsicFBinOp, left: f64, right: f64) -> Option<ConstValue> {
    use FMIRIntrinsicFBinOp as Op;
    match op {
        Op::FADD => Some(ConstValue::Float(left + right)),
        Op::FSUB => Some(ConstValue::Float(left - right)),
        Op::FMUL => Some(ConstValue::Float(left * right)),
        Op::FDIV => Some(ConstValue::Float(left / right)),
        Op::FEQ => Some(ConstValue::Bool(left == right)),
        Op::FNE => Some(ConstValue::Bool(left != right)),
        Op::FLT => Some(ConstValue::Bool(left < right)),
        Op::FLE => Some(ConstValue::Bool(left <= right)),
        Op::FGT => Some(ConstValue::Bool(left > right)),
        Op::FGE => Some(ConstValue::Bool(left >= right)),
    }
}

pub fn eval_binop(
    op: &FMIRBinaryIntrinsic,
    left: ConstValue,
    right: ConstValue,
) -> Option<ConstValue> {
    match op {
        FMIRBinaryIntrinsic::Integer { op, .. } => {
            let left = match left {
                ConstValue::Int(v) => v,
                ConstValue::Bool(v) => bool_to_int(v),
                _ => return None,
            };
            let right = match right {
                ConstValue::Int(v) => v,
                ConstValue::Bool(v) => bool_to_int(v),
                _ => return None,
            };
            eval_int_binop(op, left, right)
        }
        FMIRBinaryIntrinsic::Float { op, .. } => {
            let ConstValue::Float(left) = left else {
                return None;
            };
            let ConstValue::Float(right) = right else {
                return None;
            };
            eval_float_binop(op, left, right)
        }
        FMIRBinaryIntrinsic::Pointer { op } => {
            let ConstValue::Int(left) = left else {
                return None;
            };
            let ConstValue::Int(right) = right else {
                return None;
            };
            let iop = match op {
                FMIRPointerBinaryIntrinsicOp::EQ => FMIRIntrinsicIBinOp::EQ,
                FMIRPointerBinaryIntrinsicOp::NE => FMIRIntrinsicIBinOp::NE,
                FMIRPointerBinaryIntrinsicOp::LT => FMIRIntrinsicIBinOp::LT,
                FMIRPointerBinaryIntrinsicOp::GT => FMIRIntrinsicIBinOp::GT,
                FMIRPointerBinaryIntrinsicOp::LE => FMIRIntrinsicIBinOp::LE,
                FMIRPointerBinaryIntrinsicOp::GE => FMIRIntrinsicIBinOp::GE,
            };
            eval_int_binop(&iop, left, right)
        }
        FMIRBinaryIntrinsic::PointerDiff { op } => {
            let ConstValue::Int(left) = left else {
                return None;
            };
            let ConstValue::Int(right) = right else {
                return None;
            };
            let iop = match op {
                FMIRIntrinsicPtrDiffBinop::ADD => FMIRIntrinsicIBinOp::ADD,
                FMIRIntrinsicPtrDiffBinop::SUB => FMIRIntrinsicIBinOp::SUB,
            };
            eval_int_binop(&iop, left, right)
        }
    }
}
