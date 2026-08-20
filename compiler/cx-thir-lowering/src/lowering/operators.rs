use cx_mir::{
    MIRBinaryOp, MIRCoercion, MIRFloatBinaryOp, MIRIntBinaryOp, MIRPointerBinaryOp,
    MIRPointerOffsetOp, MIRUnaryOp,
};
use cx_thir::thir::{
    data::{THIRType, THIRTypeKind},
    expression::{
        THIRBinOp, THIRCoercion, THIRFloatBinOp, THIRIntBinOp, THIRPtrBinOp, THIRPtrDiffBinOp,
        THIRUnOp,
    },
};

use super::types::{lower_float_type, lower_int_type};
use crate::{
    builder::{MIRBuilder, integer_type},
    lowering::types::lower_type,
};

pub(super) fn lower_binary_op(builder: &mut MIRBuilder<'_>, op: &THIRBinOp) -> MIRBinaryOp {
    match op {
        THIRBinOp::Integer { itype, op } => MIRBinaryOp::Integer {
            ty: lower_int_type(*itype),
            signed: matches!(
                op,
                THIRIntBinOp::IMUL
                    | THIRIntBinOp::IDIV
                    | THIRIntBinOp::IMOD
                    | THIRIntBinOp::ILT
                    | THIRIntBinOp::ILE
                    | THIRIntBinOp::IGT
                    | THIRIntBinOp::IGE
                    | THIRIntBinOp::ASHR
            ),
            op: match op {
                THIRIntBinOp::ADD => MIRIntBinaryOp::Add,
                THIRIntBinOp::SUB => MIRIntBinaryOp::Sub,
                THIRIntBinOp::MUL => MIRIntBinaryOp::Mul,
                THIRIntBinOp::DIV => MIRIntBinaryOp::Div,
                THIRIntBinOp::MOD => MIRIntBinaryOp::Mod,
                THIRIntBinOp::IMUL => MIRIntBinaryOp::SignedMul,
                THIRIntBinOp::IDIV => MIRIntBinaryOp::SignedDiv,
                THIRIntBinOp::IMOD => MIRIntBinaryOp::SignedMod,
                THIRIntBinOp::EQ => MIRIntBinaryOp::Eq,
                THIRIntBinOp::NE => MIRIntBinaryOp::Ne,
                THIRIntBinOp::LT => MIRIntBinaryOp::Lt,
                THIRIntBinOp::LE => MIRIntBinaryOp::Le,
                THIRIntBinOp::GT => MIRIntBinaryOp::Gt,
                THIRIntBinOp::GE => MIRIntBinaryOp::Ge,
                THIRIntBinOp::ILT => MIRIntBinaryOp::SignedLt,
                THIRIntBinOp::ILE => MIRIntBinaryOp::SignedLe,
                THIRIntBinOp::IGT => MIRIntBinaryOp::SignedGt,
                THIRIntBinOp::IGE => MIRIntBinaryOp::SignedGe,
                THIRIntBinOp::LAND => MIRIntBinaryOp::LogicalAnd,
                THIRIntBinOp::LOR => MIRIntBinaryOp::LogicalOr,
                THIRIntBinOp::BAND => MIRIntBinaryOp::BitAnd,
                THIRIntBinOp::BOR => MIRIntBinaryOp::BitOr,
                THIRIntBinOp::BXOR => MIRIntBinaryOp::BitXor,
                THIRIntBinOp::SHL => MIRIntBinaryOp::ShiftLeft,
                THIRIntBinOp::ASHR => MIRIntBinaryOp::ArithmeticShiftRight,
                THIRIntBinOp::LSHR => MIRIntBinaryOp::LogicalShiftRight,
            },
        },
        THIRBinOp::Float { ftype, op } => MIRBinaryOp::Float {
            ty: lower_float_type(*ftype),
            op: match op {
                THIRFloatBinOp::FADD => MIRFloatBinaryOp::Add,
                THIRFloatBinOp::FSUB => MIRFloatBinaryOp::Sub,
                THIRFloatBinOp::FMUL => MIRFloatBinaryOp::Mul,
                THIRFloatBinOp::FDIV => MIRFloatBinaryOp::Div,
                THIRFloatBinOp::FEQ => MIRFloatBinaryOp::Eq,
                THIRFloatBinOp::FNE => MIRFloatBinaryOp::Ne,
                THIRFloatBinOp::FLT => MIRFloatBinaryOp::Lt,
                THIRFloatBinOp::FLE => MIRFloatBinaryOp::Le,
                THIRFloatBinOp::FGT => MIRFloatBinaryOp::Gt,
                THIRFloatBinOp::FGE => MIRFloatBinaryOp::Ge,
            },
        },
        THIRBinOp::PtrDiff { op, ptr_inner } => MIRBinaryOp::PointerOffset {
            op: match op {
                THIRPtrDiffBinOp::ADD => MIRPointerOffsetOp::Add,
                THIRPtrDiffBinOp::SUB => MIRPointerOffsetOp::Sub,
            },
            pointee: lower_type(builder, ptr_inner.as_ref()),
        },
        THIRBinOp::Pointer { op } => MIRBinaryOp::Pointer(match op {
            THIRPtrBinOp::EQ => MIRPointerBinaryOp::Eq,
            THIRPtrBinOp::NE => MIRPointerBinaryOp::Ne,
            THIRPtrBinOp::LT => MIRPointerBinaryOp::Lt,
            THIRPtrBinOp::LE => MIRPointerBinaryOp::Le,
            THIRPtrBinOp::GT => MIRPointerBinaryOp::Gt,
            THIRPtrBinOp::GE => MIRPointerBinaryOp::Ge,
        }),
    }
}

pub(super) fn lower_unary_op(op: &THIRUnOp, operand_type: &THIRType) -> MIRUnaryOp {
    match op {
        THIRUnOp::NEG | THIRUnOp::INEG => {
            let (ty, signed) = integer_type(operand_type);
            MIRUnaryOp::IntegerNeg { ty, signed }
        }
        THIRUnOp::FNEG => MIRUnaryOp::FloatNeg(match operand_type.kind {
            THIRTypeKind::Float { _type } => lower_float_type(_type),
            _ => cx_mir::MIRFloatType::F64,
        }),
        THIRUnOp::BNOT => MIRUnaryOp::BitNot(integer_type(operand_type).0),
        THIRUnOp::LNOT => MIRUnaryOp::LogicalNot,
        THIRUnOp::PreIncrement(amount) => MIRUnaryOp::Increment {
            amount: *amount,
            post: false,
        },
        THIRUnOp::PostIncrement(amount) => MIRUnaryOp::Increment {
            amount: *amount,
            post: true,
        },
    }
}

pub(super) fn lower_coercion(
    coercion: &THIRCoercion,
    from_type: &THIRType,
    _to_type: &THIRType,
) -> MIRCoercion {
    match coercion {
        THIRCoercion::Integral {
            sextend,
            from_type,
            to_type,
        } => MIRCoercion::Integral {
            sign_extend: *sextend,
            from: lower_int_type(*from_type),
            to: lower_int_type(*to_type),
        },
        THIRCoercion::FloatCast { to_type } => MIRCoercion::FloatCast {
            from: match from_type.kind {
                THIRTypeKind::Float { _type } => lower_float_type(_type),
                _ => cx_mir::MIRFloatType::F64,
            },
            to: lower_float_type(*to_type),
        },
        THIRCoercion::IntToFloat { to_type, sextend } => MIRCoercion::IntToFloat {
            from: integer_type(from_type).0,
            to: lower_float_type(*to_type),
            signed: *sextend,
        },
        THIRCoercion::FloatToInt { to_type, sextend } => MIRCoercion::FloatToInt {
            from: match from_type.kind {
                THIRTypeKind::Float { _type } => lower_float_type(_type),
                _ => cx_mir::MIRFloatType::F64,
            },
            to: lower_int_type(*to_type),
            signed: *sextend,
        },
        THIRCoercion::PtrToInt { to_type } => MIRCoercion::PointerToInt {
            to: lower_int_type(*to_type),
        },
        THIRCoercion::IntToPtr { sextend } => MIRCoercion::IntToPointer {
            from: integer_type(from_type).0,
            sign_extend: *sextend,
        },
        THIRCoercion::GetFnPtr => MIRCoercion::FunctionToPointer,
        THIRCoercion::Typechange => MIRCoercion::TypeChange,
        THIRCoercion::ReinterpretBits => MIRCoercion::ReinterpretBits,
    }
}
