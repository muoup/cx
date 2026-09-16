use cx_log::CXResult;
use cx_mir::{
    MIRBinaryOp, MIRCoercion, MIRFloatBinaryOp, MIRInstrKind, MIRIntBinaryOp, MIRPointerBinaryOp,
    MIRPointerOffsetOp, MIRTarget, MIRUnaryOp, MIRValue,
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

pub(super) fn lower_binary_op(
    builder: &mut MIRBuilder<'_>,
    op: &THIRBinOp,
) -> CXResult<MIRBinaryOp> {
    Ok(match op {
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
            pointee: lower_type(builder, ptr_inner.as_ref())?,
        },
        THIRBinOp::Pointer { op } => MIRBinaryOp::Pointer(match op {
            THIRPtrBinOp::EQ => MIRPointerBinaryOp::Eq,
            THIRPtrBinOp::NE => MIRPointerBinaryOp::Ne,
            THIRPtrBinOp::LT => MIRPointerBinaryOp::Lt,
            THIRPtrBinOp::LE => MIRPointerBinaryOp::Le,
            THIRPtrBinOp::GT => MIRPointerBinaryOp::Gt,
            THIRPtrBinOp::GE => MIRPointerBinaryOp::Ge,
        }),
    })
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
    builder: &mut MIRBuilder<'_>,
    operand: MIRValue,
    coercion: &THIRCoercion,
    from_type: &THIRType,
    to_type: &THIRType,
) -> CXResult<MIRValue> {
    let to_type = lower_type(builder, to_type)?;

    let mut emit_coercion = |operand: MIRValue, coercion: MIRCoercion| {
        let out = builder.fun_mut().new_register(to_type, None);

        builder.emit(MIRInstrKind::Coerce {
            out,
            operand,
            coercion,
            to_type,
        });

        Ok(MIRValue::Register(out))
    };

    match coercion {
        THIRCoercion::Integral {
            sextend,
            from_type,
            to_type,
        } => emit_coercion(
            operand,
            MIRCoercion::Integral {
                sign_extend: *sextend,
                from: lower_int_type(*from_type),
                to: lower_int_type(*to_type),
            },
        ),
        THIRCoercion::FloatCast { to_type } => emit_coercion(
            operand,
            MIRCoercion::FloatCast {
                from: match from_type.kind {
                    THIRTypeKind::Float { _type } => lower_float_type(_type),
                    _ => cx_mir::MIRFloatType::F64,
                },
                to: lower_float_type(*to_type),
            },
        ),
        THIRCoercion::IntToFloat { to_type, sextend } => emit_coercion(
            operand,
            MIRCoercion::IntToFloat {
                from: integer_type(from_type).0,
                to: lower_float_type(*to_type),
                signed: *sextend,
            },
        ),
        THIRCoercion::FloatToInt { to_type, sextend } => emit_coercion(
            operand,
            MIRCoercion::FloatToInt {
                from: match from_type.kind {
                    THIRTypeKind::Float { _type } => lower_float_type(_type),
                    _ => cx_mir::MIRFloatType::F64,
                },
                to: lower_int_type(*to_type),
                signed: *sextend,
            },
        ),
        THIRCoercion::PtrToInt { to_type } => emit_coercion(
            operand,
            MIRCoercion::PointerToInt {
                to: lower_int_type(*to_type),
            },
        ),
        THIRCoercion::IntToPtr { sextend } => emit_coercion(
            operand,
            MIRCoercion::IntToPointer {
                from: integer_type(from_type).0,
                sign_extend: *sextend,
            },
        ),
        THIRCoercion::GetFnPtr => emit_coercion(operand, MIRCoercion::FunctionToPointer),
        THIRCoercion::Typechange => emit_coercion(operand, MIRCoercion::TypeChange),
        THIRCoercion::ReinterpretBits => emit_coercion(operand, MIRCoercion::ReinterpretBits),

        THIRCoercion::ReferenceBounding(bounded) => {
            let place = builder.fun_mut().new_place(to_type, None, false);

            builder.emit(MIRInstrKind::Store {
                target: MIRTarget::Place(place),
                value: operand,
                ty: to_type,
            });

            for bound in bounded {
                let Some(value) = builder.fun().local(*bound) else {
                    unreachable!("bound local not found")
                };

                let MIRValue::Reference(target) = value else {
                    unreachable!("bound local is not a target")
                };

                builder.emit(MIRInstrKind::Bind { place, to: target });
            }

            Ok(MIRValue::Reference(cx_mir::MIRTarget::Place(place)))
        }

        THIRCoercion::Unreachable => {
            unreachable!("unreachable coercions do not reach MIR coercion lowering")
        }
    }
}
