use cx_lmir::types::TypeSize;
use cx_lmir::{
    LMIRCoercionType, LMIRFloatBinOp, LMIRFloatUnOp, LMIRInstructionKind, LMIRIntBinOp,
    LMIRIntUnOp, LMIRPtrBinOp,
};
use cx_mir::ty::interface::MTRegistry;
use cx_mir::ty::layout::calculate_type_layout;
use cx_mir::{
    MIRConstant, MIRFloatIntrinsic, MIRIntIntrinsic, MIRIntType, MIRPtrIntrinsic, MIRTypeKind,
    MIRValue,
};

use crate::context::FunctionContext;

use super::output;
use crate::lowering::typing::{convert_float_type, convert_integer_type};
use crate::lowering::values::lower_read;

pub(super) fn integer(context: &mut FunctionContext<'_, '_>, op: &MIRIntIntrinsic) {
    use MIRIntIntrinsic as I;
    let binary = match op {
        I::Add { out, lhs, rhs } => Some((*out, lhs, rhs, LMIRIntBinOp::ADD)),
        I::Sub { out, lhs, rhs } => Some((*out, lhs, rhs, LMIRIntBinOp::SUB)),
        I::UMul { out, lhs, rhs } => Some((*out, lhs, rhs, LMIRIntBinOp::MUL)),
        I::SMul { out, lhs, rhs } => Some((*out, lhs, rhs, LMIRIntBinOp::IMUL)),
        I::UDiv { out, lhs, rhs } => Some((*out, lhs, rhs, LMIRIntBinOp::UDIV)),
        I::SDiv { out, lhs, rhs } => Some((*out, lhs, rhs, LMIRIntBinOp::IDIV)),
        I::UMod { out, lhs, rhs } => Some((*out, lhs, rhs, LMIRIntBinOp::UREM)),
        I::SMod { out, lhs, rhs } => Some((*out, lhs, rhs, LMIRIntBinOp::IREM)),
        I::Eq { out, lhs, rhs } => Some((*out, lhs, rhs, LMIRIntBinOp::EQ)),
        I::Neq { out, lhs, rhs } => Some((*out, lhs, rhs, LMIRIntBinOp::NE)),
        I::ULt { out, lhs, rhs } => Some((*out, lhs, rhs, LMIRIntBinOp::ULT)),
        I::SLt { out, lhs, rhs } => Some((*out, lhs, rhs, LMIRIntBinOp::ILT)),
        I::ULe { out, lhs, rhs } => Some((*out, lhs, rhs, LMIRIntBinOp::ULE)),
        I::SLe { out, lhs, rhs } => Some((*out, lhs, rhs, LMIRIntBinOp::ILE)),
        I::UGt { out, lhs, rhs } => Some((*out, lhs, rhs, LMIRIntBinOp::UGT)),
        I::SGt { out, lhs, rhs } => Some((*out, lhs, rhs, LMIRIntBinOp::IGT)),
        I::UGe { out, lhs, rhs } => Some((*out, lhs, rhs, LMIRIntBinOp::UGE)),
        I::SGe { out, lhs, rhs } => Some((*out, lhs, rhs, LMIRIntBinOp::IGE)),
        I::LAnd { out, lhs, rhs } => Some((*out, lhs, rhs, LMIRIntBinOp::LAND)),
        I::LOr { out, lhs, rhs } => Some((*out, lhs, rhs, LMIRIntBinOp::LOR)),
        I::BAnd { out, lhs, rhs } => Some((*out, lhs, rhs, LMIRIntBinOp::BAND)),
        I::BOr { out, lhs, rhs } => Some((*out, lhs, rhs, LMIRIntBinOp::BOR)),
        I::BXor { out, lhs, rhs } => Some((*out, lhs, rhs, LMIRIntBinOp::BXOR)),
        I::LShift { out, lhs, rhs } => Some((*out, lhs, rhs, LMIRIntBinOp::SHL)),
        I::ARShift { out, lhs, rhs } => Some((*out, lhs, rhs, LMIRIntBinOp::ASHR)),
        I::LRShift { out, lhs, rhs } => Some((*out, lhs, rhs, LMIRIntBinOp::LSHR)),
        _ => None,
    };
    if let Some((out, lhs, rhs, op)) = binary {
        let left = lower_read(context, lhs);
        let right = lower_read(context, rhs);
        output(
            context,
            out,
            LMIRInstructionKind::IntegerBinOp { op, left, right },
        );
        return;
    }
    match op {
        I::Neg { out, value } | I::LNot { out, value } | I::BNot { out, value } => {
            let kind = match op {
                I::Neg { .. } => LMIRIntUnOp::NEG,
                I::LNot { .. } => LMIRIntUnOp::LNOT,
                _ => LMIRIntUnOp::BNOT,
            };
            let value = lower_read(context, value);
            output(
                context,
                *out,
                LMIRInstructionKind::IntegerUnOp { op: kind, value },
            );
        }
        I::IntCast {
            out,
            value,
            target,
            sign_extend,
        } => {
            let from = integer_type(context, value);
            let coercion_type = if from.bytes() < target.bytes() {
                if *sign_extend {
                    LMIRCoercionType::SExtend
                } else {
                    LMIRCoercionType::ZExtend
                }
            } else if from.bytes() > target.bytes() {
                LMIRCoercionType::Trunc
            } else {
                LMIRCoercionType::BitCast
            };
            let value = lower_read(context, value);
            output(
                context,
                *out,
                LMIRInstructionKind::Coercion {
                    value,
                    coercion_type,
                },
            );
        }
        I::ToFloat {
            out, value, signed, ..
        } => {
            let from = integer_type(context, value);
            let value = lower_read(context, value);
            output(
                context,
                *out,
                LMIRInstructionKind::Coercion {
                    value,
                    coercion_type: LMIRCoercionType::IntToFloat {
                        from: convert_integer_type(from),
                        sextend: *signed,
                    },
                },
            );
        }
        I::ToPtr {
            out,
            value,
            sign_extend,
        } => {
            let from = integer_type(context, value);
            let value = lower_read(context, value);
            output(
                context,
                *out,
                LMIRInstructionKind::Coercion {
                    value,
                    coercion_type: LMIRCoercionType::IntToPtr {
                        from: convert_integer_type(from),
                        sextend: *sign_extend,
                    },
                },
            );
        }
        _ => unreachable!(),
    }
}

pub(super) fn float(context: &mut FunctionContext<'_, '_>, op: &MIRFloatIntrinsic) {
    use MIRFloatIntrinsic as F;
    let binary = match op {
        F::Add { out, lhs, rhs } => Some((*out, lhs, rhs, LMIRFloatBinOp::ADD)),
        F::Sub { out, lhs, rhs } => Some((*out, lhs, rhs, LMIRFloatBinOp::SUB)),
        F::Mul { out, lhs, rhs } => Some((*out, lhs, rhs, LMIRFloatBinOp::FMUL)),
        F::Div { out, lhs, rhs } => Some((*out, lhs, rhs, LMIRFloatBinOp::FDIV)),
        F::Eq { out, lhs, rhs } => Some((*out, lhs, rhs, LMIRFloatBinOp::EQ)),
        F::Neq { out, lhs, rhs } => Some((*out, lhs, rhs, LMIRFloatBinOp::NEQ)),
        F::Lt { out, lhs, rhs } => Some((*out, lhs, rhs, LMIRFloatBinOp::FLT)),
        F::Le { out, lhs, rhs } => Some((*out, lhs, rhs, LMIRFloatBinOp::FLE)),
        F::Gt { out, lhs, rhs } => Some((*out, lhs, rhs, LMIRFloatBinOp::FGT)),
        F::Geq { out, lhs, rhs } => Some((*out, lhs, rhs, LMIRFloatBinOp::FGE)),
        _ => None,
    };
    if let Some((out, lhs, rhs, op)) = binary {
        let left = lower_read(context, lhs);
        let right = lower_read(context, rhs);
        output(
            context,
            out,
            LMIRInstructionKind::FloatBinOp { op, left, right },
        );
        return;
    }
    match op {
        F::Neg { out, value } => {
            let value = lower_read(context, value);
            output(
                context,
                *out,
                LMIRInstructionKind::FloatUnOp {
                    op: LMIRFloatUnOp::NEG,
                    value,
                },
            );
        }
        F::ToInt {
            out, value, signed, ..
        } => {
            let from = float_type(context, value);
            let value = lower_read(context, value);
            output(
                context,
                *out,
                LMIRInstructionKind::Coercion {
                    value,
                    coercion_type: LMIRCoercionType::FloatToInt {
                        from: convert_float_type(from),
                        sextend: *signed,
                    },
                },
            );
        }
        F::FloatCast { out, value, .. } => {
            let from = float_type(context, value);
            let value = lower_read(context, value);
            output(
                context,
                *out,
                LMIRInstructionKind::Coercion {
                    value,
                    coercion_type: LMIRCoercionType::FloatCast {
                        from: convert_float_type(from),
                    },
                },
            );
        }
        _ => unreachable!(),
    }
}

pub(super) fn pointer(context: &mut FunctionContext<'_, '_>, op: &MIRPtrIntrinsic) {
    use MIRPtrIntrinsic as P;
    let (out, lhs, rhs, op, type_size) = match op {
        P::ToInt { out, ptr, .. } => {
            let value = lower_read(context, ptr);
            output(
                context,
                *out,
                LMIRInstructionKind::Coercion {
                    value,
                    coercion_type: LMIRCoercionType::PtrToInt,
                },
            );
            return;
        }
        P::Diff {
            out,
            lhs,
            rhs,
            element_ty,
        } => {
            let stride = calculate_type_layout(context.types(), *element_ty)
                .size()
                .max(1);
            (*out, lhs, rhs, LMIRPtrBinOp::DIFF, stride)
        }
        P::Add { out, ptr, offset } => (*out, ptr, offset, LMIRPtrBinOp::ADD, 1),
        P::Sub { out, ptr, offset } => (*out, ptr, offset, LMIRPtrBinOp::SUB, 1),
        P::Eq { out, lhs, rhs } => (*out, lhs, rhs, LMIRPtrBinOp::EQ, 1),
        P::Neq { out, lhs, rhs } => (*out, lhs, rhs, LMIRPtrBinOp::NE, 1),
        P::Lt { out, lhs, rhs } => (*out, lhs, rhs, LMIRPtrBinOp::LT, 1),
        P::Leq { out, lhs, rhs } => (*out, lhs, rhs, LMIRPtrBinOp::LE, 1),
        P::Gt { out, lhs, rhs } => (*out, lhs, rhs, LMIRPtrBinOp::GT, 1),
        P::Geq { out, lhs, rhs } => (*out, lhs, rhs, LMIRPtrBinOp::GE, 1),
    };
    let left = lower_read(context, lhs);
    let right = lower_read(context, rhs);
    output(
        context,
        out,
        LMIRInstructionKind::PointerBinOp {
            op,
            ptr_type: context.pointer(),
            type_size: TypeSize::from(type_size),
            left,
            right,
        },
    );
}

fn operand_type(context: &FunctionContext<'_, '_>, value: &MIRValue) -> Option<cx_mir::MIRTypeID> {
    match value {
        MIRValue::Register(id) => Some(context.body.register(*id).unwrap().ty),
        MIRValue::PlaceRef(id) => Some(context.body.place(*id).unwrap().ty),
        MIRValue::Constant(MIRConstant::GlobalRef(reference)) => Some(reference.ty),
        _ => None,
    }
}

fn integer_type(context: &FunctionContext<'_, '_>, value: &MIRValue) -> MIRIntType {
    if let MIRValue::Constant(MIRConstant::Integer { ty, .. }) = value {
        return *ty;
    }
    let ty = operand_type(context, value).expect("integer operand lacks a MIR type");
    integer_kind(context, ty)
}

fn integer_kind(context: &FunctionContext<'_, '_>, ty: cx_mir::MIRTypeID) -> MIRIntType {
    match context.types().definition(ty).unwrap().kind() {
        MIRTypeKind::Integer { ty } => *ty,
        _ => panic!("integer operation on non-integer"),
    }
}

fn float_type(context: &FunctionContext<'_, '_>, value: &MIRValue) -> cx_mir::MIRFloatType {
    if let MIRValue::Constant(MIRConstant::Float { ty, .. }) = value {
        return *ty;
    }
    let ty = operand_type(context, value).expect("float operand lacks a MIR type");
    match context.types().definition(ty).unwrap().kind() {
        MIRTypeKind::Float { ty } => *ty,
        _ => panic!("float operation on non-float"),
    }
}
