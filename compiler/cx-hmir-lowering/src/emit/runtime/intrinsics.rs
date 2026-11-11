use cx_hmir::ty::{HMIRTypeID, context::HMIRTypeContext};
use cx_intrinsics::{
    AggregateIntrinsic, FloatBinaryOp, FloatIntrinsic, IntBinaryOp, IntIntrinsic, IntUnaryOp,
    InternalIntrinsic, PointerCompareOp, PointerIntrinsic, PointerOffsetOp, VAIntrinsic,
};
use cx_log::CXResult;
use cx_mir::{
    MIRAggregateIntrinsic, MIRFloatIntrinsic, MIRFloatType, MIRIntIntrinsic, MIRIntType,
    MIRInternalIntrinsic, MIRIntrinsic, MIRPtrIntrinsic, MIRTarget, MIRVAIntrinsic, MIRValue,
};
use cx_tokens::TokenRange;

use crate::{
    emit::runtime::{self, storage},
    env::function::LocalLoweringContext,
    eval::types,
    lower::types::{lower_type, mir_float, mir_int},
    value::{HMIRValue, immediate::HMIRImmediate},
};

type Operands = (MIRTarget, MIRValue, MIRValue);

fn int_type(context: &LocalLoweringContext, ty: HMIRTypeID) -> MIRIntType {
    mir_int(types::int_type(context.env(), ty).width())
}

fn float_type(context: &LocalLoweringContext, ty: HMIRTypeID) -> MIRFloatType {
    mir_float(types::float_type(context.env(), ty).width())
}

// An aggregate operand is its storage, so one held behind a reference is read where it is used
fn aggregate_operand(
    context: &mut LocalLoweringContext,
    value: &HMIRValue,
    span: &TokenRange,
) -> CXResult<MIRValue> {
    match context.env().is_reference(value.ty()) {
        true => storage::contents(context, value, span),
        false => runtime::value(context, value, span),
    }
}

fn emit(
    context: &mut LocalLoweringContext,
    intrinsic: impl Into<MIRIntrinsic>,
    result: HMIRValue,
    span: &TokenRange,
) -> CXResult<HMIRValue> {
    context.runtime_mut().emit_intrinsic(intrinsic, span.clone());
    Ok(result)
}

fn binary(
    context: &mut LocalLoweringContext,
    lhs: &HMIRValue,
    rhs: &HMIRValue,
    result: HMIRTypeID,
    span: &TokenRange,
) -> CXResult<(Operands, HMIRValue)> {
    let lhs = runtime::value(context, lhs, span)?;
    let rhs = runtime::value(context, rhs, span)?;
    let (out, result) = runtime::output(context, result, span)?;
    Ok(((out, lhs, rhs), result))
}

fn unary(
    context: &mut LocalLoweringContext,
    value: &HMIRValue,
    result: HMIRTypeID,
    span: &TokenRange,
) -> CXResult<(MIRTarget, MIRValue, HMIRValue)> {
    let value = runtime::value(context, value, span)?;
    let (out, result) = runtime::output(context, result, span)?;
    Ok((out, value, result))
}

pub fn int(
    context: &mut LocalLoweringContext,
    intrinsic: &IntIntrinsic<HMIRValue, HMIRTypeID>,
    result: HMIRTypeID,
    span: &TokenRange,
) -> CXResult<HMIRValue> {
    let (instruction, output) = match intrinsic {
        IntIntrinsic::Binary { op, lhs, rhs } => {
            let ((out, lhs, rhs), output) = binary(context, lhs, rhs, result, span)?;
            let instruction = match op {
            IntBinaryOp::Add => MIRIntIntrinsic::Add { out, lhs, rhs },
            IntBinaryOp::Sub => MIRIntIntrinsic::Sub { out, lhs, rhs },
            IntBinaryOp::UMul => MIRIntIntrinsic::UMul { out, lhs, rhs },
            IntBinaryOp::SMul => MIRIntIntrinsic::SMul { out, lhs, rhs },
            IntBinaryOp::UDiv => MIRIntIntrinsic::UDiv { out, lhs, rhs },
            IntBinaryOp::SDiv => MIRIntIntrinsic::SDiv { out, lhs, rhs },
            IntBinaryOp::UMod => MIRIntIntrinsic::UMod { out, lhs, rhs },
            IntBinaryOp::SMod => MIRIntIntrinsic::SMod { out, lhs, rhs },
            IntBinaryOp::Eq => MIRIntIntrinsic::Eq { out, lhs, rhs },
            IntBinaryOp::Neq => MIRIntIntrinsic::Neq { out, lhs, rhs },
            IntBinaryOp::ULt => MIRIntIntrinsic::ULt { out, lhs, rhs },
            IntBinaryOp::SLt => MIRIntIntrinsic::SLt { out, lhs, rhs },
            IntBinaryOp::ULe => MIRIntIntrinsic::ULe { out, lhs, rhs },
            IntBinaryOp::SLe => MIRIntIntrinsic::SLe { out, lhs, rhs },
            IntBinaryOp::UGt => MIRIntIntrinsic::UGt { out, lhs, rhs },
            IntBinaryOp::SGt => MIRIntIntrinsic::SGt { out, lhs, rhs },
            IntBinaryOp::UGe => MIRIntIntrinsic::UGe { out, lhs, rhs },
            IntBinaryOp::SGe => MIRIntIntrinsic::SGe { out, lhs, rhs },
            IntBinaryOp::LAnd => MIRIntIntrinsic::LAnd { out, lhs, rhs },
            IntBinaryOp::LOr => MIRIntIntrinsic::LOr { out, lhs, rhs },
            IntBinaryOp::BAnd => MIRIntIntrinsic::BAnd { out, lhs, rhs },
            IntBinaryOp::BOr => MIRIntIntrinsic::BOr { out, lhs, rhs },
            IntBinaryOp::BXor => MIRIntIntrinsic::BXor { out, lhs, rhs },
            IntBinaryOp::LShift => MIRIntIntrinsic::LShift { out, lhs, rhs },
            IntBinaryOp::ARShift => MIRIntIntrinsic::ARShift { out, lhs, rhs },
            IntBinaryOp::LRShift => MIRIntIntrinsic::LRShift { out, lhs, rhs },
            };
            (instruction, output)
        }
        IntIntrinsic::Unary { op, value } => {
            let (out, value, output) = unary(context, value, result, span)?;
            let instruction = match op {
                IntUnaryOp::Neg => MIRIntIntrinsic::Neg { out, value },
                IntUnaryOp::LNot => MIRIntIntrinsic::LNot { out, value },
                IntUnaryOp::BNot => MIRIntIntrinsic::BNot { out, value },
            };
            (instruction, output)
        }
        IntIntrinsic::ToFloat {
            value,
            target,
            signed,
        } => {
            let (out, value, output) = unary(context, value, result, span)?;
            let target = float_type(context, *target);
            let signed = *signed;
            (MIRIntIntrinsic::ToFloat { out, value, target, signed }, output)
        }
        IntIntrinsic::Cast {
            value,
            target,
            sign_extend,
        } => {
            let (out, value, output) = unary(context, value, result, span)?;
            let target = int_type(context, *target);
            let sign_extend = *sign_extend;
            (MIRIntIntrinsic::IntCast { out, value, target, sign_extend }, output)
        }
        IntIntrinsic::ToPtr { value, sign_extend } => {
            let (out, value, output) = unary(context, value, result, span)?;
            let sign_extend = *sign_extend;
            (MIRIntIntrinsic::ToPtr { out, value, sign_extend }, output)
        }
    };
    emit(context, instruction, output, span)
}

pub fn float(
    context: &mut LocalLoweringContext,
    intrinsic: &FloatIntrinsic<HMIRValue, HMIRTypeID>,
    result: HMIRTypeID,
    span: &TokenRange,
) -> CXResult<HMIRValue> {
    let (instruction, output) = match intrinsic {
        FloatIntrinsic::Binary { op, lhs, rhs } => {
            let ((out, lhs, rhs), output) = binary(context, lhs, rhs, result, span)?;
            let instruction = match op {
            FloatBinaryOp::Add => MIRFloatIntrinsic::Add { out, lhs, rhs },
            FloatBinaryOp::Sub => MIRFloatIntrinsic::Sub { out, lhs, rhs },
            FloatBinaryOp::Mul => MIRFloatIntrinsic::Mul { out, lhs, rhs },
            FloatBinaryOp::Div => MIRFloatIntrinsic::Div { out, lhs, rhs },
            FloatBinaryOp::Eq => MIRFloatIntrinsic::Eq { out, lhs, rhs },
            FloatBinaryOp::Neq => MIRFloatIntrinsic::Neq { out, lhs, rhs },
            FloatBinaryOp::Lt => MIRFloatIntrinsic::Lt { out, lhs, rhs },
            FloatBinaryOp::Le => MIRFloatIntrinsic::Le { out, lhs, rhs },
            FloatBinaryOp::Gt => MIRFloatIntrinsic::Gt { out, lhs, rhs },
            FloatBinaryOp::Geq => MIRFloatIntrinsic::Geq { out, lhs, rhs },
            };
            (instruction, output)
        }
        FloatIntrinsic::Neg { value } => {
            let (out, value, output) = unary(context, value, result, span)?;
            (MIRFloatIntrinsic::Neg { out, value }, output)
        }
        FloatIntrinsic::ToInt {
            value,
            target,
            signed,
        } => {
            let target_ty = lower_type(context.env_mut(), *target, span)?;
            let (out, value, output) = unary(context, value, result, span)?;
            let signed = *signed;
            (MIRFloatIntrinsic::ToInt { out, value, target_ty, signed }, output)
        }
        FloatIntrinsic::Cast { value, target } => {
            let float_ty = float_type(context, *target);
            let (out, value, output) = unary(context, value, result, span)?;
            (MIRFloatIntrinsic::FloatCast { out, value, float_ty }, output)
        }
    };
    emit(context, instruction, output, span)
}

// Offsets are counted in bytes
pub fn pointer(
    context: &mut LocalLoweringContext,
    intrinsic: &PointerIntrinsic<HMIRValue, HMIRTypeID>,
    result: HMIRTypeID,
    span: &TokenRange,
) -> CXResult<HMIRValue> {
    let (instruction, output) = match intrinsic {
        PointerIntrinsic::ToInt { ptr, target } => {
            let target_ty = lower_type(context.env_mut(), *target, span)?;
            let ptr = runtime::value(context, ptr, span)?;
            let (out, output) = runtime::output(context, result, span)?;
            (MIRPtrIntrinsic::ToInt { out, ptr, target_ty }, output)
        }
        PointerIntrinsic::Offset { op, ptr, offset } => {
            let ((out, ptr, offset), output) = binary(context, ptr, offset, result, span)?;
            let instruction = match op {
                PointerOffsetOp::Add => MIRPtrIntrinsic::Add { out, ptr, offset },
                PointerOffsetOp::Sub => MIRPtrIntrinsic::Sub { out, ptr, offset },
            };
            (instruction, output)
        }
        PointerIntrinsic::Diff { lhs, rhs, element } => {
            let element_ty = lower_type(context.env_mut(), *element, span)?;
            let ((out, lhs, rhs), output) = binary(context, lhs, rhs, result, span)?;
            let instruction = MIRPtrIntrinsic::Diff {
                out,
                lhs,
                rhs,
                element_ty,
            };
            (instruction, output)
        }
        PointerIntrinsic::Compare { op, lhs, rhs } => {
            let ((out, lhs, rhs), output) = binary(context, lhs, rhs, result, span)?;
            let instruction = match op {
                PointerCompareOp::Eq => MIRPtrIntrinsic::Eq { out, lhs, rhs },
                PointerCompareOp::Neq => MIRPtrIntrinsic::Neq { out, lhs, rhs },
                PointerCompareOp::Lt => MIRPtrIntrinsic::Lt { out, lhs, rhs },
                PointerCompareOp::Leq => MIRPtrIntrinsic::Leq { out, lhs, rhs },
                PointerCompareOp::Gt => MIRPtrIntrinsic::Gt { out, lhs, rhs },
                PointerCompareOp::Geq => MIRPtrIntrinsic::Geq { out, lhs, rhs },
            };
            (instruction, output)
        }
    };
    emit(context, instruction, output, span)
}

// Projections take a reference to the aggregate and produce a reference to the projected member
pub fn aggregate(
    context: &mut LocalLoweringContext,
    intrinsic: &AggregateIntrinsic<HMIRValue, HMIRTypeID>,
    result: HMIRTypeID,
    span: &TokenRange,
) -> CXResult<HMIRValue> {
    let (instruction, output) = match intrinsic {
        AggregateIntrinsic::SumIndex { value, sum } => {
            let sum_ty = lower_type(context.env_mut(), *sum, span)?;
            let value = aggregate_operand(context, value, span)?;
            let (out, output) = runtime::output(context, result, span)?;
            (MIRAggregateIntrinsic::SumIndex { out, value, sum_ty }, output)
        }
        AggregateIntrinsic::SumVariant {
            value,
            variant,
            sum,
        } => {
            let sum_ty = lower_type(context.env_mut(), *sum, span)?;
            let base = aggregate_operand(context, value, span)?;
            let (out, output) = runtime::output(context, result, span)?;
            let instruction = MIRAggregateIntrinsic::SumVariant {
                out,
                base,
                variant: *variant,
                sum_ty,
            };
            (instruction, output)
        }
        AggregateIntrinsic::Init { ty, fields } => {
            let ty = lower_type(context.env_mut(), *ty, span)?;
            let fields = fields
                .iter()
                .map(|(index, value)| Ok((*index, runtime::value(context, value, span)?)))
                .collect::<CXResult<_>>()?;
            let (out, output) = runtime::output(context, result, span)?;
            (MIRAggregateIntrinsic::AggregateInit { out, ty, fields }, output)
        }
        AggregateIntrinsic::StructField {
            base,
            field,
            aggregate,
        } => {
            let struct_ty = lower_type(context.env_mut(), *aggregate, span)?;
            let base = aggregate_operand(context, base, span)?;
            let (out, output) = runtime::output(context, result, span)?;
            let instruction = MIRAggregateIntrinsic::StructField {
                out,
                base,
                field: *field,
                struct_ty,
            };
            (instruction, output)
        }
        AggregateIntrinsic::ArrayIndex {
            base,
            index,
            element,
        } => {
            let element_ty = lower_type(context.env_mut(), *element, span)?;
            let base = aggregate_operand(context, base, span)?;
            let index = runtime::value(context, index, span)?;
            let (out, output) = runtime::output(context, result, span)?;
            let instruction = MIRAggregateIntrinsic::ArrayIndex {
                out,
                base,
                index,
                element_ty,
            };
            (instruction, output)
        }
    };
    emit(context, instruction, output, span)
}

pub fn internal(
    context: &mut LocalLoweringContext,
    intrinsic: &InternalIntrinsic<HMIRValue, HMIRTypeID>,
    result: HMIRTypeID,
    span: &TokenRange,
) -> CXResult<HMIRValue> {
    let unit = HMIRValue::new(result, HMIRImmediate::Unit);
    match intrinsic {
        InternalIntrinsic::Bitcast { value, target } => {
            let target_ty = lower_type(context.env_mut(), *target, span)?;
            let value = runtime::value(context, value, span)?;
            let (out, output) = runtime::output(context, result, span)?;
            let instruction = MIRInternalIntrinsic::Bitcast {
                out,
                value,
                target_ty,
            };
            emit(context, instruction, output, span)
        }
        InternalIntrinsic::Assert { condition, message } => {
            let condition = runtime::value(context, condition, span)?;
            let message = message.clone();
            emit(context, MIRInternalIntrinsic::Assert { condition, message }, unit, span)
        }
        InternalIntrinsic::Assume { condition } => {
            let condition = runtime::value(context, condition, span)?;
            emit(context, MIRInternalIntrinsic::Assume { condition }, unit, span)
        }
    }
}

pub fn va(
    context: &mut LocalLoweringContext,
    intrinsic: &VAIntrinsic<HMIRValue, HMIRTypeID>,
    result: HMIRTypeID,
    span: &TokenRange,
) -> CXResult<HMIRValue> {
    let unit = HMIRValue::new(result, HMIRImmediate::Unit);
    match intrinsic {
        VAIntrinsic::Start { list, last } => {
            let list = runtime::value(context, list, span)?;
            let last = runtime::value(context, last, span)?;
            emit(context, MIRVAIntrinsic::VaStart { list, last }, unit, span)
        }
        VAIntrinsic::End { list } => {
            let list = runtime::value(context, list, span)?;
            emit(context, MIRVAIntrinsic::VaEnd { list }, unit, span)
        }
        VAIntrinsic::Arg { list, ty } => {
            let list = runtime::value(context, list, span)?;
            let ty = lower_type(context.env_mut(), *ty, span)?;
            let (out, output) = runtime::output(context, result, span)?;
            emit(context, MIRVAIntrinsic::VaArg { out, list, ty }, output, span)
        }
    }
}
