use cx_hmir::{
    HMIRBinaryOp, HMIRUnaryOp,
    ty::{HMIRTypeID, HMIRTypeKind, context::HMIRTypeContext, intrinsic::HMIRIntWidth},
};
use cx_log::CXResult;
use cx_mir::{
    MIRAggregateIntrinsic, MIRFloatIntrinsic, MIRIntIntrinsic, MIRInternalIntrinsic, MIRIntrinsic,
    MIRPtrIntrinsic, MIRTarget, MIRValue, ty::layout::calculate_type_layout,
};
use cx_tokens::TokenRange;

use super::{initialized, intrinsic, operand, output, register};
use crate::{
    env::function::LocalLoweringContext,
    eval::types,
    lower::types::{lower_type, mir_float, mir_int},
    value::{HMIRValue, HMIRValueKind, constant::HMIRConstant},
};

fn kind(context: &LocalLoweringContext, ty: HMIRTypeID) -> HMIRTypeKind {
    context.env().resolve_kind(ty).clone()
}

pub fn binary(
    context: &mut LocalLoweringContext,
    op: HMIRBinaryOp,
    lhs: &HMIRValue,
    rhs: &HMIRValue,
    result: HMIRTypeID,
    span: &TokenRange,
) -> CXResult<HMIRValue> {
    let operands = kind(context, lhs.ty());
    let ml = operand(context, lhs, span)?;
    let mr = operand(context, rhs, span)?;
    let (out, result) = output(context, result, span)?;

    let instruction: MIRIntrinsic = match operands {
        HMIRTypeKind::Float(_) => match op {
            HMIRBinaryOp::Add => MIRFloatIntrinsic::Add { out, lhs: ml, rhs: mr },
            HMIRBinaryOp::Sub => MIRFloatIntrinsic::Sub { out, lhs: ml, rhs: mr },
            HMIRBinaryOp::Mul => MIRFloatIntrinsic::Mul { out, lhs: ml, rhs: mr },
            HMIRBinaryOp::Div => MIRFloatIntrinsic::Div { out, lhs: ml, rhs: mr },
            HMIRBinaryOp::Eq => MIRFloatIntrinsic::Eq { out, lhs: ml, rhs: mr },
            HMIRBinaryOp::Neq => MIRFloatIntrinsic::Neq { out, lhs: ml, rhs: mr },
            HMIRBinaryOp::Lt => MIRFloatIntrinsic::Lt { out, lhs: ml, rhs: mr },
            HMIRBinaryOp::Le => MIRFloatIntrinsic::Le { out, lhs: ml, rhs: mr },
            HMIRBinaryOp::Gt => MIRFloatIntrinsic::Gt { out, lhs: ml, rhs: mr },
            HMIRBinaryOp::Ge => MIRFloatIntrinsic::Geq { out, lhs: ml, rhs: mr },
            _ => unreachable!("'{}' has no floating form", op.path()),
        }
        .into(),
        HMIRTypeKind::Int(int) => {
            let signed = int.signed();
            match op {
                HMIRBinaryOp::Add => MIRIntIntrinsic::Add { out, lhs: ml, rhs: mr },
                HMIRBinaryOp::Sub => MIRIntIntrinsic::Sub { out, lhs: ml, rhs: mr },
                HMIRBinaryOp::Mul if signed => MIRIntIntrinsic::SMul { out, lhs: ml, rhs: mr },
                HMIRBinaryOp::Mul => MIRIntIntrinsic::UMul { out, lhs: ml, rhs: mr },
                HMIRBinaryOp::Div if signed => MIRIntIntrinsic::SDiv { out, lhs: ml, rhs: mr },
                HMIRBinaryOp::Div => MIRIntIntrinsic::UDiv { out, lhs: ml, rhs: mr },
                HMIRBinaryOp::Mod if signed => MIRIntIntrinsic::SMod { out, lhs: ml, rhs: mr },
                HMIRBinaryOp::Mod => MIRIntIntrinsic::UMod { out, lhs: ml, rhs: mr },
                HMIRBinaryOp::Eq => MIRIntIntrinsic::Eq { out, lhs: ml, rhs: mr },
                HMIRBinaryOp::Neq => MIRIntIntrinsic::Neq { out, lhs: ml, rhs: mr },
                HMIRBinaryOp::Lt if signed => MIRIntIntrinsic::SLt { out, lhs: ml, rhs: mr },
                HMIRBinaryOp::Lt => MIRIntIntrinsic::ULt { out, lhs: ml, rhs: mr },
                HMIRBinaryOp::Le if signed => MIRIntIntrinsic::SLe { out, lhs: ml, rhs: mr },
                HMIRBinaryOp::Le => MIRIntIntrinsic::ULe { out, lhs: ml, rhs: mr },
                HMIRBinaryOp::Gt if signed => MIRIntIntrinsic::SGt { out, lhs: ml, rhs: mr },
                HMIRBinaryOp::Gt => MIRIntIntrinsic::UGt { out, lhs: ml, rhs: mr },
                HMIRBinaryOp::Ge if signed => MIRIntIntrinsic::SGe { out, lhs: ml, rhs: mr },
                HMIRBinaryOp::Ge => MIRIntIntrinsic::UGe { out, lhs: ml, rhs: mr },
                HMIRBinaryOp::BAnd => MIRIntIntrinsic::BAnd { out, lhs: ml, rhs: mr },
                HMIRBinaryOp::BOr => MIRIntIntrinsic::BOr { out, lhs: ml, rhs: mr },
                HMIRBinaryOp::BXor => MIRIntIntrinsic::BXor { out, lhs: ml, rhs: mr },
                HMIRBinaryOp::LShift => MIRIntIntrinsic::LShift { out, lhs: ml, rhs: mr },
                HMIRBinaryOp::RShift if signed => MIRIntIntrinsic::ARShift { out, lhs: ml, rhs: mr },
                HMIRBinaryOp::RShift => MIRIntIntrinsic::LRShift { out, lhs: ml, rhs: mr },
                HMIRBinaryOp::LAnd | HMIRBinaryOp::LOr => {
                    unreachable!("logical operators evaluate as short circuits")
                }
            }
            .into()
        }
        HMIRTypeKind::PointerTo(_) => match op {
            HMIRBinaryOp::Eq => MIRPtrIntrinsic::Eq { out, lhs: ml, rhs: mr },
            HMIRBinaryOp::Neq => MIRPtrIntrinsic::Neq { out, lhs: ml, rhs: mr },
            HMIRBinaryOp::Lt => MIRPtrIntrinsic::Lt { out, lhs: ml, rhs: mr },
            HMIRBinaryOp::Le => MIRPtrIntrinsic::Leq { out, lhs: ml, rhs: mr },
            HMIRBinaryOp::Gt => MIRPtrIntrinsic::Gt { out, lhs: ml, rhs: mr },
            HMIRBinaryOp::Ge => MIRPtrIntrinsic::Geq { out, lhs: ml, rhs: mr },
            _ => unreachable!("'{}' has no pointer form", op.path()),
        }
        .into(),
        _ => {
            let lhs = types::name(context.env(), lhs.ty());
            let rhs = types::name(context.env(), rhs.ty());
            unreachable!("'{}' does not apply to these {lhs} and {rhs}", op.path())
        }
    };
    intrinsic(context, instruction, span);

    Ok(initialized(context, result, span))
}

pub fn unary(
    context: &mut LocalLoweringContext,
    op: HMIRUnaryOp,
    value: &HMIRValue,
    span: &TokenRange,
) -> CXResult<HMIRValue> {
    let ty = value.ty();
    let value = operand(context, value, span)?;
    let (out, result) = output(context, ty, span)?;

    let instruction: MIRIntrinsic = match (op, kind(context, ty)) {
        (HMIRUnaryOp::LNot, _) => MIRIntIntrinsic::LNot { out, value }.into(),
        (HMIRUnaryOp::Neg, HMIRTypeKind::Float(_)) => MIRFloatIntrinsic::Neg { out, value }.into(),
        (HMIRUnaryOp::Neg, HMIRTypeKind::Int(_)) => MIRIntIntrinsic::Neg { out, value }.into(),
        (HMIRUnaryOp::BNot, HMIRTypeKind::Int(_)) => MIRIntIntrinsic::BNot { out, value }.into(),
        _ => unreachable!("'{}' does not apply to this operand", op.path()),
    };
    intrinsic(context, instruction, span);

    Ok(initialized(context, result, span))
}

pub fn cast(
    context: &mut LocalLoweringContext,
    value: &HMIRValue,
    target: HMIRTypeID,
    span: &TokenRange,
) -> CXResult<HMIRValue> {
    let kinds = (kind(context, value.ty()), kind(context, target));
    let target_ty = lower_type(context.env_mut(), target, span)?;
    let value = operand(context, value, span)?;
    let (out, result) = output(context, target, span)?;

    let instruction: MIRIntrinsic = match kinds {
        (HMIRTypeKind::Int(from), HMIRTypeKind::Int(to)) => MIRIntIntrinsic::IntCast {
            out,
            value,
            target: mir_int(to.width()),
            sign_extend: from.signed() && from.width() != HMIRIntWidth::I1,
        }
        .into(),
        (HMIRTypeKind::Int(from), HMIRTypeKind::Float(to)) => MIRIntIntrinsic::ToFloat {
            out,
            value,
            target: mir_float(to.width()),
            signed: from.signed(),
        }
        .into(),
        (HMIRTypeKind::Int(from), HMIRTypeKind::PointerTo(_)) => MIRIntIntrinsic::ToPtr {
            out,
            value,
            sign_extend: from.signed(),
        }
        .into(),

        (HMIRTypeKind::Float(_), HMIRTypeKind::Int(to)) => MIRFloatIntrinsic::ToInt {
            out,
            value,
            target_ty,
            signed: to.signed(),
        }
        .into(),
        (HMIRTypeKind::Float(_), HMIRTypeKind::Float(to)) => MIRFloatIntrinsic::FloatCast {
            out,
            value,
            float_ty: mir_float(to.width()),
        }
        .into(),

        (HMIRTypeKind::PointerTo(_), HMIRTypeKind::Int(_)) => MIRPtrIntrinsic::ToInt {
            out,
            ptr: value,
            target_ty,
        }
        .into(),
        (HMIRTypeKind::PointerTo(_), HMIRTypeKind::PointerTo(_)) => MIRInternalIntrinsic::Bitcast {
            out,
            value,
            target_ty,
        }
        .into(),

        _ => unreachable!("conversions are between scalar types"),
    };
    intrinsic(context, instruction, span);

    Ok(initialized(context, result, span))
}

fn element_size(
    context: &mut LocalLoweringContext,
    element: HMIRTypeID,
    span: &TokenRange,
) -> CXResult<i128> {
    let element = lower_type(context.env_mut(), element, span)?;

    Ok(calculate_type_layout(context.env().module(), element).size() as i128)
}

pub fn offset(
    context: &mut LocalLoweringContext,
    pointer: &HMIRValue,
    index: &HMIRValue,
    element: HMIRTypeID,
    subtract: bool,
    span: &TokenRange,
) -> CXResult<HMIRValue> {
    let ty = index.ty();
    let size = element_size(context, element, span)?;

    let (ptr, offset) = match index.kind() {
        HMIRValueKind::Constant(HMIRConstant::Int(value)) => {
            let value = value * size;
            let offset = HMIRValue::constant(ty, HMIRConstant::Int(value));

            let ptr = operand(context, pointer, span)?;
            (ptr, operand(context, &offset, span)?)
        }
        _ => {
            let size = HMIRValue::constant(ty, HMIRConstant::Int(size));
            let lhs = operand(context, index, span)?;
            let rhs = operand(context, &size, span)?;
            let ptr = operand(context, pointer, span)?;

            let scaled = register(context, ty, span)?;
            let out = MIRTarget::Register(scaled);
            let scale = match kind(context, ty) {
                HMIRTypeKind::Int(int) if int.signed() => MIRIntIntrinsic::SMul { out, lhs, rhs },
                _ => MIRIntIntrinsic::UMul { out, lhs, rhs },
            };
            intrinsic(context, scale, span);

            (ptr, MIRValue::Register(scaled))
        }
    };

    let (out, result) = output(context, pointer.ty(), span)?;
    let instruction = match subtract {
        true => MIRPtrIntrinsic::Sub { out, ptr, offset },
        false => MIRPtrIntrinsic::Add { out, ptr, offset },
    };
    intrinsic(context, instruction, span);

    Ok(initialized(context, result, span))
}

pub fn difference(
    context: &mut LocalLoweringContext,
    lhs: &HMIRValue,
    rhs: &HMIRValue,
    element: HMIRTypeID,
    result: HMIRTypeID,
    span: &TokenRange,
) -> CXResult<HMIRValue> {
    let element_ty = lower_type(context.env_mut(), element, span)?;
    let lhs = operand(context, lhs, span)?;
    let rhs = operand(context, rhs, span)?;
    let (out, result) = output(context, result, span)?;
    intrinsic(context, MIRPtrIntrinsic::Diff { out, lhs, rhs, element_ty }, span);

    Ok(initialized(context, result, span))
}

pub fn tag(
    context: &mut LocalLoweringContext,
    value: &HMIRValue,
    ty: HMIRTypeID,
    span: &TokenRange,
) -> CXResult<HMIRValue> {
    if let HMIRValueKind::Constant(HMIRConstant::TaggedUnion { variant, .. }) = value.kind() {
        return Ok(HMIRValue::constant(ty, HMIRConstant::Int(*variant as i128)));
    }

    let stored = context.env().referent(value.ty());
    let sum_ty = lower_type(context.env_mut(), stored, span)?;
    let value = operand(context, value, span)?;
    let (out, result) = output(context, ty, span)?;
    intrinsic(context, MIRAggregateIntrinsic::SumIndex { out, value, sum_ty }, span);

    Ok(initialized(context, result, span))
}

pub fn aggregate(
    context: &mut LocalLoweringContext,
    ty: HMIRTypeID,
    fields: &[(usize, HMIRValue)],
    span: &TokenRange,
) -> CXResult<HMIRValue> {
    let fields = fields
        .iter()
        .map(|(index, value)| Ok((*index, operand(context, value, span)?)))
        .collect::<CXResult<_>>()?;

    let mir_ty = lower_type(context.env_mut(), ty, span)?;
    let (out, result) = output(context, ty, span)?;
    intrinsic(context, MIRAggregateIntrinsic::AggregateInit { out, ty: mir_ty, fields }, span);

    Ok(initialized(context, result, span))
}
