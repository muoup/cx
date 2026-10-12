use cx_hmir::{
    HMIRBinaryOp, HMIRUnaryOp,
    expr::constant::HMIRConstant,
    ty::{HMIRTypeID, HMIRTypeKind, context::HMIRTypeContext, intrinsic::HMIRIntWidth},
    value::{HMIRValue, HMIRValueKind},
};
use cx_log::CXResult;
use cx_mir::{
    MIRFloatIntrinsic, MIRIntIntrinsic, MIRInternalIntrinsic, MIRIntrinsic, MIRPtrIntrinsic,
    MIRTarget, MIRValue, ty::layout::calculate_type_layout,
};
use cx_tokens::TokenRange;

use super::{intrinsic, materialize, output, register};
use crate::{
    env::function::FnLoweringContext,
    eval::types::{self, mir_float, mir_int},
};

fn kind(context: &FnLoweringContext, ty: HMIRTypeID) -> HMIRTypeKind {
    context.env().resolve_type_id(ty).kind().clone()
}

pub fn binary(
    context: &mut FnLoweringContext,
    op: HMIRBinaryOp,
    lhs: &HMIRValue,
    rhs: &HMIRValue,
    result: HMIRTypeID,
    span: &TokenRange,
) -> CXResult<HMIRValue> {
    let operands = kind(context, lhs.ty());
    let lhs = materialize(context, lhs, span)?;
    let rhs = materialize(context, rhs, span)?;
    let (out, result) = output(context, result, span)?;

    let instruction: MIRIntrinsic = match operands {
        HMIRTypeKind::Float(_) => match op {
            HMIRBinaryOp::Add => MIRFloatIntrinsic::Add { out, lhs, rhs },
            HMIRBinaryOp::Sub => MIRFloatIntrinsic::Sub { out, lhs, rhs },
            HMIRBinaryOp::Mul => MIRFloatIntrinsic::Mul { out, lhs, rhs },
            HMIRBinaryOp::Div => MIRFloatIntrinsic::Div { out, lhs, rhs },
            HMIRBinaryOp::Eq => MIRFloatIntrinsic::Eq { out, lhs, rhs },
            HMIRBinaryOp::Neq => MIRFloatIntrinsic::Neq { out, lhs, rhs },
            HMIRBinaryOp::Lt => MIRFloatIntrinsic::Lt { out, lhs, rhs },
            HMIRBinaryOp::Le => MIRFloatIntrinsic::Le { out, lhs, rhs },
            HMIRBinaryOp::Gt => MIRFloatIntrinsic::Gt { out, lhs, rhs },
            HMIRBinaryOp::Ge => MIRFloatIntrinsic::Geq { out, lhs, rhs },
            _ => unreachable!("'{}' has no floating form", op.path()),
        }
        .into(),
        HMIRTypeKind::Int(int) => {
            let signed = int.signed();
            match op {
                HMIRBinaryOp::Add => MIRIntIntrinsic::Add { out, lhs, rhs },
                HMIRBinaryOp::Sub => MIRIntIntrinsic::Sub { out, lhs, rhs },
                HMIRBinaryOp::Mul if signed => MIRIntIntrinsic::SMul { out, lhs, rhs },
                HMIRBinaryOp::Mul => MIRIntIntrinsic::UMul { out, lhs, rhs },
                HMIRBinaryOp::Div if signed => MIRIntIntrinsic::SDiv { out, lhs, rhs },
                HMIRBinaryOp::Div => MIRIntIntrinsic::UDiv { out, lhs, rhs },
                HMIRBinaryOp::Mod if signed => MIRIntIntrinsic::SMod { out, lhs, rhs },
                HMIRBinaryOp::Mod => MIRIntIntrinsic::UMod { out, lhs, rhs },
                HMIRBinaryOp::Eq => MIRIntIntrinsic::Eq { out, lhs, rhs },
                HMIRBinaryOp::Neq => MIRIntIntrinsic::Neq { out, lhs, rhs },
                HMIRBinaryOp::Lt if signed => MIRIntIntrinsic::SLt { out, lhs, rhs },
                HMIRBinaryOp::Lt => MIRIntIntrinsic::ULt { out, lhs, rhs },
                HMIRBinaryOp::Le if signed => MIRIntIntrinsic::SLe { out, lhs, rhs },
                HMIRBinaryOp::Le => MIRIntIntrinsic::ULe { out, lhs, rhs },
                HMIRBinaryOp::Gt if signed => MIRIntIntrinsic::SGt { out, lhs, rhs },
                HMIRBinaryOp::Gt => MIRIntIntrinsic::UGt { out, lhs, rhs },
                HMIRBinaryOp::Ge if signed => MIRIntIntrinsic::SGe { out, lhs, rhs },
                HMIRBinaryOp::Ge => MIRIntIntrinsic::UGe { out, lhs, rhs },
                HMIRBinaryOp::BAnd => MIRIntIntrinsic::BAnd { out, lhs, rhs },
                HMIRBinaryOp::BOr => MIRIntIntrinsic::BOr { out, lhs, rhs },
                HMIRBinaryOp::BXor => MIRIntIntrinsic::BXor { out, lhs, rhs },
                HMIRBinaryOp::LShift => MIRIntIntrinsic::LShift { out, lhs, rhs },
                HMIRBinaryOp::RShift if signed => MIRIntIntrinsic::ARShift { out, lhs, rhs },
                HMIRBinaryOp::RShift => MIRIntIntrinsic::LRShift { out, lhs, rhs },
                HMIRBinaryOp::LAnd | HMIRBinaryOp::LOr => {
                    unreachable!("logical operators evaluate as short circuits")
                }
            }
            .into()
        }
        HMIRTypeKind::PointerTo(_) => match op {
            HMIRBinaryOp::Eq => MIRPtrIntrinsic::Eq { out, lhs, rhs },
            HMIRBinaryOp::Neq => MIRPtrIntrinsic::Neq { out, lhs, rhs },
            HMIRBinaryOp::Lt => MIRPtrIntrinsic::Lt { out, lhs, rhs },
            HMIRBinaryOp::Le => MIRPtrIntrinsic::Leq { out, lhs, rhs },
            HMIRBinaryOp::Gt => MIRPtrIntrinsic::Gt { out, lhs, rhs },
            HMIRBinaryOp::Ge => MIRPtrIntrinsic::Geq { out, lhs, rhs },
            _ => unreachable!("'{}' has no pointer form", op.path()),
        }
        .into(),
        _ => unreachable!("binary operands are arithmetic or pointers"),
    };
    intrinsic(context, instruction, span.clone());

    Ok(result)
}

pub fn unary(
    context: &mut FnLoweringContext,
    op: HMIRUnaryOp,
    value: &HMIRValue,
    span: &TokenRange,
) -> CXResult<HMIRValue> {
    let ty = value.ty();
    let value = materialize(context, value, span)?;
    let (out, result) = output(context, ty, span)?;

    let instruction: MIRIntrinsic = match (op, kind(context, ty)) {
        (HMIRUnaryOp::LNot, _) => MIRIntIntrinsic::LNot { out, value }.into(),
        (HMIRUnaryOp::Neg, HMIRTypeKind::Float(_)) => MIRFloatIntrinsic::Neg { out, value }.into(),
        (HMIRUnaryOp::Neg, HMIRTypeKind::Int(_)) => MIRIntIntrinsic::Neg { out, value }.into(),
        (HMIRUnaryOp::BNot, HMIRTypeKind::Int(_)) => MIRIntIntrinsic::BNot { out, value }.into(),
        _ => unreachable!("'{}' does not apply to this operand", op.path()),
    };
    intrinsic(context, instruction, span.clone());

    Ok(result)
}

pub fn cast(
    context: &mut FnLoweringContext,
    value: &HMIRValue,
    target: HMIRTypeID,
    span: &TokenRange,
) -> CXResult<HMIRValue> {
    let kinds = (kind(context, value.ty()), kind(context, target));
    let target_ty = types::lower_type(context.env_mut(), target, span)?;
    let value = materialize(context, value, span)?;
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
    intrinsic(context, instruction, span.clone());

    Ok(result)
}

fn element_size(
    context: &mut FnLoweringContext,
    element: HMIRTypeID,
    span: &TokenRange,
) -> CXResult<i128> {
    let element = types::lower_type(context.env_mut(), element, span)?;

    Ok(calculate_type_layout(context.env().module(), element).size() as i128)
}

pub fn offset(
    context: &mut FnLoweringContext,
    pointer: &HMIRValue,
    index: &HMIRValue,
    element: HMIRTypeID,
    subtract: bool,
    span: &TokenRange,
) -> CXResult<HMIRValue> {
    let ty = index.ty();
    let size = element_size(context, element, span)?;

    let (ptr, offset) = match index.kind() {
        HMIRValueKind::Constant(HMIRConstant::Int { value, .. }) => {
            let value = value * size;
            let offset = HMIRValue::constant(ty, HMIRConstant::Int { value, ty });

            let ptr = materialize(context, pointer, span)?;
            (ptr, materialize(context, &offset, span)?)
        }
        _ => {
            let size = HMIRValue::constant(ty, HMIRConstant::Int { value: size, ty });
            let lhs = materialize(context, index, span)?;
            let rhs = materialize(context, &size, span)?;
            let ptr = materialize(context, pointer, span)?;

            let scaled = register(context, ty, span)?;
            let out = MIRTarget::Register(scaled);
            let scale = match kind(context, ty) {
                HMIRTypeKind::Int(int) if int.signed() => MIRIntIntrinsic::SMul { out, lhs, rhs },
                _ => MIRIntIntrinsic::UMul { out, lhs, rhs },
            };
            intrinsic(context, scale, span.clone());

            (ptr, MIRValue::Register(scaled))
        }
    };

    let (out, result) = output(context, pointer.ty(), span)?;
    let instruction = match subtract {
        true => MIRPtrIntrinsic::Sub { out, ptr, offset },
        false => MIRPtrIntrinsic::Add { out, ptr, offset },
    };
    intrinsic(context, instruction, span.clone());

    Ok(result)
}

pub fn difference(
    context: &mut FnLoweringContext,
    lhs: &HMIRValue,
    rhs: &HMIRValue,
    element: HMIRTypeID,
    result: HMIRTypeID,
    span: &TokenRange,
) -> CXResult<HMIRValue> {
    let element_ty = types::lower_type(context.env_mut(), element, span)?;
    let lhs = materialize(context, lhs, span)?;
    let rhs = materialize(context, rhs, span)?;
    let (out, result) = output(context, result, span)?;
    intrinsic(
        context,
        MIRPtrIntrinsic::Diff {
            out,
            lhs,
            rhs,
            element_ty,
        },
        span.clone(),
    );

    Ok(result)
}
