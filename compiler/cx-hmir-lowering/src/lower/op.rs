use cx_hmir::{
    HMIRBinaryOp, HMIRBody, HMIRUnaryOp,
    expr::{HMIRExprID, constant::HMIRConstant},
    ty::{HMIRTypeID, HMIRTypeKind, context::HMIRTypeContext, intrinsic::HMIRIntWidth},
};
use cx_log::{CXResult, catalogue::typecheck};
use cx_mir::{
    MIRBlockTarget, MIRFloatIntrinsic, MIRIntIntrinsic, MIRIntrinsic, MIRPtrIntrinsic, MIRTarget,
    MIRValue, ty::layout::calculate_type_layout,
};
use cx_tokens::TokenRange;
use cx_util::{identifier::CXIdent, unsafe_float::FloatWrapper};

use crate::{
    env::lowering::FnLoweringContext,
    log::hmir_error,
    lower::{
        HMIRValue, HMIRValueKind,
        coerce::{convert, truthy},
        constant,
        dispatch::lower_expression,
        emit, promote, types,
    },
};

fn is_comparison(op: HMIRBinaryOp) -> bool {
    matches!(
        op,
        HMIRBinaryOp::Eq
            | HMIRBinaryOp::Neq
            | HMIRBinaryOp::Lt
            | HMIRBinaryOp::Le
            | HMIRBinaryOp::Gt
            | HMIRBinaryOp::Ge
    )
}

fn kind(context: &FnLoweringContext, ty: HMIRTypeID) -> HMIRTypeKind {
    context.env().resolve_type_id(ty).kind().clone()
}

fn element_size(
    context: &mut FnLoweringContext,
    element: HMIRTypeID,
    ty: HMIRTypeID,
    span: &TokenRange,
) -> CXResult<MIRValue> {
    let element = context.env_mut().lower_type(element, span)?;
    let size = calculate_type_layout(context.env().module(), element).size();
    let size = HMIRConstant::Int {
        value: size as i128,
        ty,
    };

    context.materialize(&HMIRValue::constant(ty, size), span)
}

fn promote_operand(
    context: &mut FnLoweringContext,
    value: HMIRValue,
    span: &TokenRange,
) -> CXResult<HMIRValue> {
    match kind(context, value.ty()) {
        HMIRTypeKind::ReferenceTo(_) => {
            todo!("reference operand: needs an indirect place value")
        }
        HMIRTypeKind::Array { .. } | HMIRTypeKind::Str | HMIRTypeKind::Function(_) => {
            todo!("array, string or function operand decay")
        }
        _ => {}
    }

    let promoted = promote::integer(context, value.ty(), span);
    convert(context, value, promoted, span)
}

fn lower_operand(
    context: &mut FnLoweringContext,
    body: &HMIRBody,
    span: &TokenRange,
    expr: HMIRExprID,
) -> CXResult<Option<HMIRValue>> {
    let value = lower_expression(context, body, body.expr(expr))?;
    if emit::terminated(context.builder()) {
        return Ok(None);
    }

    Ok(Some(promote_operand(context, value, span)?))
}

pub(crate) fn lower_binary(
    context: &mut FnLoweringContext,
    body: &HMIRBody,
    span: &TokenRange,
    op: HMIRBinaryOp,
    lhs: HMIRExprID,
    rhs: HMIRExprID,
) -> CXResult<HMIRValue> {
    if matches!(op, HMIRBinaryOp::LAnd | HMIRBinaryOp::LOr) {
        return lower_short_circuit(context, body, span, op, lhs, rhs);
    }

    let Some(lhs) = lower_operand(context, body, span, lhs)? else {
        return Ok(constant::unit(context, span));
    };
    let Some(rhs) = lower_operand(context, body, span, rhs)? else {
        return Ok(constant::unit(context, span));
    };

    match (kind(context, lhs.ty()), kind(context, rhs.ty())) {
        (HMIRTypeKind::PointerTo(element), HMIRTypeKind::Int(_))
            if matches!(op, HMIRBinaryOp::Add | HMIRBinaryOp::Sub) =>
        {
            let subtract = op == HMIRBinaryOp::Sub;
            lower_pointer_offset(context, span, lhs, rhs, element, subtract)
        }
        (HMIRTypeKind::Int(_), HMIRTypeKind::PointerTo(element)) if op == HMIRBinaryOp::Add => {
            lower_pointer_offset(context, span, rhs, lhs, element, false)
        }
        (HMIRTypeKind::PointerTo(element), HMIRTypeKind::PointerTo(_))
            if op == HMIRBinaryOp::Sub =>
        {
            let element_ty = context.env_mut().lower_type(element, span)?;
            let ty = types::int(context.env_mut(), HMIRIntWidth::I64, true, span);
            let lhs = context.materialize(&lhs, span)?;
            let rhs = context.materialize(&rhs, span)?;
            let register = emit::register(context, ty, span)?;
            let intrinsic = MIRPtrIntrinsic::Diff {
                out: MIRTarget::Register(register),
                lhs,
                rhs,
                element_ty,
            };
            emit::intrinsic(context.builder_mut(), intrinsic, span.clone());

            Ok(HMIRValue::register(ty, register))
        }
        (HMIRTypeKind::PointerTo(_), _) if is_comparison(op) => {
            let rhs = convert(context, rhs, lhs.ty(), span)?;
            lower_pointer_comparison(context, span, op, lhs, rhs)
        }
        (_, HMIRTypeKind::PointerTo(_)) if is_comparison(op) => {
            let lhs = convert(context, lhs, rhs.ty(), span)?;
            lower_pointer_comparison(context, span, op, lhs, rhs)
        }
        _ => lower_arithmetic(context, span, op, lhs, rhs),
    }
}

fn lower_arithmetic(
    context: &mut FnLoweringContext,
    span: &TokenRange,
    op: HMIRBinaryOp,
    lhs: HMIRValue,
    rhs: HMIRValue,
) -> CXResult<HMIRValue> {
    let common = match op {
        HMIRBinaryOp::LShift | HMIRBinaryOp::RShift => {
            promote::common(context, lhs.ty(), lhs.ty(), span)
        }
        _ => promote::common(context, lhs.ty(), rhs.ty(), span),
    };
    let Some(common) = common else {
        return Err(hmir_error(
            span,
            &typecheck::INVALID_BINARY_OPERANDS,
            (
                op.path().into(),
                todo!("type display"),
                todo!("type display"),
            ),
        ));
    };

    let lhs = convert(context, lhs, common, span)?;
    let rhs = convert(context, rhs, common, span)?;
    let lhs = context.materialize(&lhs, span)?;
    let rhs = context.materialize(&rhs, span)?;

    let result = match is_comparison(op) {
        true => types::int(context.env_mut(), HMIRIntWidth::I1, false, span),
        false => common,
    };
    let register = emit::register(context, result, span)?;
    let out = MIRTarget::Register(register);

    let intrinsic: MIRIntrinsic = match kind(context, common) {
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
            _ => {
                return Err(hmir_error(
                    span,
                    &typecheck::FLOATING_OPERAND,
                    op.path().into(),
                ));
            }
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
                    unreachable!("logical operators are lowered as short circuits")
                }
            }
            .into()
        }
        _ => unreachable!("arithmetic types are integral or floating"),
    };
    emit::intrinsic(context.builder_mut(), intrinsic, span.clone());

    Ok(HMIRValue::register(result, register))
}

fn lower_pointer_comparison(
    context: &mut FnLoweringContext,
    span: &TokenRange,
    op: HMIRBinaryOp,
    lhs: HMIRValue,
    rhs: HMIRValue,
) -> CXResult<HMIRValue> {
    let lhs = context.materialize(&lhs, span)?;
    let rhs = context.materialize(&rhs, span)?;
    let ty = types::int(context.env_mut(), HMIRIntWidth::I1, false, span);
    let register = emit::register(context, ty, span)?;
    let out = MIRTarget::Register(register);

    let intrinsic = match op {
        HMIRBinaryOp::Eq => MIRPtrIntrinsic::Eq { out, lhs, rhs },
        HMIRBinaryOp::Neq => MIRPtrIntrinsic::Neq { out, lhs, rhs },
        HMIRBinaryOp::Lt => MIRPtrIntrinsic::Lt { out, lhs, rhs },
        HMIRBinaryOp::Le => MIRPtrIntrinsic::Leq { out, lhs, rhs },
        HMIRBinaryOp::Gt => MIRPtrIntrinsic::Gt { out, lhs, rhs },
        HMIRBinaryOp::Ge => MIRPtrIntrinsic::Geq { out, lhs, rhs },
        _ => unreachable!("pointer comparisons are comparisons"),
    };
    emit::intrinsic(context.builder_mut(), intrinsic, span.clone());

    Ok(HMIRValue::register(ty, register))
}

fn lower_pointer_offset(
    context: &mut FnLoweringContext,
    span: &TokenRange,
    pointer: HMIRValue,
    index: HMIRValue,
    element: HMIRTypeID,
    subtract: bool,
) -> CXResult<HMIRValue> {
    let ty = pointer.ty();
    let signed = matches!(kind(context, index.ty()), HMIRTypeKind::Int(int) if int.signed());
    let offset_ty = types::int(context.env_mut(), HMIRIntWidth::I64, signed, span);

    let index = convert(context, index, offset_ty, span)?;
    let lhs = context.materialize(&index, span)?;
    let rhs = element_size(context, element, offset_ty, span)?;
    let ptr = context.materialize(&pointer, span)?;

    let scaled = emit::register(context, offset_ty, span)?;
    let out = MIRTarget::Register(scaled);
    let scale = match signed {
        true => MIRIntIntrinsic::SMul { out, lhs, rhs },
        false => MIRIntIntrinsic::UMul { out, lhs, rhs },
    };
    emit::intrinsic(context.builder_mut(), scale, span.clone());

    let register = emit::register(context, ty, span)?;
    let out = MIRTarget::Register(register);
    let offset = MIRValue::Register(scaled);
    let intrinsic = match subtract {
        true => MIRPtrIntrinsic::Sub { out, ptr, offset },
        false => MIRPtrIntrinsic::Add { out, ptr, offset },
    };
    emit::intrinsic(context.builder_mut(), intrinsic, span.clone());

    Ok(HMIRValue::register(ty, register))
}

fn lower_short_circuit(
    context: &mut FnLoweringContext,
    body: &HMIRBody,
    span: &TokenRange,
    op: HMIRBinaryOp,
    lhs: HMIRExprID,
    rhs: HMIRExprID,
) -> CXResult<HMIRValue> {
    let left = lower_expression(context, body, body.expr(lhs))?;
    if emit::terminated(context.builder()) {
        return Ok(left);
    }

    let left = truthy(context, left, span)?;
    let left = emit::read(context, left, span)?;
    let left = context.materialize(&left, span)?;

    let ty = types::int(context.env_mut(), HMIRIntWidth::I1, false, span);
    let rhs_block = context
        .builder_mut()
        .add_block(Some(CXIdent::new("logical.rhs")));
    let merge = context
        .builder_mut()
        .add_block(Some(CXIdent::new("logical.merge")));
    let result = emit::register(context, ty, span)?;
    context.builder_mut().add_block_param(merge, result);

    let rhs_target = MIRBlockTarget::new(rhs_block);
    let merge_target = MIRBlockTarget::with_args(merge, vec![left.clone()]);
    let (true_target, false_target) = match op {
        HMIRBinaryOp::LAnd => (rhs_target, merge_target),
        _ => (merge_target, rhs_target),
    };
    emit::branch(
        context.builder_mut(),
        left,
        true_target,
        false_target,
        span.clone(),
    );

    context.builder_mut().switch_to(rhs_block);
    let right = lower_expression(context, body, body.expr(rhs))?;
    if !emit::terminated(context.builder()) {
        let right = truthy(context, right, span)?;
        let right = context.materialize(&right, span)?;
        emit::jump(context.builder_mut(), merge, vec![right], span.clone());
    }

    context.builder_mut().switch_to(merge);
    Ok(HMIRValue::register(ty, result))
}

pub(crate) fn lower_unary(
    context: &mut FnLoweringContext,
    body: &HMIRBody,
    span: &TokenRange,
    op: HMIRUnaryOp,
    operand: HMIRExprID,
) -> CXResult<HMIRValue> {
    let value = lower_expression(context, body, body.expr(operand))?;
    if emit::terminated(context.builder()) {
        return Ok(value);
    }

    if matches!(
        op,
        HMIRUnaryOp::PreIncrement
            | HMIRUnaryOp::PreDecrement
            | HMIRUnaryOp::PostIncrement
            | HMIRUnaryOp::PostDecrement
    ) {
        return lower_increment(context, span, op, value);
    }

    let value = match op {
        HMIRUnaryOp::LNot => truthy(context, value, span)?,
        _ => promote_operand(context, value, span)?,
    };
    let ty = value.ty();
    let value = context.materialize(&value, span)?;
    let register = emit::register(context, ty, span)?;
    let out = MIRTarget::Register(register);

    let intrinsic: MIRIntrinsic = match (op, kind(context, ty)) {
        (HMIRUnaryOp::LNot, _) => MIRIntIntrinsic::LNot { out, value }.into(),
        (HMIRUnaryOp::Neg, HMIRTypeKind::Float(_)) => MIRFloatIntrinsic::Neg { out, value }.into(),
        (HMIRUnaryOp::Neg, HMIRTypeKind::Int(_)) => MIRIntIntrinsic::Neg { out, value }.into(),
        (HMIRUnaryOp::BNot, HMIRTypeKind::Int(_)) => MIRIntIntrinsic::BNot { out, value }.into(),
        (_, HMIRTypeKind::Float(_)) => {
            return Err(hmir_error(
                span,
                &typecheck::FLOATING_OPERAND,
                op.path().into(),
            ));
        }
        _ => {
            return Err(hmir_error(
                span,
                &typecheck::INVALID_OPERAND,
                (format!("apply '{}' to", op.path()), todo!("type display")),
            ));
        }
    };
    emit::intrinsic(context.builder_mut(), intrinsic, span.clone());

    Ok(HMIRValue::register(ty, register))
}

fn lower_increment(
    context: &mut FnLoweringContext,
    span: &TokenRange,
    op: HMIRUnaryOp,
    target: HMIRValue,
) -> CXResult<HMIRValue> {
    let HMIRValueKind::PlaceRef(place) = *target.kind() else {
        return Err(hmir_error(
            span,
            &typecheck::NOT_ADDRESSABLE,
            "increment".into(),
        ));
    };

    let subtract = matches!(op, HMIRUnaryOp::PreDecrement | HMIRUnaryOp::PostDecrement);
    let prefix = matches!(op, HMIRUnaryOp::PreIncrement | HMIRUnaryOp::PreDecrement);

    let ty = target.ty();
    let current = emit::read(context, target.clone(), span)?;
    let lhs = context.materialize(&current, span)?;
    let register = emit::register(context, ty, span)?;
    let out = MIRTarget::Register(register);

    let intrinsic: MIRIntrinsic = match kind(context, ty) {
        HMIRTypeKind::Int(_) => {
            let one = HMIRConstant::Int { value: 1, ty };
            let rhs = context.materialize(&HMIRValue::constant(ty, one), span)?;
            match subtract {
                true => MIRIntIntrinsic::Sub { out, lhs, rhs },
                false => MIRIntIntrinsic::Add { out, lhs, rhs },
            }
            .into()
        }
        HMIRTypeKind::Float(_) => {
            let one = HMIRConstant::Float {
                value: FloatWrapper::from(1.0),
                ty,
            };
            let rhs = context.materialize(&HMIRValue::constant(ty, one), span)?;
            match subtract {
                true => MIRFloatIntrinsic::Sub { out, lhs, rhs },
                false => MIRFloatIntrinsic::Add { out, lhs, rhs },
            }
            .into()
        }
        HMIRTypeKind::PointerTo(element) => {
            let offset_ty = types::int(context.env_mut(), HMIRIntWidth::I64, true, span);
            let offset = element_size(context, element, offset_ty, span)?;
            match subtract {
                true => MIRPtrIntrinsic::Sub {
                    out,
                    ptr: lhs,
                    offset,
                },
                false => MIRPtrIntrinsic::Add {
                    out,
                    ptr: lhs,
                    offset,
                },
            }
            .into()
        }
        HMIRTypeKind::ReferenceTo(_) => {
            todo!("increment of a reference: needs an indirect place value")
        }
        _ => {
            return Err(hmir_error(
                span,
                &typecheck::INVALID_OPERAND,
                ("increment".into(), todo!("type display")),
            ));
        }
    };
    emit::intrinsic(context.builder_mut(), intrinsic, span.clone());

    let mir = context.env_mut().lower_type(ty, span)?;
    emit::store(
        context.builder_mut(),
        MIRTarget::Place(place),
        MIRValue::Register(register),
        mir,
        span.clone(),
    );

    if prefix { Ok(target) } else { Ok(current) }
}
