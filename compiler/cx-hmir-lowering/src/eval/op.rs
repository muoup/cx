use cx_hmir::{
    HMIRBinaryOp, HMIRBody, HMIRUnaryOp,
    expr::{HMIRExprID, constant::HMIRConstant},
    ty::{HMIRTypeID, HMIRTypeKind, context::HMIRTypeContext, intrinsic::HMIRIntWidth},
    value::HMIRValue,
};
use cx_log::{CXResult, catalogue::typecheck};
use cx_tokens::TokenRange;
use cx_util::unsafe_float::FloatWrapper;

use crate::{
    emit,
    env::function::FnLoweringContext,
    eval::{
        EvalResult,
        coerce::{convert, truthy},
        control::eval_short_circuit,
        eval, promote, types,
    },
    log::hmir_error,
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

pub(crate) fn promote_operand(
    context: &mut FnLoweringContext,
    value: HMIRValue,
    span: &TokenRange,
) -> CXResult<HMIRValue> {
    let value = emit::referent(context, value, span)?;
    if matches!(
        kind(context, value.ty()),
        HMIRTypeKind::Array { .. } | HMIRTypeKind::Str | HMIRTypeKind::Function(_)
    ) {
        todo!("array, string or function operand decay")
    }

    let promoted = promote::integer(context, value.ty(), span);
    convert(context, value, promoted, span)
}

pub(crate) fn eval_operand(
    context: &mut FnLoweringContext,
    body: &HMIRBody,
    span: &TokenRange,
    expr: HMIRExprID,
) -> EvalResult<HMIRValue> {
    let value = eval(context, body, expr)?;

    Ok(promote_operand(context, value, span)?)
}

pub(crate) fn eval_binary(
    context: &mut FnLoweringContext,
    body: &HMIRBody,
    span: &TokenRange,
    op: HMIRBinaryOp,
    lhs: HMIRExprID,
    rhs: HMIRExprID,
) -> EvalResult<HMIRValue> {
    if matches!(op, HMIRBinaryOp::LAnd | HMIRBinaryOp::LOr) {
        return eval_short_circuit(context, body, span, op, lhs, rhs);
    }

    let lhs = eval_operand(context, body, span, lhs)?;
    let rhs = eval_operand(context, body, span, rhs)?;

    Ok(binary_operands(context, span, op, lhs, rhs)?)
}

pub(crate) fn binary_operands(
    context: &mut FnLoweringContext,
    span: &TokenRange,
    op: HMIRBinaryOp,
    lhs: HMIRValue,
    rhs: HMIRValue,
) -> CXResult<HMIRValue> {
    match (kind(context, lhs.ty()), kind(context, rhs.ty())) {
        (HMIRTypeKind::PointerTo(element), HMIRTypeKind::Int(_))
            if matches!(op, HMIRBinaryOp::Add | HMIRBinaryOp::Sub) =>
        {
            let subtract = op == HMIRBinaryOp::Sub;
            pointer_offset(context, span, lhs, rhs, element, subtract)
        }
        (HMIRTypeKind::Int(_), HMIRTypeKind::PointerTo(element)) if op == HMIRBinaryOp::Add => {
            pointer_offset(context, span, rhs, lhs, element, false)
        }
        (HMIRTypeKind::PointerTo(element), HMIRTypeKind::PointerTo(_))
            if op == HMIRBinaryOp::Sub =>
        {
            let ty = types::int(context.env_mut(), HMIRIntWidth::I64, true, span);
            emit::difference(context, &lhs, &rhs, element, ty, span)
        }
        (HMIRTypeKind::PointerTo(_), _) if is_comparison(op) => {
            let rhs = convert(context, rhs, lhs.ty(), span)?;
            let ty = types::int(context.env_mut(), HMIRIntWidth::I1, false, span);
            emit::binary(context, op, &lhs, &rhs, ty, span)
        }
        (_, HMIRTypeKind::PointerTo(_)) if is_comparison(op) => {
            let lhs = convert(context, lhs, rhs.ty(), span)?;
            let ty = types::int(context.env_mut(), HMIRIntWidth::I1, false, span);
            emit::binary(context, op, &lhs, &rhs, ty, span)
        }
        _ => arithmetic(context, span, op, lhs, rhs),
    }
}

fn arithmetic(
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

    let integral_only = matches!(
        op,
        HMIRBinaryOp::Mod
            | HMIRBinaryOp::BAnd
            | HMIRBinaryOp::BOr
            | HMIRBinaryOp::BXor
            | HMIRBinaryOp::LShift
            | HMIRBinaryOp::RShift
    );
    if integral_only && matches!(kind(context, common), HMIRTypeKind::Float(_)) {
        return Err(hmir_error(
            span,
            &typecheck::FLOATING_OPERAND,
            op.path().into(),
        ));
    }

    let lhs = convert(context, lhs, common, span)?;
    let rhs = convert(context, rhs, common, span)?;
    let result = match is_comparison(op) {
        true => types::int(context.env_mut(), HMIRIntWidth::I1, false, span),
        false => common,
    };

    emit::binary(context, op, &lhs, &rhs, result, span)
}

fn pointer_offset(
    context: &mut FnLoweringContext,
    span: &TokenRange,
    pointer: HMIRValue,
    index: HMIRValue,
    element: HMIRTypeID,
    subtract: bool,
) -> CXResult<HMIRValue> {
    let signed = matches!(kind(context, index.ty()), HMIRTypeKind::Int(int) if int.signed());
    let offset_ty = types::int(context.env_mut(), HMIRIntWidth::I64, signed, span);
    let index = convert(context, index, offset_ty, span)?;

    emit::offset(context, &pointer, &index, element, subtract, span)
}

pub(crate) fn eval_unary(
    context: &mut FnLoweringContext,
    body: &HMIRBody,
    span: &TokenRange,
    op: HMIRUnaryOp,
    operand: HMIRExprID,
) -> EvalResult<HMIRValue> {
    let value = eval(context, body, operand)?;

    if matches!(
        op,
        HMIRUnaryOp::PreIncrement
            | HMIRUnaryOp::PreDecrement
            | HMIRUnaryOp::PostIncrement
            | HMIRUnaryOp::PostDecrement
    ) {
        return Ok(increment(context, span, op, value)?);
    }

    let value = match op {
        HMIRUnaryOp::LNot => truthy(context, value, span)?,
        _ => promote_operand(context, value, span)?,
    };

    match (op, kind(context, value.ty())) {
        (HMIRUnaryOp::LNot, _)
        | (HMIRUnaryOp::Neg, HMIRTypeKind::Float(_) | HMIRTypeKind::Int(_))
        | (HMIRUnaryOp::BNot, HMIRTypeKind::Int(_)) => Ok(emit::unary(context, op, &value, span)?),
        (_, HMIRTypeKind::Float(_)) => {
            Err(hmir_error(span, &typecheck::FLOATING_OPERAND, op.path().into()).into())
        }
        _ => Err(hmir_error(
            span,
            &typecheck::INVALID_OPERAND,
            (format!("apply '{}' to", op.path()), todo!("type display")),
        )
        .into()),
    }
}

fn increment(
    context: &mut FnLoweringContext,
    span: &TokenRange,
    op: HMIRUnaryOp,
    target: HMIRValue,
) -> CXResult<HMIRValue> {
    let target = emit::referent(context, target, span)?;
    if !emit::addressable(context, &target) {
        return Err(hmir_error(
            span,
            &typecheck::NOT_ADDRESSABLE,
            "increment".into(),
        ));
    }

    let subtract = matches!(op, HMIRUnaryOp::PreDecrement | HMIRUnaryOp::PostDecrement);
    let prefix = matches!(op, HMIRUnaryOp::PreIncrement | HMIRUnaryOp::PreDecrement);
    let step = match subtract {
        true => HMIRBinaryOp::Sub,
        false => HMIRBinaryOp::Add,
    };

    let ty = target.ty();
    let current = emit::read(context, target.clone(), span)?;

    let updated = match kind(context, ty) {
        HMIRTypeKind::Int(_) => {
            let one = HMIRValue::constant(ty, HMIRConstant::Int { value: 1, ty });
            emit::binary(context, step, &current, &one, ty, span)?
        }
        HMIRTypeKind::Float(_) => {
            let one = HMIRConstant::Float {
                value: FloatWrapper::from(1.0),
                ty,
            };
            let one = HMIRValue::constant(ty, one);
            emit::binary(context, step, &current, &one, ty, span)?
        }
        HMIRTypeKind::PointerTo(element) => {
            let offset_ty = types::int(context.env_mut(), HMIRIntWidth::I64, true, span);
            let one = HMIRConstant::Int {
                value: 1,
                ty: offset_ty,
            };
            let one = HMIRValue::constant(offset_ty, one);
            emit::offset(context, &current, &one, element, subtract, span)?
        }
        _ => {
            return Err(hmir_error(
                span,
                &typecheck::INVALID_OPERAND,
                ("increment".into(), todo!("type display")),
            ));
        }
    };
    emit::write(context, &target, updated, false, span)?;

    if prefix { Ok(target) } else { Ok(current) }
}
