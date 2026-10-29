use cx_hmir::{
    HMIRBinaryOp, HMIRBody, HMIRUnaryOp,
    expr::HMIRExprID,
    ty::{HMIRTypeID, HMIRTypeKind, context::HMIRTypeContext, intrinsic::HMIRIntWidth},
};
use cx_log::{CXResult, catalogue::typecheck};
use cx_tokens::TokenRange;
use cx_util::unsafe_float::FloatWrapper;

use crate::{
    emit,
    env::function::LocalLoweringContext,
    eval::{
        EvalResult, coerce, control, eval, expr,
        flow::Evaluation,
        types,
    },
    log::hmir_error,
    value::{HMIRValue, constant::HMIRConstant},
};

pub(crate) fn eval_operand(
    context: &mut LocalLoweringContext,
    body: &HMIRBody,
    span: &TokenRange,
    expr: HMIRExprID,
) -> EvalResult<Evaluation<HMIRValue>> {
    let value = (eval(context, body, expr))?.ready()?;
    Ok(Evaluation::Ready(coerce::arithmetic(context, value, span)?))
}

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

pub(crate) fn eval_binary(
    context: &mut LocalLoweringContext,
    body: &HMIRBody,
    span: &TokenRange,
    op: HMIRBinaryOp,
    lhs: HMIRExprID,
    rhs: HMIRExprID,
) -> EvalResult<Evaluation<HMIRValue>> {
    if matches!(op, HMIRBinaryOp::LAnd | HMIRBinaryOp::LOr) {
        return control::eval_short_circuit(context, body, span, op, lhs, rhs);
    }
    let lhs = (eval_operand(context, body, span, lhs))?.ready()?;
    let rhs = (eval_operand(context, body, span, rhs))?.ready()?;
    Ok(Evaluation::Ready(binary_operands(context, span, op, lhs, rhs)?))
}

pub(crate) fn binary_operands(
    context: &mut LocalLoweringContext,
    span: &TokenRange,
    op: HMIRBinaryOp,
    lhs: HMIRValue,
    rhs: HMIRValue,
) -> CXResult<HMIRValue> {
    let env = context.env();
    match (env.resolve_kind(lhs.ty()), env.resolve_kind(rhs.ty())) {
        (HMIRTypeKind::PointerTo(element), HMIRTypeKind::Int(_))
            if matches!(op, HMIRBinaryOp::Add | HMIRBinaryOp::Sub) =>
        {
            pointer_offset(context, span, lhs, rhs, *element, op == HMIRBinaryOp::Sub)
        }
        (HMIRTypeKind::Int(_), HMIRTypeKind::PointerTo(element)) if op == HMIRBinaryOp::Add => {
            pointer_offset(context, span, rhs, lhs, *element, false)
        }
        (HMIRTypeKind::PointerTo(element), HMIRTypeKind::PointerTo(_)) if op == HMIRBinaryOp::Sub => {
            let element = *element;
            let ty = types::int(context.env_mut(), HMIRIntWidth::I64, true, span);
            emit::difference(context, &lhs, &rhs, element, ty, span)
        }
        (left, right)
            if is_comparison(op)
                && matches!((left, right), (HMIRTypeKind::PointerTo(_), _) | (_, HMIRTypeKind::PointerTo(_))) =>
        {
            let (lhs, rhs) = match (left, right) {
                (HMIRTypeKind::PointerTo(left), HMIRTypeKind::PointerTo(right)) => {
                    let ty = coerce::pointer(context, *left, *right, span);
                    let lhs = coerce::implicit(context, lhs, ty, span)?;
                    (lhs, coerce::implicit(context, rhs, ty, span)?)
                }
                (HMIRTypeKind::PointerTo(_), _) => {
                    let ty = lhs.ty();
                    (lhs, coerce::implicit(context, rhs, ty, span)?)
                }
                _ => {
                    let ty = rhs.ty();
                    (coerce::implicit(context, lhs, ty, span)?, rhs)
                }
            };
            let ty = types::int(context.env_mut(), HMIRIntWidth::I1, false, span);
            emit::binary(context, op, &lhs, &rhs, ty, span)
        }
        _ => arithmetic(context, span, op, lhs, rhs),
    }
}

fn arithmetic(
    context: &mut LocalLoweringContext,
    span: &TokenRange,
    op: HMIRBinaryOp,
    lhs: HMIRValue,
    rhs: HMIRValue,
) -> CXResult<HMIRValue> {
    let operand = match op {
        HMIRBinaryOp::LShift | HMIRBinaryOp::RShift => lhs.ty(),
        _ => rhs.ty(),
    };
    let Some(common) = coerce::common(context, lhs.ty(), operand, span) else {
        let operands = (
            op.path().into(),
            types::name(context.env(), lhs.ty()),
            types::name(context.env(), rhs.ty()),
        );
        return Err(hmir_error(span, &typecheck::INVALID_BINARY_OPERANDS, operands).into());
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
    if integral_only && context.env().is_float(common) {
        return Err(hmir_error(span, &typecheck::FLOATING_OPERAND, op.path().into()).into());
    }
    let lhs = coerce::implicit(context, lhs, common, span)?;
    let rhs = coerce::implicit(context, rhs, common, span)?;
    let result = match is_comparison(op) {
        true => types::int(context.env_mut(), HMIRIntWidth::I1, false, span),
        false => common,
    };
    emit::binary(context, op, &lhs, &rhs, result, span)
}

fn pointer_offset(
    context: &mut LocalLoweringContext,
    span: &TokenRange,
    pointer: HMIRValue,
    index: HMIRValue,
    element: HMIRTypeID,
    subtract: bool,
) -> CXResult<HMIRValue> {
    let signed = matches!(context.env().resolve_kind(index.ty()), HMIRTypeKind::Int(int) if int.signed());
    let offset_ty = types::int(context.env_mut(), HMIRIntWidth::I64, signed, span);
    let index = coerce::implicit(context, index, offset_ty, span)?;
    emit::offset(context, &pointer, &index, element, subtract, span)
}

pub(crate) fn eval_unary(
    context: &mut LocalLoweringContext,
    body: &HMIRBody,
    span: &TokenRange,
    op: HMIRUnaryOp,
    operand: HMIRExprID,
) -> EvalResult<Evaluation<HMIRValue>> {
    match op {
        HMIRUnaryOp::LNot => {
            let value = (eval(context, body, operand))?.ready()?;
            let value = coerce::condition(context, value, span)?;
            Ok(Evaluation::Ready(emit::unary(context, op, &value, span)?))
        }
        HMIRUnaryOp::PreIncrement
        | HMIRUnaryOp::PreDecrement
        | HMIRUnaryOp::PostIncrement
        | HMIRUnaryOp::PostDecrement => {
            expr::require_pure(context, span, "increment or decrement")?;
            let value = (eval(context, body, operand))?.ready()?;
            let value = coerce::collapse(context, value, span)?;
            Ok(Evaluation::Ready(increment(context, span, op, value)?))
        }
        HMIRUnaryOp::Neg | HMIRUnaryOp::BNot => {
            let value = (eval_operand(context, body, span, operand))?.ready()?;
            match (op, context.env().resolve_kind(value.ty())) {
                (HMIRUnaryOp::Neg, HMIRTypeKind::Float(_) | HMIRTypeKind::Int(_))
                | (HMIRUnaryOp::BNot, HMIRTypeKind::Int(_)) => {
                    Ok(Evaluation::Ready(emit::unary(context, op, &value, span)?))
                }
                (_, HMIRTypeKind::Float(_)) => {
                    Err(hmir_error(span, &typecheck::FLOATING_OPERAND, op.path().into()).into())
                }
                _ => {
                    let operand = (format!("apply '{}' to", op.path()), types::name(context.env(), value.ty()));
                    Err(hmir_error(span, &typecheck::INVALID_OPERAND, operand).into())
                }
            }
        }
    }
}

fn increment(
    context: &mut LocalLoweringContext,
    span: &TokenRange,
    op: HMIRUnaryOp,
    target: HMIRValue,
) -> CXResult<HMIRValue> {
    let Some(ty) = context.env().reference_inner(target.ty()) else {
        return Err(hmir_error(span, &typecheck::NOT_ADDRESSABLE, "increment".into()).into());
    };
    if context.env().semantics(ty).constant {
        let mutation = ("increment".into(), types::name(context.env(), ty));
        return Err(hmir_error(span, &typecheck::MUTATE_CONST, mutation).into());
    }
    let (prefix, op) = match op {
        HMIRUnaryOp::PreIncrement => (true, HMIRBinaryOp::Add),
        HMIRUnaryOp::PostIncrement => (false, HMIRBinaryOp::Add),
        HMIRUnaryOp::PreDecrement => (true, HMIRBinaryOp::Sub),
        HMIRUnaryOp::PostDecrement => (false, HMIRBinaryOp::Sub),
        _ => unreachable!("increment called with non-increment operator"),
    };
    let current = coerce::std_rvalue(context, target.clone(), span)?;
    let updated = match context.env().resolve_kind(ty) {
        HMIRTypeKind::Int(_) => {
            let one = HMIRValue::constant(ty, HMIRConstant::Int(1));
            emit::binary(context, op, &current, &one, ty, span)?
        }
        HMIRTypeKind::Float(_) => {
            let one = HMIRValue::constant(ty, HMIRConstant::Float(FloatWrapper::from(1.0)));
            emit::binary(context, op, &current, &one, ty, span)?
        }
        HMIRTypeKind::PointerTo(element) => {
            let element = *element;
            let offset_ty = types::int(context.env_mut(), HMIRIntWidth::I64, true, span);
            let one = HMIRValue::constant(offset_ty, HMIRConstant::Int(1));
            emit::offset(context, &current, &one, element, op == HMIRBinaryOp::Sub, span)?
        }
        _ => {
            let operand = ("increment".into(), "type display".into());
            return Err(hmir_error(span, &typecheck::INVALID_OPERAND, operand).into());
        }
    };
    emit::write(context, &target, &updated, span)?;
    Ok(if prefix { target } else { current })
}
