use cx_hmir::{
    HMIRBinaryOp, HMIRBody,
    expr::HMIRExprID,
    ty::{HMIRTypeID, HMIRTypeKind, context::HMIRTypeContext, intrinsic::HMIRIntWidth},
};
use cx_log::{CXResult, catalogue::typecheck};
use cx_tokens::TokenRange;

use crate::{
    emit,
    env::function::LocalLoweringContext,
    eval::{
        EvalResult, coerce,
        control::eval_short_circuit,
        flow::{Evaluation, ready},
        op::eval_operand,
        types,
    },
    log::hmir_error,
    value::HMIRValue,
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

pub(crate) fn eval_binary(
    context: &mut LocalLoweringContext,
    body: &HMIRBody,
    span: &TokenRange,
    op: HMIRBinaryOp,
    lhs: HMIRExprID,
    rhs: HMIRExprID,
) -> EvalResult<HMIRValue> {
    if matches!(op, HMIRBinaryOp::LAnd | HMIRBinaryOp::LOr) {
        return eval_short_circuit(context, body, span, op, lhs, rhs);
    }

    let lhs = ready!(eval_operand(context, body, span, lhs));
    let rhs = ready!(eval_operand(context, body, span, rhs));

    Ok(Evaluation::Ready(binary_operands(
        context, span, op, lhs, rhs,
    )?))
}

pub(crate) fn binary_operands(
    context: &mut LocalLoweringContext,
    span: &TokenRange,
    op: HMIRBinaryOp,
    lhs: HMIRValue,
    rhs: HMIRValue,
) -> CXResult<HMIRValue> {
    match (
        context.env().resolve_kind(lhs.ty()),
        context.env().resolve_kind(rhs.ty()),
    ) {
        (HMIRTypeKind::PointerTo(element), HMIRTypeKind::Int(_))
            if matches!(op, HMIRBinaryOp::Add | HMIRBinaryOp::Sub) =>
        {
            let subtract = op == HMIRBinaryOp::Sub;
            pointer_offset(context, span, lhs, rhs, *element, subtract)
        }
        (HMIRTypeKind::Int(_), HMIRTypeKind::PointerTo(element)) if op == HMIRBinaryOp::Add => {
            pointer_offset(context, span, rhs, lhs, *element, false)
        }
        (HMIRTypeKind::PointerTo(element), HMIRTypeKind::PointerTo(_))
            if op == HMIRBinaryOp::Sub =>
        {
            let element = *element;
            let ty = types::int(context.env_mut(), HMIRIntWidth::I64, true, span);
            emit::difference(context, &lhs, &rhs, element, ty, span)
        }
        (HMIRTypeKind::PointerTo(_), _) if is_comparison(op) => {
            let rhs = coerce::implicit(context, rhs, lhs.ty(), span)?;
            let ty = types::int(context.env_mut(), HMIRIntWidth::I1, false, span);
            emit::binary(context, op, &lhs, &rhs, ty, span)
        }
        (_, HMIRTypeKind::PointerTo(_)) if is_comparison(op) => {
            let lhs = coerce::implicit(context, lhs, rhs.ty(), span)?;
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
    let common = match op {
        HMIRBinaryOp::LShift | HMIRBinaryOp::RShift => {
            coerce::common(context, lhs.ty(), lhs.ty(), span)
        }
        _ => coerce::common(context, lhs.ty(), rhs.ty(), span),
    };
    let Some(common) = common else {
        return Err(hmir_error(
            span,
            &typecheck::INVALID_BINARY_OPERANDS,
            (
                op.path().into(),
                types::name(context.env(), lhs.ty()),
                types::name(context.env(), rhs.ty()),
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
    if integral_only && context.env().is_float(common) {
        return Err(hmir_error(
            span,
            &typecheck::FLOATING_OPERAND,
            op.path().into(),
        ));
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
    let signed =
        matches!(context.env().resolve_kind(index.ty()), HMIRTypeKind::Int(int) if int.signed());
    let offset_ty = types::int(context.env_mut(), HMIRIntWidth::I64, signed, span);
    let index = coerce::implicit(context, index, offset_ty, span)?;

    emit::offset(context, &pointer, &index, element, subtract, span)
}
