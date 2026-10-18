use cx_hmir::{
    HMIRBinaryOp, HMIRBody,
    expr::HMIRExprID,
    ty::{HMIRTypeID, HMIRTypeKind, intrinsic::HMIRIntWidth},
    value::HMIRValue,
};
use cx_log::{CXResult, catalogue::typecheck};
use cx_tokens::TokenRange;

use crate::{
    emit,
    env::function::LocalLoweringContext,
    eval::{
        EvalResult,
        coerce::{arithmetic, implicit},
        control::eval_short_circuit,
        op::{eval_operand, kind},
        types,
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

    let lhs = eval_operand(context, body, span, lhs)?;
    let rhs = eval_operand(context, body, span, rhs)?;

    Ok(binary_operands(context, span, op, lhs, rhs)?)
}

pub(crate) fn binary_operands(
    context: &mut LocalLoweringContext,
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
            let rhs = implicit(context, rhs, lhs.ty(), span)?;
            let ty = types::int(context.env_mut(), HMIRIntWidth::I1, false, span);
            emit::binary(context, op, &lhs, &rhs, ty, span)
        }
        (_, HMIRTypeKind::PointerTo(_)) if is_comparison(op) => {
            let lhs = implicit(context, lhs, rhs.ty(), span)?;
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
            arithmetic::common(context, lhs.ty(), lhs.ty(), span)
        }
        _ => arithmetic::common(context, lhs.ty(), rhs.ty(), span),
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
    if integral_only && matches!(kind(context, common), HMIRTypeKind::Float(_)) {
        return Err(hmir_error(
            span,
            &typecheck::FLOATING_OPERAND,
            op.path().into(),
        ));
    }

    let lhs = implicit(context, lhs, common, span)?;
    let rhs = implicit(context, rhs, common, span)?;
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
    let signed = matches!(kind(context, index.ty()), HMIRTypeKind::Int(int) if int.signed());
    let offset_ty = types::int(context.env_mut(), HMIRIntWidth::I64, signed, span);
    let index = implicit(context, index, offset_ty, span)?;

    emit::offset(context, &pointer, &index, element, subtract, span)
}
