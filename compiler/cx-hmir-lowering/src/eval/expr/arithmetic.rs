use cx_hmir::{
    HMIRBinaryOp, HMIRBodyContext, HMIRUnaryOp,
    expr::HMIRExprID,
    ty::{HMIRTypeID, HMIRTypeKind, context::HMIRTypeContext, intrinsic::HMIRIntWidth},
};
use cx_intrinsics::{
    FloatBinaryOp, FloatIntrinsic, IntBinaryOp, IntIntrinsic, IntUnaryOp, PointerCompareOp,
    PointerIntrinsic, PointerOffsetOp,
};
use cx_log::{CXResult, catalogue::typecheck};
use cx_tokens::TokenRange;

use crate::{
    emit::{self, intrinsics},
    env::function::{LocalLoweringContext, memory::layout},
    eval::{EvalResult, coerce, control, eval, expr, flow::Evaluation, types},
    log::hmir_error,
    value::{self, HMIRValue},
};

pub(crate) fn eval_operand(
    context: &mut LocalLoweringContext,
    body: &HMIRBodyContext,
    span: &TokenRange,
    expr: HMIRExprID,
) -> EvalResult<Evaluation<HMIRValue>> {
    let span = span.clone();

    eval(context, body, expr)?.map(context, move |context, value| {
        coerce::arithmetic(context, value, &span)
    })
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

fn int_op(op: HMIRBinaryOp, signed: bool) -> IntBinaryOp {
    match (op, signed) {
        (HMIRBinaryOp::Add, _) => IntBinaryOp::Add,
        (HMIRBinaryOp::Sub, _) => IntBinaryOp::Sub,
        (HMIRBinaryOp::Mul, true) => IntBinaryOp::SMul,
        (HMIRBinaryOp::Mul, false) => IntBinaryOp::UMul,
        (HMIRBinaryOp::Div, true) => IntBinaryOp::SDiv,
        (HMIRBinaryOp::Div, false) => IntBinaryOp::UDiv,
        (HMIRBinaryOp::Mod, true) => IntBinaryOp::SMod,
        (HMIRBinaryOp::Mod, false) => IntBinaryOp::UMod,
        (HMIRBinaryOp::Eq, _) => IntBinaryOp::Eq,
        (HMIRBinaryOp::Neq, _) => IntBinaryOp::Neq,
        (HMIRBinaryOp::Lt, true) => IntBinaryOp::SLt,
        (HMIRBinaryOp::Lt, false) => IntBinaryOp::ULt,
        (HMIRBinaryOp::Le, true) => IntBinaryOp::SLe,
        (HMIRBinaryOp::Le, false) => IntBinaryOp::ULe,
        (HMIRBinaryOp::Gt, true) => IntBinaryOp::SGt,
        (HMIRBinaryOp::Gt, false) => IntBinaryOp::UGt,
        (HMIRBinaryOp::Ge, true) => IntBinaryOp::SGe,
        (HMIRBinaryOp::Ge, false) => IntBinaryOp::UGe,
        (HMIRBinaryOp::BAnd, _) => IntBinaryOp::BAnd,
        (HMIRBinaryOp::BOr, _) => IntBinaryOp::BOr,
        (HMIRBinaryOp::BXor, _) => IntBinaryOp::BXor,
        (HMIRBinaryOp::LShift, _) => IntBinaryOp::LShift,
        (HMIRBinaryOp::RShift, true) => IntBinaryOp::ARShift,
        (HMIRBinaryOp::RShift, false) => IntBinaryOp::LRShift,
        (HMIRBinaryOp::LAnd | HMIRBinaryOp::LOr, _) => {
            unreachable!("logical operators evaluate as short circuits")
        }
    }
}

fn float_op(op: HMIRBinaryOp) -> FloatBinaryOp {
    match op {
        HMIRBinaryOp::Add => FloatBinaryOp::Add,
        HMIRBinaryOp::Sub => FloatBinaryOp::Sub,
        HMIRBinaryOp::Mul => FloatBinaryOp::Mul,
        HMIRBinaryOp::Div => FloatBinaryOp::Div,
        HMIRBinaryOp::Eq => FloatBinaryOp::Eq,
        HMIRBinaryOp::Neq => FloatBinaryOp::Neq,
        HMIRBinaryOp::Lt => FloatBinaryOp::Lt,
        HMIRBinaryOp::Le => FloatBinaryOp::Le,
        HMIRBinaryOp::Gt => FloatBinaryOp::Gt,
        HMIRBinaryOp::Ge => FloatBinaryOp::Geq,
        _ => unreachable!("'{}' has no floating form", op.path()),
    }
}

fn pointer_op(op: HMIRBinaryOp) -> PointerCompareOp {
    match op {
        HMIRBinaryOp::Eq => PointerCompareOp::Eq,
        HMIRBinaryOp::Neq => PointerCompareOp::Neq,
        HMIRBinaryOp::Lt => PointerCompareOp::Lt,
        HMIRBinaryOp::Le => PointerCompareOp::Leq,
        HMIRBinaryOp::Gt => PointerCompareOp::Gt,
        HMIRBinaryOp::Ge => PointerCompareOp::Geq,
        _ => unreachable!("'{}' has no pointer form", op.path()),
    }
}

pub(crate) fn eval_binary(
    context: &mut LocalLoweringContext,
    body: &HMIRBodyContext,
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
        (HMIRTypeKind::PointerTo(element), HMIRTypeKind::PointerTo(_))
            if op == HMIRBinaryOp::Sub =>
        {
            let element = *element;
            let ty = types::int(context.env_mut(), HMIRIntWidth::I64, true, span);
            difference(context, &lhs, &rhs, element, ty, span)
        }
        (left, right)
            if is_comparison(op)
                && matches!(
                    (left, right),
                    (HMIRTypeKind::PointerTo(_), _) | (_, HMIRTypeKind::PointerTo(_))
                ) =>
        {
            let (lhs, rhs) = match (left, right) {
                (HMIRTypeKind::PointerTo(left), HMIRTypeKind::PointerTo(right)) => {
                    let ty = coerce::pointer(context, *left, *right, span);
                    let lhs = coerce::implicit::standard(context, lhs, ty, span)?;
                    (lhs, coerce::implicit::standard(context, rhs, ty, span)?)
                }
                (HMIRTypeKind::PointerTo(_), _) => {
                    let ty = lhs.ty();
                    (lhs, coerce::implicit::standard(context, rhs, ty, span)?)
                }
                _ => {
                    let ty = rhs.ty();
                    (coerce::implicit::standard(context, lhs, ty, span)?, rhs)
                }
            };
            let ty = types::int(context.env_mut(), HMIRIntWidth::I1, false, span);
            binary(context, op, &lhs, &rhs, ty, span)
        }
        _ => promoted(context, span, op, lhs, rhs),
    }
}

fn promoted(
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
    let Some(common) = coerce::promotion::common(context, lhs.ty(), operand, span) else {
        let operands = (
            op.path().into(),
            types::name(context.env(), lhs.ty()),
            types::name(context.env(), rhs.ty()),
        );
        return Err(hmir_error(
            span,
            &typecheck::INVALID_BINARY_OPERANDS,
            operands,
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
    let lhs = coerce::implicit::standard(context, lhs, common, span)?;
    let rhs = coerce::implicit::standard(context, rhs, common, span)?;
    let result = match is_comparison(op) {
        true => types::int(context.env_mut(), HMIRIntWidth::I1, false, span),
        false => common,
    };
    binary(context, op, &lhs, &rhs, result, span)
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
    let index = coerce::implicit::standard(context, index, offset_ty, span)?;
    offset(context, &pointer, &index, element, subtract, span)
}

pub(crate) fn binary(
    context: &mut LocalLoweringContext,
    op: HMIRBinaryOp,
    lhs: &HMIRValue,
    rhs: &HMIRValue,
    result: HMIRTypeID,
    span: &TokenRange,
) -> CXResult<HMIRValue> {
    let (lhs, rhs) = (lhs.clone(), rhs.clone());
    match context.env().resolve_kind(lhs.ty()) {
        HMIRTypeKind::Int(int) => {
            let op = int_op(op, int.signed());
            intrinsics::int(context, &IntIntrinsic::Binary { op, lhs, rhs }, result, span)
        }
        HMIRTypeKind::Float(_) => {
            let op = float_op(op);
            intrinsics::float(context, &FloatIntrinsic::Binary { op, lhs, rhs }, result, span)
        }
        HMIRTypeKind::PointerTo(_) => {
            let op = pointer_op(op);
            intrinsics::pointer(context, &PointerIntrinsic::Compare { op, lhs, rhs }, result, span)
        }
        _ => unreachable!("'{}' applies to scalar operands", op.path()),
    }
}

pub(crate) fn unary(
    context: &mut LocalLoweringContext,
    op: HMIRUnaryOp,
    value: &HMIRValue,
    span: &TokenRange,
) -> CXResult<HMIRValue> {
    let (ty, value) = (value.ty(), value.clone());
    if op == HMIRUnaryOp::Neg && context.env().is_float(ty) {
        return intrinsics::float(context, &FloatIntrinsic::Neg { value }, ty, span);
    }
    let op = match op {
        HMIRUnaryOp::Neg => IntUnaryOp::Neg,
        HMIRUnaryOp::LNot => IntUnaryOp::LNot,
        HMIRUnaryOp::BNot => IntUnaryOp::BNot,
        _ => unreachable!("'{}' is not a value operator", op.path()),
    };
    intrinsics::int(context, &IntIntrinsic::Unary { op, value }, ty, span)
}

// Offsets 'pointer' by 'index' elements of 'element', scaled to bytes
pub(crate) fn offset(
    context: &mut LocalLoweringContext,
    pointer: &HMIRValue,
    index: &HMIRValue,
    element: HMIRTypeID,
    subtract: bool,
    span: &TokenRange,
) -> CXResult<HMIRValue> {
    let ty = index.ty();
    let size = layout(context.env_mut(), element, span)?.size() as i128;
    let offset = match value::as_int(index) {
        Some(index) => value::int(context.env(), index * size, ty),
        None => {
            let size = value::int(context.env(), size, ty);
            binary(context, HMIRBinaryOp::Mul, index, &size, ty, span)?
        }
    };
    let op = match subtract {
        true => PointerOffsetOp::Sub,
        false => PointerOffsetOp::Add,
    };
    let ptr = pointer.clone();
    intrinsics::pointer(context, &PointerIntrinsic::Offset { op, ptr, offset }, pointer.ty(), span)
}

pub(crate) fn difference(
    context: &mut LocalLoweringContext,
    lhs: &HMIRValue,
    rhs: &HMIRValue,
    element: HMIRTypeID,
    result: HMIRTypeID,
    span: &TokenRange,
) -> CXResult<HMIRValue> {
    let (lhs, rhs) = (lhs.clone(), rhs.clone());
    intrinsics::pointer(context, &PointerIntrinsic::Diff { lhs, rhs, element }, result, span)
}

pub(crate) fn eval_unary(
    context: &mut LocalLoweringContext,
    body: &HMIRBodyContext,
    span: &TokenRange,
    op: HMIRUnaryOp,
    operand: HMIRExprID,
) -> EvalResult<Evaluation<HMIRValue>> {
    match op {
        HMIRUnaryOp::LNot => {
            let value = (eval(context, body, operand))?.ready()?;
            let value = coerce::condition(context, value, span)?;
            Ok(Evaluation::Ready(unary(context, op, &value, span)?))
        }
        HMIRUnaryOp::PreIncrement
        | HMIRUnaryOp::PreDecrement
        | HMIRUnaryOp::PostIncrement
        | HMIRUnaryOp::PostDecrement => {
            expr::require_impure(context, span, "increment or decrement")?;
            let value = (eval(context, body, operand))?.ready()?;
            let value = coerce::collapse(context, value, span)?;
            Ok(Evaluation::Ready(increment(context, span, op, value)?))
        }
        HMIRUnaryOp::Neg | HMIRUnaryOp::BNot => {
            let value = (eval_operand(context, body, span, operand))?.ready()?;
            match (op, context.env().resolve_kind(value.ty())) {
                (HMIRUnaryOp::Neg, HMIRTypeKind::Float(_) | HMIRTypeKind::Int(_))
                | (HMIRUnaryOp::BNot, HMIRTypeKind::Int(_)) => {
                    Ok(Evaluation::Ready(unary(context, op, &value, span)?))
                }
                (_, HMIRTypeKind::Float(_)) => {
                    Err(hmir_error(span, &typecheck::FLOATING_OPERAND, op.path().into()).into())
                }
                _ => {
                    let operand = (
                        format!("apply '{}' to", op.path()),
                        types::name(context.env(), value.ty()),
                    );
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
        return Err(hmir_error(
            span,
            &typecheck::NOT_ADDRESSABLE,
            "increment".into(),
        ));
    };
    if context.env().semantics(ty).constant {
        let mutation = ("increment".into(), types::name(context.env(), ty));
        return Err(hmir_error(span, &typecheck::MUTATE_CONST, mutation));
    }
    let (prefix, op) = match op {
        HMIRUnaryOp::PreIncrement => (true, HMIRBinaryOp::Add),
        HMIRUnaryOp::PostIncrement => (false, HMIRBinaryOp::Add),
        HMIRUnaryOp::PreDecrement => (true, HMIRBinaryOp::Sub),
        HMIRUnaryOp::PostDecrement => (false, HMIRBinaryOp::Sub),
        _ => unreachable!("increment called with non-increment operator"),
    };
    let current = coerce::promotion::std_rvalue(context, target.clone(), span)?;
    let updated = match context.env().resolve_kind(ty) {
        HMIRTypeKind::Int(_) => {
            let one = value::int(context.env(), 1, ty);
            binary(context, op, &current, &one, ty, span)?
        }
        HMIRTypeKind::Float(_) => {
            let one = value::float(context.env(), 1.0, ty);
            binary(context, op, &current, &one, ty, span)?
        }
        HMIRTypeKind::PointerTo(element) => {
            let element = *element;
            let offset_ty = types::int(context.env_mut(), HMIRIntWidth::I64, true, span);
            let one = value::int(context.env(), 1, offset_ty);
            offset(context, &current, &one, element, op == HMIRBinaryOp::Sub, span)?
        }
        _ => {
            let operand = ("increment".into(), "type name".into());
            return Err(hmir_error(span, &typecheck::INVALID_OPERAND, operand));
        }
    };
    emit::storage::write(context, &target, &updated, span)?;
    Ok(if prefix { target } else { current })
}
