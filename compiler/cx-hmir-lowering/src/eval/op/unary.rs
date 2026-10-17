use cx_hmir::{
    HMIRBinaryOp, HMIRBody, HMIRUnaryOp,
    expr::{HMIRExprID, constant::HMIRConstant},
    ty::{HMIRTypeKind, context::HMIRTypeContext, intrinsic::HMIRIntWidth},
    value::HMIRValue,
};
use cx_log::{CXResult, catalogue::typecheck};
use cx_tokens::TokenRange;
use cx_util::unsafe_float::FloatWrapper;

use crate::{
    emit,
    env::function::LocalLoweringContext,
    eval::{
        EvalResult,
        coerce::truthy,
        eval,
        op::{kind, promote_operand},
        types,
    },
    log::hmir_error,
};

pub(crate) fn eval_unary(
    context: &mut LocalLoweringContext,
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
            (format!("apply '{}' to", op.path()), types::name(context.env(), value.ty())),
        )
        .into()),
    }
}

fn increment(
    context: &mut LocalLoweringContext,
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
    if context
        .env()
        .resolve_type_id(target.ty())
        .semantics()
        .constant
    {
        return Err(hmir_error(
            span,
            &typecheck::MUTATE_CONST,
            ("increment".into(), types::name(context.env(), target.ty())),
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
                ("increment".into(), types::name(context.env(), ty)),
            ));
        }
    };
    emit::write(context, &target, updated, false, span)?;

    if prefix {
        emit::reference(context, &target, span)
    } else {
        Ok(current)
    }
}
