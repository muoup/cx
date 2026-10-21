use cx_hmir::{
    HMIRBinaryOp, HMIRBody, HMIRUnaryOp,
    expr::HMIRExprID,
    ty::{HMIRTypeKind, context::HMIRTypeContext, intrinsic::HMIRIntWidth},
};
use cx_log::{CXResult, catalogue::typecheck};
use cx_tokens::TokenRange;
use cx_util::unsafe_float::FloatWrapper;

use crate::{
    emit::{self},
    env::function::LocalLoweringContext,
    eval::{
        EvalResult,
        coerce::truthy,
        eval,
        flow::EvalStop,
        op::{kind, promote_operand},
        types,
    },
    log::hmir_error,
    value::{HMIRValue, constant::HMIRConstant},
};

pub(crate) fn eval_unary(
    context: &mut LocalLoweringContext,
    body: &HMIRBody,
    span: &TokenRange,
    op: HMIRUnaryOp,
    operand: HMIRExprID,
) -> EvalResult<HMIRValue> {
    let value = eval(context, body, operand)?;

    match op {
        HMIRUnaryOp::LNot => {
            let value = truthy(context, value, span)?;
            Ok(emit::unary(context, op, &value, span)?)
        }

        HMIRUnaryOp::PreIncrement
        | HMIRUnaryOp::PreDecrement
        | HMIRUnaryOp::PostIncrement
        | HMIRUnaryOp::PostDecrement => increment(context, span, op, value).map_err(EvalStop::from),

        HMIRUnaryOp::Neg | HMIRUnaryOp::BNot => {
            let value = promote_operand(context, value, span)?;
            match (op, kind(context, value.ty())) {
                (HMIRUnaryOp::Neg, HMIRTypeKind::Float(_) | HMIRTypeKind::Int(_))
                | (HMIRUnaryOp::BNot, HMIRTypeKind::Int(_)) => {
                    Ok(emit::unary(context, op, &value, span)?)
                }
                (_, HMIRTypeKind::Float(_)) => {
                    Err(hmir_error(span, &typecheck::FLOATING_OPERAND, op.path().into()).into())
                }
                _ => Err(hmir_error(
                    span,
                    &typecheck::INVALID_OPERAND,
                    (
                        format!("apply '{}' to", op.path()),
                        types::name(context.env(), value.ty()),
                    ),
                )
                .into()),
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
        return Err(hmir_error(
            span,
            &typecheck::MUTATE_CONST,
            ("increment".into(), types::name(context.env(), ty)),
        ));
    }

    let (prefix, op) = match op {
        HMIRUnaryOp::PreIncrement => (true, HMIRBinaryOp::Add),
        HMIRUnaryOp::PostIncrement => (false, HMIRBinaryOp::Add),
        HMIRUnaryOp::PreDecrement => (true, HMIRBinaryOp::Sub),
        HMIRUnaryOp::PostDecrement => (false, HMIRBinaryOp::Sub),

        _ => unreachable!("increment called with non-increment operator"),
    };

    let current = emit::copy(context, target.clone(), span)?;

    let updated = match kind(context, ty) {
        HMIRTypeKind::Int(_) => {
            let one = HMIRValue::constant(ty, HMIRConstant::Int { value: 1, ty });
            emit::binary(context, op, &current, &one, ty, span)?
        }

        HMIRTypeKind::Float(_) => {
            let one = HMIRConstant::Float {
                value: FloatWrapper::from(1.0),
                ty,
            };
            let one = HMIRValue::constant(ty, one);
            emit::binary(context, op, &current, &one, ty, span)?
        }

        HMIRTypeKind::PointerTo(element) => {
            let offset_ty = types::int(context.env_mut(), HMIRIntWidth::I64, true, span);
            let one = HMIRConstant::Int {
                value: 1,
                ty: offset_ty,
            };
            let one = HMIRValue::constant(offset_ty, one);
            emit::offset(
                context,
                &current,
                &one,
                element,
                matches!(op, HMIRBinaryOp::Sub),
                span,
            )?
        }

        _ => {
            return Err(hmir_error(
                span,
                &typecheck::INVALID_OPERAND,
                ("increment".into(), "type display".into()),
            ));
        }
    };

    emit::storage::write(context, &target, &updated, span)?;

    if prefix { Ok(target) } else { Ok(updated) }
}
