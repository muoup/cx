use cx_hmir::{
    HMIRAggregateOp, HMIRBody, HMIROp,
    body::HMIRHoleID,
    expr::{HMIRExprID, HMIRExprKind},
    ty::{HMIRTypeID, HMIRTypeKind, context::HMIRTypeContext},
};
use cx_log::catalogue::typecheck;
use cx_tokens::TokenRange;

use crate::{
    env::function::LocalLoweringContext,
    eval::{
        EvalResult, eval,
        flow::{Evaluation, Incomplete, IncompleteReason},
        types,
    },
    log::hmir_error,
    value::{HMIRValue, constant::HMIRConstant},
};

pub(crate) fn contextual(
    context: &LocalLoweringContext,
    body: &HMIRBody,
    expr: HMIRExprID,
) -> Option<Incomplete<HMIRValue>> {
    let mut holes = Vec::new();
    contextual_holes(context, body, expr, &mut holes);
    if holes.is_empty() {
        return None;
    }
    Some(Incomplete::new(
        context,
        body.expr(expr).span(),
        IncompleteReason::ExpectedType,
        None,
        move |context, body, expected| {
            let expected = context.env().reference_inner(expected).unwrap_or(expected);
            let mark = context.elaboration_mut().bind(&holes, expected);
            let result = eval(context, body, expr);
            context.elaboration_mut().release(mark);
            result
        },
    ))
}

pub(crate) fn type_result(
    context: &mut LocalLoweringContext,
    ty: HMIRTypeID,
    span: &TokenRange,
) -> EvalResult<HMIRValue> {
    let value = super::def::type_value(context, ty, span)?;
    if !types::unsized_array(context.env(), ty) {
        return Ok(Evaluation::Ready(value));
    }
    let span = span.clone();
    Ok(Evaluation::Incomplete(Incomplete::new(
        context,
        &span.clone(),
        IncompleteReason::ArrayBound,
        Some(value),
        move |context, _, expected| {
            let env = context.env();
            let HMIRTypeKind::Array {
                element: from,
                length: Some(length),
            } = *env.resolve_kind(expected)
            else {
                return Err(hmir_error(
                    &span,
                    &typecheck::CANNOT_INFER,
                    "the array bound".into(),
                ));
            };
            let HMIRTypeKind::Array { element: to, .. } = *env.resolve_kind(ty) else {
                unreachable!("an incomplete array retains its constructor")
            };
            if !env.same_type(from, to) {
                return Err(hmir_error(
                    &span,
                    &typecheck::TYPE_REQUIREMENT,
                    (
                        "array initializer".into(),
                        types::name(env, ty),
                        Some(types::name(env, expected)),
                    ),
                ));
            }
            let ty = types::array(context.env_mut(), ty, length, &span);
            let universe = types::universe(context.env_mut(), &span);
            Ok(Evaluation::Ready(HMIRValue::constant(
                universe,
                HMIRConstant::Type(ty),
            )))
        },
    )))
}

pub(crate) fn hole(
    context: &mut LocalLoweringContext,
    span: &TokenRange,
    hole: HMIRHoleID,
) -> EvalResult<HMIRValue> {
    if let Some(ty) = context.elaboration().hole(hole) {
        return type_result(context, ty, span);
    }
    let span = span.clone();
    Ok(Evaluation::Incomplete(Incomplete::new(
        context,
        &span.clone(),
        IncompleteReason::ExpectedType,
        None,
        move |context, _, expected| {
            let universe = types::universe(context.env_mut(), &span);
            Ok(Evaluation::Ready(HMIRValue::constant(
                universe,
                HMIRConstant::Type(expected),
            )))
        },
    )))
}

fn contextual_holes(
    context: &LocalLoweringContext,
    body: &HMIRBody,
    expr: HMIRExprID,
    holes: &mut Vec<HMIRHoleID>,
) {
    match body.expr(expr).kind() {
        HMIRExprKind::Op(HMIROp::AggregateOp(HMIRAggregateOp::Initialize { ty, .. })) => {
            if let HMIRExprKind::Hole(hole) = body.expr(*ty).kind()
                && context.elaboration().hole(*hole).is_none()
            {
                holes.push(*hole);
            }
        }
        HMIRExprKind::Block {
            statements,
            scoped,
            yields,
        } if *yields || !*scoped => {
            if let Some(last) = statements.last() {
                contextual_holes(context, body, *last, holes);
            }
        }
        HMIRExprKind::If {
            then_branch,
            else_branch: Some(else_branch),
            ..
        } => {
            contextual_holes(context, body, *then_branch, holes);
            contextual_holes(context, body, *else_branch, holes);
        }
        _ => {}
    }
}
