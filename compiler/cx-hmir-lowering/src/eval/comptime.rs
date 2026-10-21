use cx_hmir::{
    HMIRBody,
    body::HMIRLocalID,
    expr::HMIRExprID,
    ty::{HMIRTypeID, context::HMIRTypeContext},
};
use cx_log::{
    CXResult,
    catalogue::{mir, typecheck},
};
use cx_tokens::TokenRange;

use crate::{
    emit,
    env::function::{ComptimeContext, FnContext, LocalLoweringContext},
    eval::{
        EvalResult, coerce, eval,
        flow::{Evaluation, StopReason, ready},
    },
    log::hmir_error,
    value::{HMIRValue, HMIRValueKind},
};

pub(crate) fn eval_comptime(
    context: &mut LocalLoweringContext,
    body: &HMIRBody,
    expr: HMIRExprID,
    expected: Option<HMIRTypeID>,
) -> CXResult<HMIRValue> {
    let span = body.expr(expr).span();
    let inputs = frozen_inputs(context);
    let elaboration = context.elaboration().clone();
    let mut child = context.child(None, FnContext::Comptime(ComptimeContext::new()));
    child.inherit_elaboration(elaboration);
    for (local, value) in inputs {
        emit::storage::bind_local(&mut child, local, value);
    }

    match evaluate(&mut child, body, expr, expected, span) {
        Ok(Evaluation::Ready(value)) => Ok(value),
        Err(error) => Err(error),
        Ok(Evaluation::Incomplete(incomplete)) => Err(incomplete.error()),
        Ok(Evaluation::Stop(StopReason::Comptime(_))) => {
            Err(hmir_error(span, &mir::COMPTIME_CONTROL_ESCAPE, ()))
        }
        Ok(Evaluation::Stop(StopReason::Runtime(stop))) => {
            unreachable!("runtime stop {stop:?} in a comptime evaluation")
        }
    }
}

pub(crate) fn type_expression(
    context: &mut LocalLoweringContext,
    body: &HMIRBody,
    expr: HMIRExprID,
) -> EvalResult<HMIRTypeID> {
    let inputs = frozen_inputs(context);
    let elaboration = context.elaboration().clone();
    let mut child = context.child(None, FnContext::Comptime(ComptimeContext::new()));
    child.inherit_elaboration(elaboration);
    for (local, value) in inputs {
        emit::storage::bind_local(&mut child, local, value);
    }
    let span = body.expr(expr).span().clone();
    let result = eval(&mut child, body, expr)?.map(&mut child, move |context, value| {
        super::require_type(context, value, &span)
    })?;
    match result {
        Evaluation::Incomplete(incomplete) => {
            let elaboration = child.elaboration().clone();
            let FnContext::Comptime(state) = child.into_context() else {
                unreachable!()
            };
            Ok(Evaluation::Incomplete(
                incomplete.in_comptime(state, elaboration),
            ))
        }
        Evaluation::Stop(StopReason::Comptime(_)) => Err(hmir_error(
            body.expr(expr).span(),
            &mir::COMPTIME_CONTROL_ESCAPE,
            (),
        )),
        Evaluation::Stop(StopReason::Runtime(stop)) => {
            unreachable!("runtime stop {stop:?} in a type expression")
        }
        result => Ok(result),
    }
}

fn frozen_inputs(context: &LocalLoweringContext) -> Vec<(HMIRLocalID, HMIRValue)> {
    let frozen = |(local, value): (&HMIRLocalID, &HMIRValue)| match value.kind() {
        HMIRValueKind::Constant(constant) if !constant.holds_local_address() => {
            Some((*local, value.clone()))
        }
        _ => None,
    };

    match context.context() {
        FnContext::Runtime(runtime) => runtime.locals().filter_map(frozen).collect(),
        FnContext::Comptime(comptime) => comptime.locals().filter_map(frozen).collect(),
    }
}

fn evaluate(
    context: &mut LocalLoweringContext,
    body: &HMIRBody,
    expr: HMIRExprID,
    expected: Option<HMIRTypeID>,
    span: &TokenRange,
) -> EvalResult<HMIRValue> {
    let value = eval(context, body, expr)?;
    let describes_type = value.known().is_some_and(|value| {
        matches!(
            value.kind(),
            HMIRValueKind::Constant(crate::value::constant::HMIRConstant::Type(_))
        )
    });
    let value = if describes_type {
        let type_span = span.clone();
        let ty = ready!(
            value
                .map(context, move |context, value| super::require_type(
                    context, value, &type_span
                ))?
                .description()
        );
        super::def::type_value(context, ty, span)?
    } else {
        ready!(value.complete(context, body, expected))
    };
    let value = match expected {
        Some(expected) => coerce::implicit(context, value, expected, span)?,
        None => coerce::std_rvalue(context, value, span)?,
    };
    let value = settle(context, value, span)?;

    match value.kind() {
        HMIRValueKind::Constant(constant) if !constant.holds_local_address() => {
            Ok(Evaluation::Ready(value))
        }
        _ => {
            let subject = (
                "the address of compile-time storage".into(),
                "the result of a compile-time expression".into(),
            );
            Err(hmir_error(span, &typecheck::INVALID_CONTEXT, subject).into())
        }
    }
}

fn settle(
    context: &mut LocalLoweringContext,
    value: HMIRValue,
    span: &TokenRange,
) -> CXResult<HMIRValue> {
    match value.kind() {
        HMIRValueKind::Constant(_) => Ok(value),
        _ if context.env().is_reference(value.ty()) => Err(hmir_error(
            span,
            &typecheck::INVALID_CONTEXT,
            (
                "a reference to compile-time storage".into(),
                "an exported value".into(),
            ),
        )),
        HMIRValueKind::PlaceRef(_) | HMIRValueKind::Indirect(_) => {
            let reference = emit::reference(context, &value, span)?;
            emit::lift(context, &reference, span)
        }
    }
}
