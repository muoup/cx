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
    eval::{EvalResult, coerce, complete::elaborate, flow::EvalStop},
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
    let mut child = context.child(None, FnContext::Comptime(ComptimeContext::new()));
    for (local, value) in inputs {
        emit::storage::bind_local(&mut child, local, value);
    }

    match evaluate(&mut child, body, expr, expected, span) {
        Ok(value) => Ok(value),
        Err(EvalStop::Diagnostic(error)) => Err(error),
        Err(EvalStop::Comptime(_)) => Err(hmir_error(span, &mir::COMPTIME_CONTROL_ESCAPE, ())),
        Err(EvalStop::Runtime(stop)) => {
            unreachable!("runtime stop {stop:?} in a comptime evaluation")
        }
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
    let value = elaborate(context, body, expr)?.complete(context, body, expected)?;
    let value = match expected {
        Some(expected) => coerce::implicit(context, value, expected, span)?,
        None => coerce::std_rvalue(context, value, span)?,
    };
    let value = settle(context, value, span)?;

    match value.kind() {
        HMIRValueKind::Constant(constant) if !constant.holds_local_address() => Ok(value),
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
