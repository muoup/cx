use cx_hmir::{
    HMIRBodyContext,
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
        EvalResult, coerce, deduce, eval,
        flow::{EvalExit, Evaluation, StopReason},
        name::type_value,
    },
    log::hmir_error,
    value::{self, HMIRValue, HMIRValueKind},
};

// Evaluates an expression in a comptime child that sees only the known values of the visible locals
pub(crate) fn freeze(
    context: &mut LocalLoweringContext,
    body: &HMIRBodyContext,
    expr: HMIRExprID,
    expected: Option<HMIRTypeID>,
) -> EvalResult<Evaluation<HMIRValue>> {
    let span = body.expr(expr).span();
    frozen(context, span, |child| {
        evaluate(child, body, expr, expected, span)
    })
}

pub(crate) fn frozen<T: 'static>(
    context: &mut LocalLoweringContext,
    span: &TokenRange,
    evaluate: impl FnOnce(&mut LocalLoweringContext) -> EvalResult<Evaluation<T>>,
) -> EvalResult<Evaluation<T>> {
    let mut child = child(context);
    match evaluate(&mut child) {
        Ok(Evaluation::Incomplete(incomplete)) => Ok(Evaluation::Incomplete(
            incomplete.in_context(child.into_context()),
        )),
        Err(EvalExit::Stop(StopReason::Comptime(_))) => {
            Err(hmir_error(span, &mir::COMPTIME_CONTROL_ESCAPE, ()).into())
        }
        Err(EvalExit::Stop(StopReason::Runtime(stop))) => {
            unreachable!("runtime stop {stop:?} in a comptime evaluation")
        }
        Ok(result) => Ok(result),
        Err(EvalExit::Diagnostic(error)) => Err(EvalExit::Diagnostic(error)),
    }
}

pub(crate) fn child<'a, 'global, 'hmir>(
    context: &'a mut LocalLoweringContext<'_, 'global, 'hmir>,
) -> LocalLoweringContext<'a, 'global, 'hmir> {
    let inputs = inputs(context);
    let evaluation = context.evaluation();
    let comptime = FnContext::Comptime(ComptimeContext::new());
    let mut child = context.child(None, comptime, Some(evaluation));
    for (local, value) in inputs {
        emit::storage::bind_local(&mut child, local, value);
    }
    child
}

fn evaluate(
    context: &mut LocalLoweringContext,
    body: &HMIRBodyContext,
    expr: HMIRExprID,
    expected: Option<HMIRTypeID>,
    span: &TokenRange,
) -> EvalResult<Evaluation<HMIRValue>> {
    let value = eval(context, body, expr)?;
    let value = match value.known().and_then(value::as_type) {
        Some(_) => {
            let ty =
                (deduce::described_type(context, body, value, span)?.description())?.ready()?;
            Evaluation::Ready(type_value(context, ty, span)?)
        }
        None => deduce::complete(context, body, value, expected)?,
    };
    let span = span.clone();
    value.and_then(context, body, move |context, _, value| {
        let value = match expected {
            Some(expected) => coerce::implicit::standard(context, value, expected, &span)?,
            None => coerce::promotion::std_rvalue(context, value, &span)?,
        };
        Ok(Evaluation::Ready(converge(context, value, &span)?))
    })
}

// Brings a compile-time result into its independently well-formed form, which is valid in any context
pub(crate) fn converge(
    context: &mut LocalLoweringContext,
    value: HMIRValue,
    span: &TokenRange,
) -> CXResult<HMIRValue> {
    // A function in compile-time storage cannot change, so a reference to it converges to the function itself
    let env = context.env();
    let contents = matches!(value.kind(), HMIRValueKind::Reference(_))
        && !context.is_runtime()
        && !env.is_pointer(value.ty())
        && (!env.is_reference(value.ty()) || env.is_function(env.referent(value.ty())));
    let value = match contents {
        true => emit::storage::read(context, &value, span)?,
        false => value,
    };
    if let HMIRValueKind::Immediate(_) = value.kind() {
        return Ok(value);
    }
    let subject = match context.env().is_reference(value.ty()) {
        true => ("a reference to compile-time storage", "an exported value"),
        false => (
            "the address of compile-time storage",
            "the result of a compile-time expression",
        ),
    };
    let subject = (subject.0.into(), subject.1.into());
    Err(hmir_error(span, &typecheck::INVALID_CONTEXT, subject))
}

pub(crate) fn known(context: &mut LocalLoweringContext, value: &HMIRValue) -> Option<HMIRValue> {
    let span = TokenRange::internal();
    let value = match value.kind() {
        HMIRValueKind::Reference(_) if !context.is_runtime() => {
            emit::storage::read(context, value, &span).ok()?
        }
        _ => value.clone(),
    };
    converge(context, value, &span).ok()
}

pub(crate) fn inputs(context: &mut LocalLoweringContext) -> Vec<(HMIRLocalID, HMIRValue)> {
    context
        .visible_locals()
        .into_iter()
        .filter_map(|(local, value)| Some((local, known(context, &value)?)))
        .collect()
}
