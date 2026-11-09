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
    env::function::{
        ComptimeContext, FnContext, LocalLoweringContext, comptime::ComptimeSlotState,
    },
    eval::{
        EvalResult, coerce, deduce, eval,
        flow::{EvalExit, Evaluation, StopReason},
        name::type_value,
    },
    log::hmir_error,
    value::{self, HMIRReference, HMIRValue, HMIRValueKind, address::HMIRAddress},
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
        emit::bind_local(&mut child, local, value);
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
    value.and_then(context, body, None, move |context, _, value, _| {
        let value = match expected {
            Some(expected) => coerce::implicit::standard(context, value, expected, &span)?,
            None => coerce::promotion::std_rvalue(context, value, &span)?,
        };
        export(context, value, &span)
    })
}

pub(crate) fn export(
    context: &mut LocalLoweringContext,
    value: HMIRValue,
    span: &TokenRange,
) -> EvalResult<Evaluation<HMIRValue>> {
    let value = settle(context, value, span)?;
    match value.kind() {
        HMIRValueKind::Immediate(constant) if !constant.holds_local_address() => {
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
        HMIRValueKind::Immediate(_) => Ok(value),
        _ if context.env().is_reference(value.ty()) => Err(hmir_error(
            span,
            &typecheck::INVALID_CONTEXT,
            (
                "a reference to compile-time storage".into(),
                "an exported value".into(),
            ),
        )),
        HMIRValueKind::Reference(_) | HMIRValueKind::Indirect(_) => {
            let reference = emit::reference(context, &value, span)?;
            emit::copy(context, reference, span)
        }
    }
}
