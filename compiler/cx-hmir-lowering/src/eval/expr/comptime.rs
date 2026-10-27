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
    env::function::{ComptimeContext, FnContext, LocalLoweringContext, comptime::ComptimeSlotState},
    eval::{
        EvalResult, coerce, deduce, eval,
        flow::{EvalExit, Evaluation, StopReason},
        name::type_value,
    },
    log::hmir_error,
    value::{HMIRPlace, HMIRValue, HMIRValueKind, address::HMIRAddress},
};

// Evaluates an expression in a comptime child that sees only the known values of the visible locals
pub(crate) fn freeze(
    context: &mut LocalLoweringContext,
    body: &HMIRBody,
    expr: HMIRExprID,
    expected: Option<HMIRTypeID>,
) -> EvalResult<Evaluation<HMIRValue>> {
    let span = body.expr(expr).span();
    frozen(context, span, |child| evaluate(child, body, expr, expected, span))
}

pub(crate) fn frozen<T: 'static>(
    context: &mut LocalLoweringContext,
    span: &TokenRange,
    evaluate: impl FnOnce(&mut LocalLoweringContext) -> EvalResult<Evaluation<T>>,
) -> EvalResult<Evaluation<T>> {
    let mut child = child(context);
    match evaluate(&mut child) {
        Ok(Evaluation::Incomplete(incomplete)) => {
            Ok(Evaluation::Incomplete(incomplete.in_context(child.into_context())))
        }
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

pub(crate) fn inputs(context: &mut LocalLoweringContext) -> Vec<(HMIRLocalID, HMIRValue)> {
    context
        .visible_locals()
        .into_iter()
        .filter_map(|(local, value)| Some((local, known(context, &value)?)))
        .collect()
}

pub(crate) fn known(context: &mut LocalLoweringContext, value: &HMIRValue) -> Option<HMIRValue> {
    let value = match (value.kind(), context.context()) {
        (HMIRValueKind::Constant(_), _) => value.clone(),
        (HMIRValueKind::PlaceRef(HMIRPlace::Local(slot)), FnContext::Runtime(runtime)) => {
            runtime.slot(*slot).known().cloned()?
        }
        (
            HMIRValueKind::PlaceRef(place @ HMIRPlace::Region(region)),
            FnContext::Comptime(comptime),
        ) => {
            let slot = comptime.slot(*region)?;
            if slot.state() != ComptimeSlotState::Live {
                return None;
            }
            let ty = slot.ty();
            let address = HMIRAddress::place(*place);
            let constant = context.load(&address, ty, None, &TokenRange::Internal).ok()?;
            HMIRValue::constant(ty, constant)
        }
        _ => return None,
    };
    match value.kind() {
        HMIRValueKind::Constant(constant) if !constant.holds_local_address() => Some(value),
        _ => None,
    }
}

fn evaluate(
    context: &mut LocalLoweringContext,
    body: &HMIRBody,
    expr: HMIRExprID,
    expected: Option<HMIRTypeID>,
    span: &TokenRange,
) -> EvalResult<Evaluation<HMIRValue>> {
    let value = eval(context, body, expr)?;
    let value = match value.known().and_then(HMIRValue::as_type) {
        Some(_) => {
            let ty = (deduce::described_type(context, body, value, span)?.description())?.ready()?;
            Evaluation::Ready(type_value(context, ty, span)?)
        }
        None => deduce::complete(context, body, value, expected)?,
    };
    let span = span.clone();
    value.and_then(context, body, None, move |context, _, value, _| {
        let value = match expected {
            Some(expected) => coerce::implicit(context, value, expected, &span)?,
            None => coerce::std_rvalue(context, value, &span)?,
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
        ).into()),
        HMIRValueKind::PlaceRef(_) | HMIRValueKind::Indirect(_) => {
            let reference = emit::reference(context, &value, span)?;
            emit::lift(context, &reference, None, span)
        }
    }
}
