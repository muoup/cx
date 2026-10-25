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
    env::function::{
        ComptimeContext, FnContext, LocalLoweringContext, comptime::ComptimeSlotState,
    },
    eval::{
        EvalResult, coerce, complete, eval,
        flow::{Evaluation, StopReason, ready},
    },
    log::hmir_error,
    value::{HMIRPlace, HMIRValue, HMIRValueKind, address::HMIRAddress},
};

pub(crate) fn eval_comptime(
    context: &mut LocalLoweringContext,
    body: &HMIRBody,
    expr: HMIRExprID,
    expected: Option<HMIRTypeID>,
) -> CXResult<HMIRValue> {
    match frozen(context, body, expr, expected)? {
        Evaluation::Ready(value) => Ok(value),
        Evaluation::Incomplete(incomplete) => Err(incomplete.error()),
        Evaluation::Stop(_) => unreachable!("a frozen evaluation diagnoses its stops"),
    }
}

pub(crate) fn frozen(
    context: &mut LocalLoweringContext,
    body: &HMIRBody,
    expr: HMIRExprID,
    expected: Option<HMIRTypeID>,
) -> EvalResult<HMIRValue> {
    let span = body.expr(expr).span();
    freeze(context, span, |child| {
        evaluate(child, body, expr, expected, span)
    })
}

pub(crate) fn type_expression(
    context: &mut LocalLoweringContext,
    body: &HMIRBody,
    expr: HMIRExprID,
) -> EvalResult<HMIRTypeID> {
    freeze(context, body.expr(expr).span(), |child| {
        super::eval_type(child, body, expr)
    })
}

fn freeze<T: 'static>(
    context: &mut LocalLoweringContext,
    span: &TokenRange,
    evaluate: impl FnOnce(&mut LocalLoweringContext) -> EvalResult<T>,
) -> EvalResult<T> {
    let mut child = frozen_child(context);
    match evaluate(&mut child)? {
        Evaluation::Incomplete(incomplete) => {
            let FnContext::Comptime(state) = child.into_context() else {
                unreachable!()
            };
            Ok(Evaluation::Incomplete(incomplete.in_context(FnContext::Comptime(state))))
        }
        Evaluation::Stop(StopReason::Comptime(_)) => {
            Err(hmir_error(span, &mir::COMPTIME_CONTROL_ESCAPE, ()))
        }
        Evaluation::Stop(StopReason::Runtime(stop)) => {
            unreachable!("runtime stop {stop:?} in a comptime evaluation")
        }
        result => Ok(result),
    }
}

pub(crate) fn frozen_child<'a, 'global, 'hmir>(
    context: &'a mut LocalLoweringContext<'_, 'global, 'hmir>,
) -> LocalLoweringContext<'a, 'global, 'hmir> {
    let inputs = frozen_inputs(context);
    let evaluation = context.evaluation();
    let mut child = context.child(
        None,
        FnContext::Comptime(ComptimeContext::new()),
        Some(evaluation),
    );
    for (local, value) in inputs {
        emit::storage::bind_local(&mut child, local, value);
    }
    child
}

pub(crate) fn frozen_inputs(context: &mut LocalLoweringContext) -> Vec<(HMIRLocalID, HMIRValue)> {
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
            let constant = context.load(&address, ty, &TokenRange::Internal).ok()?;
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
) -> EvalResult<HMIRValue> {
    let value = eval(context, body, expr)?;
    let describes_type = value.known().and_then(super::type_constant).is_some();
    let value = if describes_type {
        let ty = ready!(super::described_type(context, body, value, span)?.description());
        Evaluation::Ready(super::def::type_value(context, ty, span)?)
    } else {
        complete::value(context, body, value, expected)?
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
) -> EvalResult<HMIRValue> {
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
            emit::lift(context, &reference, None, span)
        }
    }
}
