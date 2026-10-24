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
        EvalResult, coerce, eval,
        flow::{Evaluation, StopReason, ready},
    },
    log::hmir_error,
    value::{HMIRPlace, HMIRValue, HMIRValueKind},
};

pub(crate) fn eval_comptime(
    context: &mut LocalLoweringContext,
    body: &HMIRBody,
    expr: HMIRExprID,
    expected: Option<HMIRTypeID>,
) -> CXResult<HMIRValue> {
    let span = body.expr(expr).span();
    let mut child = frozen_child(context);

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
    let mut child = frozen_child(context);
    match super::eval_type(&mut child, body, expr)? {
        Evaluation::Incomplete(incomplete) => {
            let FnContext::Comptime(state) = child.into_context() else {
                unreachable!()
            };
            Ok(Evaluation::Incomplete(incomplete.in_comptime(state)))
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

pub(crate) fn frozen_inputs(context: &LocalLoweringContext) -> Vec<(HMIRLocalID, HMIRValue)> {
    let frozen = |(local, value): (&HMIRLocalID, &HMIRValue)| match value.kind() {
        HMIRValueKind::Constant(constant) if !constant.holds_local_address() => {
            Some((*local, value.clone()))
        }
        _ => None,
    };

    match context.context() {
        FnContext::Runtime(runtime) => runtime.locals().filter_map(frozen).collect(),
        FnContext::Comptime(comptime) => comptime
            .locals()
            .filter_map(|(local, value)| {
                if let HMIRValueKind::PlaceRef(HMIRPlace::Local(slot)) = value.kind() {
                    let slot = comptime.slot(*slot);
                    if let ComptimeSlotState::Value(constant) = slot.state()
                        && !constant.holds_local_address()
                    {
                        return Some((*local, HMIRValue::constant(slot.ty(), constant.clone())));
                    }
                }
                frozen((local, value))
            })
            .collect(),
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
        super::def::type_value(context, ty, span)?
    } else {
        ready!(value.complete(context, body, expected))
    };
    let value = match expected {
        Some(expected) => coerce::implicit(context, value, expected, span)?,
        None => coerce::std_rvalue(context, value, span)?,
    };
    export(context, value, span)
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
            emit::lift(context, &reference, span)
        }
    }
}
