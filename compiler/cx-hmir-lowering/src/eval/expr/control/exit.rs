use cx_hmir::{
    HMIRBody,
    expr::HMIRExprID,
    ty::{HMIRTypeID, HMIRTypeKind, context::HMIRTypeContext},
};
use cx_log::catalogue::{self, typecheck};
use cx_mir::MIRInstructionKind;
use cx_tokens::TokenRange;

use super::defer;
use crate::{
    emit,
    env::function::{
        FnContext, LocalLoweringContext,
        runtime::{Emission, Exit},
    },
    eval::{
        EvalResult,
        call::contract,
        coerce, eval, eval_type, expr,
        flow::{ComptimeStop, Evaluation, ready},
    },
    log::hmir_error,
    lower::mir,
    value::{HMIRValue, HMIRValueKind, constant::HMIRConstant},
};

pub(super) fn eval_return(
    context: &mut LocalLoweringContext,
    body: &HMIRBody,
    span: &TokenRange,
    value: Option<HMIRExprID>,
) -> EvalResult<HMIRValue> {
    if deferred(context) {
        return Err(hmir_error(span, &typecheck::DEFER_JUMP, "return".into()));
    }
    if context.is_unevaluated() {
        return Ok(Evaluation::diverged());
    }
    let value = value.map(|value| eval(context, body, value)).transpose()?;
    return_value(context, body, value, true, span)
}

pub(super) fn eval_yield(
    context: &mut LocalLoweringContext,
    body: &HMIRBody,
    span: &TokenRange,
    value: Option<HMIRExprID>,
) -> EvalResult<HMIRValue> {
    let available = match context.context() {
        FnContext::Runtime(runtime) => runtime.yield_target(body.id()).is_some(),
        FnContext::Comptime(comptime) => comptime.yielding(),
    };
    if !available {
        return untargeted(context, span, "yield", "a yielding expression");
    }
    let value = match value {
        Some(value) => match eval(context, body, value)? {
            Evaluation::Incomplete(pending) => Evaluation::Incomplete(pending.yielding()),
            value => value,
        },
        None => Evaluation::Ready(expr::unit(context, span)),
    };
    let (span, origin) = (span.clone(), body.id());
    value.and_then(context, body, None, move |context, _, value, _| {
        let value = coerce::std_rvalue(context, value, &span)?;
        if !context.is_runtime() {
            return Ok(Evaluation::Stop(ComptimeStop::Yield(value).into()));
        }
        let value = if context.env().is_void(value.ty()) { value } else { emit::temporary(context, value, &span)? };
        let depth = context.runtime().yield_target(origin).expect("yield has a target");
        defer::leave(context, depth + 1, &span)?;
        let runtime = context.runtime_mut();
        runtime.yield_value(depth, value);
        runtime.set_emission(Emission::Suppressed);
        Ok(Evaluation::diverged())
    })
}

pub(super) fn eval_loop_exit(
    context: &mut LocalLoweringContext,
    span: &TokenRange,
    stop: ComptimeStop,
) -> EvalResult<HMIRValue> {
    if !context.is_runtime() {
        return Ok(Evaluation::Stop(stop.into()));
    }
    let breaks = matches!(stop, ComptimeStop::Break);
    let exit = context.runtime().target(|scope| match breaks {
        true => scope.exit().map(Exit::break_block),
        false => scope.exit().and_then(Exit::continue_block),
    });
    let Some((depth, target)) = exit else {
        return match breaks {
            true => untargeted(context, span, "break", "a loop or switch"),
            false => untargeted(context, span, "continue", "a loop"),
        };
    };
    defer::leave(context, depth, span)?;
    mir::jump(context, target, span);
    Ok(Evaluation::diverged())
}

fn deferred(context: &LocalLoweringContext) -> bool {
    matches!(context.context(), FnContext::Runtime(runtime) if runtime.deferred())
}

// A dry run has no targets to leave to, so an exit only marks its code as diverging
fn untargeted(context: &LocalLoweringContext, span: &TokenRange, jump: &str, required: &str) -> EvalResult<HMIRValue> {
    if context.is_unevaluated() {
        return Ok(Evaluation::diverged());
    }
    if deferred(context) {
        return Err(hmir_error(span, &typecheck::DEFER_JUMP, jump.into()));
    }
    Err(hmir_error(span, &typecheck::REQUIRED_CONTEXT, (format!("'{jump}'"), required.into())))
}

fn return_type(context: &mut LocalLoweringContext, body: &HMIRBody, span: &TokenRange) -> EvalResult<HMIRTypeID> {
    let Some(signature) = context.signature() else {
        return Err(hmir_error(span, &catalogue::mir::COMPTIME_CONTROL_ESCAPE, ()));
    };
    match context.return_type() {
        Some(ty) => Ok(Evaluation::Ready(ty)),
        None => eval_type(context, body, *signature.return_type()),
    }
}

// Always stops evaluation: a runtime return diverges, and a comptime return carries its value to the caller
pub(crate) fn return_value(
    context: &mut LocalLoweringContext,
    body: &HMIRBody,
    value: Option<Evaluation<HMIRValue>>,
    explicit: bool,
    span: &TokenRange,
) -> EvalResult<HMIRValue> {
    let ty = ready!(return_type(context, body, span));
    if matches!(context.env().resolve_kind(ty), HMIRTypeKind::Unreachable) {
        return Err(hmir_error(span, &typecheck::NORETURN_RETURN, ()));
    }
    let value = match (value, context.env().is_void(ty)) {
        (Some(_), true) if explicit => return Err(hmir_error(span, &typecheck::VOID_RETURN_VALUE, ())),
        (Some(Evaluation::Ready(value)), true) if context.env().is_void(value.ty()) => None,
        (Some(Evaluation::Stop(stop)), true) => return Ok(Evaluation::Stop(stop)),
        (Some(_), true) => return Err(hmir_error(span, &typecheck::VOID_RETURN_VALUE, ())),
        (None, true) => None,
        (None, false) => return Err(hmir_error(span, &typecheck::MISSING_RETURN_VALUE, ())),
        (Some(value), false) => {
            let value = ready!(value.complete(context, body, Some(ty)));
            Some(coerce::implicit(context, value, ty, span)?)
        }
    };

    if !context.is_runtime() {
        let value = value.unwrap_or_else(|| expr::unit(context, span).with_ty(ty));
        return Ok(Evaluation::Stop(ComptimeStop::Return(value).into()));
    }
    contract::exit(context, value.as_ref(), span)?;
    let value = match value {
        Some(value)
            if context.runtime().defers_past(0)
                && !context.env().is_reference(value.ty())
                && !matches!(value.kind(), HMIRValueKind::Constant(_)) =>
        {
            Some(emit::temporary(context, value, span)?)
        }
        value => value,
    };
    defer::leave(context, 0, span)?;
    let value = value.map(|value| mir::operand(context, &value, span)).transpose()?;
    context.runtime_mut().emit(MIRInstructionKind::Return { value }, span.clone());
    Ok(Evaluation::diverged())
}

pub(crate) fn fallthrough(
    context: &mut LocalLoweringContext,
    body: &HMIRBody,
    is_main: bool,
    span: &TokenRange,
) -> EvalResult<HMIRValue> {
    let ty = ready!(return_type(context, body, span));
    match context.env().resolve_kind(ty) {
        HMIRTypeKind::Void => return_value(context, body, None, false, span),
        HMIRTypeKind::Int(_) if is_main => {
            let zero = HMIRValue::constant(ty, HMIRConstant::Int(0));
            return_value(context, body, Some(Evaluation::Ready(zero)), false, span)
        }
        kind if !matches!(kind, HMIRTypeKind::Unreachable)
            && context.env().settings().require_explicit_return()
            && context.runtime().builder().current_block_reachable() =>
        {
            let name = context.runtime().builder().prototype().display_name().to_string();
            Err(hmir_error(span, &catalogue::mir::FUNCTION_RETURN, name))
        }
        _ => {
            context.runtime_mut().emit(MIRInstructionKind::Unreachable, span.clone());
            Ok(Evaluation::diverged())
        }
    }
}
