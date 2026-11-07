use cx_hmir::{
    HMIRBodyContext,
    expr::HMIRExprID,
    ty::{HMIRTypeID, HMIRTypeKind, context::HMIRTypeContext},
};
use cx_log::catalogue::{self, typecheck};
use cx_mir::MIRInstructionKind;
use cx_tokens::TokenRange;

use super::defer;
use crate::{
    env::function::{
        FnContext, LocalLoweringContext,
        runtime::{Emission, Exit},
    },
    eval::{
        EvalResult,
        call::contract,
        coerce, eval, eval_type, expr,
        flow::{self, ComptimeStop, EvalExit, Evaluation},
    },
    log::hmir_error,
    lower::{self, expr::value},
    value::{HMIRValue, immediate::HMIRImmediate},
};

pub(super) fn eval_return(
    context: &mut LocalLoweringContext,
    body: &HMIRBodyContext,
    span: &TokenRange,
    value: Option<HMIRExprID>,
) -> EvalResult<Evaluation<HMIRValue>> {
    if deferred(context) {
        return Err(hmir_error(span, &typecheck::DEFER_JUMP, "return".into()).into());
    }
    if context.is_unevaluated() {
        return flow::diverged();
    }
    let value = value.map(|value| eval(context, body, value)).transpose()?;
    return_value(context, body, value, true, span)
}

pub(super) fn eval_yield(
    context: &mut LocalLoweringContext,
    body: &HMIRBodyContext,
    span: &TokenRange,
    value: Option<HMIRExprID>,
) -> EvalResult<Evaluation<HMIRValue>> {
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
        let value = coerce::promotion::std_rvalue(context, value, &span)?;
        if !context.is_runtime() {
            return Err(EvalExit::Stop(ComptimeStop::Yield(value).into()));
        }
        let depth = context
            .runtime()
            .yield_target(origin)
            .expect("yield has a target");
        defer::leave(context, depth + 1, &span)?;
        let runtime = context.runtime_mut();
        runtime.yield_value(depth, value);
        runtime.set_emission(Emission::Suppressed);
        flow::diverged()
    })
}

pub(super) fn eval_loop_exit(
    context: &mut LocalLoweringContext,
    span: &TokenRange,
    stop: ComptimeStop,
) -> EvalResult<Evaluation<HMIRValue>> {
    if !context.is_runtime() {
        return Err(EvalExit::Stop(stop.into()));
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
    lower::expr::jump(context, target, span);
    flow::diverged()
}

fn deferred(context: &LocalLoweringContext) -> bool {
    matches!(context.context(), FnContext::Runtime(runtime) if runtime.deferred())
}

// A dry run has no targets to leave to, so an exit only marks its code as diverging
fn untargeted(
    context: &LocalLoweringContext,
    span: &TokenRange,
    jump: &str,
    required: &str,
) -> EvalResult<Evaluation<HMIRValue>> {
    if context.is_unevaluated() {
        return flow::diverged();
    }
    if deferred(context) {
        return Err(hmir_error(span, &typecheck::DEFER_JUMP, jump.into()).into());
    }
    Err(hmir_error(
        span,
        &typecheck::REQUIRED_CONTEXT,
        (format!("'{jump}'"), required.into()),
    )
    .into())
}

fn return_type(
    context: &mut LocalLoweringContext,
    body: &HMIRBodyContext,
    span: &TokenRange,
) -> EvalResult<Evaluation<HMIRTypeID>> {
    let Some(signature) = context.signature() else {
        return Err(hmir_error(span, &catalogue::mir::COMPTIME_CONTROL_ESCAPE, ()).into());
    };
    match context.return_type() {
        Some(ty) => Ok(Evaluation::Ready(ty)),
        None => eval_type(context, body, *signature.return_type()),
    }
}

// Always stops evaluation: a runtime return diverges, and a comptime return carries its value to the caller
pub(crate) fn return_value(
    context: &mut LocalLoweringContext,
    body: &HMIRBodyContext,
    value: Option<Evaluation<HMIRValue>>,
    explicit: bool,
    span: &TokenRange,
) -> EvalResult<Evaluation<HMIRValue>> {
    let ty = return_type(context, body, span)?.ready()?;
    if matches!(context.env().resolve_kind(ty), HMIRTypeKind::Unreachable) {
        return Err(hmir_error(span, &typecheck::NORETURN_RETURN, ()).into());
    }
    let value = match (value, context.env().is_void(ty)) {
        (Some(_), true) if explicit => {
            return Err(hmir_error(span, &typecheck::VOID_RETURN_VALUE, ()).into());
        }
        (Some(Evaluation::Ready(value)), true) if context.env().is_void(value.ty()) => None,
        (Some(_), true) => return Err(hmir_error(span, &typecheck::VOID_RETURN_VALUE, ()).into()),
        (None, true) => None,
        (None, false) => return Err(hmir_error(span, &typecheck::MISSING_RETURN_VALUE, ()).into()),
        (Some(value), false) => {
            let value = value.complete(context, body, Some(ty))?;
            Some(coerce::implicit::standard(context, value, ty, span)?)
        }
    };

    if !context.is_runtime() {
        let value = value.unwrap_or_else(|| expr::unit(context, span).with_ty(ty));
        return Err(EvalExit::Stop(ComptimeStop::Return(value).into()));
    }
    contract::exit(context, value.as_ref(), span)?;
    defer::leave(context, 0, span)?;
    let value = value
        .map(|value| value::standard(context, &value, span))
        .transpose()?;
    context
        .runtime_mut()
        .emit(MIRInstructionKind::Return { value }, span.clone());
    flow::diverged()
}

pub(crate) fn fallthrough(
    context: &mut LocalLoweringContext,
    body: &HMIRBodyContext,
    is_main: bool,
    span: &TokenRange,
) -> EvalResult<Evaluation<HMIRValue>> {
    let ty = return_type(context, body, span)?.ready()?;
    match context.env().resolve_kind(ty) {
        HMIRTypeKind::Void => return_value(context, body, None, false, span),
        HMIRTypeKind::Int(_) if is_main => {
            let zero = HMIRValue::constant(ty, HMIRImmediate::Int(0));
            return_value(context, body, Some(Evaluation::Ready(zero)), false, span)
        }
        kind if !matches!(kind, HMIRTypeKind::Unreachable)
            && context.env().settings().require_explicit_return()
            && context.runtime().builder().current_block_reachable() =>
        {
            let name = context
                .runtime()
                .builder()
                .prototype()
                .display_name()
                .to_string();
            Err(hmir_error(span, &catalogue::mir::FUNCTION_RETURN, name).into())
        }
        _ => {
            context
                .runtime_mut()
                .emit(MIRInstructionKind::Unreachable, span.clone());
            flow::diverged()
        }
    }
}
