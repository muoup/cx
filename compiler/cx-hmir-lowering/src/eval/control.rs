use cx_hmir::{
    HMIRBinaryOp, HMIRBody,
    expr::HMIRExprID,
    ty::{HMIRTypeID, HMIRTypeKind, context::HMIRTypeContext, intrinsic::HMIRIntWidth},
};
use cx_log::catalogue::{mir, typecheck};
use cx_mir::{MIRBasicBlockID, MIRBlockTarget, MIRInstructionKind};
use cx_tokens::TokenRange;

use crate::{
    emit::{self, runtime},
    env::function::{FnContext, LocalLoweringContext, Permission, runtime::LoopFrame},
    eval::{
        EvalResult,
        binding::eval_scoped,
        coerce, eval, eval_type,
        flow::{self, ComptimeStop, Evaluation, RuntimeStop, StopReason, ready},
        literal, storage, types,
    },
    log::hmir_error,
    value::{HMIRValue, HMIRValueKind, constant::HMIRConstant},
};

fn comptime_truth(condition: &HMIRValue) -> bool {
    match condition.kind() {
        HMIRValueKind::Constant(HMIRConstant::Int { value, .. }) => *value != 0,
        _ => unreachable!("a comptime condition reads as an integer constant"),
    }
}

pub(crate) fn eval_if(
    context: &mut LocalLoweringContext,
    body: &HMIRBody,
    span: &TokenRange,
    condition: HMIRExprID,
    then_branch: HMIRExprID,
    else_branch: Option<HMIRExprID>,
) -> EvalResult<HMIRValue> {
    let condition = ready!(eval(context, body, condition));

    match context.context() {
        FnContext::Runtime(_) => {
            runtime_if(context, body, span, condition, then_branch, else_branch)
        }
        FnContext::Comptime(_) => {
            comptime_if(context, body, span, condition, then_branch, else_branch)
        }
    }
}

fn comptime_if(
    context: &mut LocalLoweringContext,
    body: &HMIRBody,
    span: &TokenRange,
    condition: HMIRValue,
    then_branch: HMIRExprID,
    else_branch: Option<HMIRExprID>,
) -> EvalResult<HMIRValue> {
    match (comptime_truth(&condition), else_branch) {
        (true, Some(_)) => eval_scoped(context, body, then_branch),
        (true, None) => {
            ready!(eval_scoped(context, body, then_branch));
            Ok(Evaluation::Ready(literal::unit(context, span)))
        }
        (false, Some(else_branch)) => eval_scoped(context, body, else_branch),
        (false, None) => Ok(Evaluation::Ready(literal::unit(context, span))),
    }
}

struct Arm {
    block: MIRBasicBlockID,
    value: HMIRValue,
}

fn runtime_if(
    context: &mut LocalLoweringContext,
    body: &HMIRBody,
    span: &TokenRange,
    condition: HMIRValue,
    then_branch: HMIRExprID,
    else_branch: Option<HMIRExprID>,
) -> EvalResult<HMIRValue> {
    let condition = runtime::lower_value(context, &condition, span)?;
    let then_block = runtime::block(context, "if.then");

    let Some(else_branch) = else_branch else {
        let merge = runtime::block(context, "if.merge");
        runtime::branch(
            context,
            condition,
            MIRBlockTarget::new(then_block),
            MIRBlockTarget::new(merge),
            span.clone(),
        );

        runtime::switch_to(context, then_block);
        ready!(flow::catch_diverged(eval_scoped(
            context,
            body,
            then_branch
        )));
        runtime::jump(context, merge, span.clone());

        runtime::switch_to(context, merge);
        return Ok(Evaluation::Ready(literal::unit(context, span)));
    };

    let else_block = runtime::block(context, "if.else");
    runtime::branch(
        context,
        condition,
        MIRBlockTarget::new(then_block),
        MIRBlockTarget::new(else_block),
        span.clone(),
    );

    runtime::switch_to(context, then_block);
    let then = flow::catch_diverged(eval_scoped(context, body, then_branch))?;
    let span = span.clone();
    then.and_then(context, body, None, move |context, body, then, expected| {
        let mut arms: Vec<Arm> = arm(context, then).into_iter().collect();
        runtime::switch_to(context, else_block);
        let otherwise = flow::catch_diverged(eval_scoped(context, body, else_branch))?
            .complete(context, body, expected)?;
        otherwise.and_then(context, body, None, move |context, _, otherwise, _| {
            arms.extend(arm(context, otherwise));
            merge(context, arms, &span)
        })
    })
}

fn arm(context: &mut LocalLoweringContext, value: Option<HMIRValue>) -> Option<Arm> {
    let block = runtime::current_block(context);
    value.map(|value| Arm { block, value })
}

fn merge(
    context: &mut LocalLoweringContext,
    arms: Vec<Arm>,
    span: &TokenRange,
) -> EvalResult<HMIRValue> {
    if arms.is_empty() {
        return Ok(Evaluation::Stop(RuntimeStop::Diverged.into()));
    }

    let merge = runtime::block(context, "if.merge");
    let Some(ty) = merged_type(context, &arms, span) else {
        for arm in arms {
            runtime::switch_to(context, arm.block);
            runtime::jump(context, merge, span.clone());
        }

        runtime::switch_to(context, merge);
        return Ok(Evaluation::Ready(literal::unit(context, span)));
    };

    let result = emit::scratch(context, ty, span)?;
    for arm in arms {
        runtime::switch_to(context, arm.block);
        let value = coerce::implicit(context, arm.value, ty, span)?;
        storage::initialize(context, &result, &value, span)?;
        runtime::jump(context, merge, span.clone());
    }

    runtime::switch_to(context, merge);
    Ok(Evaluation::Ready(result))
}

fn merged_type(
    context: &mut LocalLoweringContext,
    arms: &[Arm],
    span: &TokenRange,
) -> Option<HMIRTypeID> {
    let mut merged: Option<HMIRTypeID> = None;

    for arm in arms {
        let ty = arm.value.ty();
        if matches!(context.env().resolve_kind(ty), HMIRTypeKind::Void) {
            return None;
        }

        merged = Some(match merged {
            None => ty,
            Some(current) if context.env().same_type(current, ty) => current,
            Some(current) => coerce::common(context, current, ty, span).unwrap_or(current),
        });
    }

    merged
}

pub(crate) fn eval_for(
    context: &mut LocalLoweringContext,
    body: &HMIRBody,
    span: &TokenRange,
    init: HMIRExprID,
    condition: HMIRExprID,
    increment: HMIRExprID,
    loop_body: HMIRExprID,
) -> EvalResult<HMIRValue> {
    emit::open_scope(context, span.clone());
    let result = match eval(context, body, init) {
        Ok(Evaluation::Ready(_)) => eval_while(
            context,
            body,
            span,
            condition,
            loop_body,
            Some(increment),
            true,
        ),
        Ok(Evaluation::Incomplete(incomplete)) => Err(incomplete.error()),
        Ok(Evaluation::Stop(stop)) => Ok(Evaluation::Stop(stop)),
        Err(error) => Err(error),
    };
    emit::close_scope(context, span.clone());

    result
}

// 'increment' runs after each iteration of the body, and is where 'continue' leads
pub(crate) fn eval_while(
    context: &mut LocalLoweringContext,
    body: &HMIRBody,
    span: &TokenRange,
    condition: HMIRExprID,
    loop_body: HMIRExprID,
    increment: Option<HMIRExprID>,
    pre_eval: bool,
) -> EvalResult<HMIRValue> {
    match context.context() {
        FnContext::Runtime(_) => runtime_while(
            context, body, span, condition, loop_body, increment, pre_eval,
        ),
        FnContext::Comptime(_) => comptime_while(
            context, body, span, condition, loop_body, increment, pre_eval,
        ),
    }
}

fn comptime_while(
    context: &mut LocalLoweringContext,
    body: &HMIRBody,
    span: &TokenRange,
    condition: HMIRExprID,
    loop_body: HMIRExprID,
    increment: Option<HMIRExprID>,
    pre_eval: bool,
) -> EvalResult<HMIRValue> {
    let mut checks = pre_eval;

    loop {
        if checks {
            let condition = ready!(eval(context, body, condition));
            if !comptime_truth(&condition) {
                break;
            }
        }
        checks = true;

        match eval_scoped(context, body, loop_body) {
            Ok(Evaluation::Ready(_))
            | Ok(Evaluation::Stop(StopReason::Comptime(ComptimeStop::Continue))) => {}
            Ok(Evaluation::Stop(StopReason::Comptime(ComptimeStop::Break))) => break,
            Ok(Evaluation::Stop(stop)) => return Ok(Evaluation::Stop(stop)),
            Ok(Evaluation::Incomplete(incomplete)) => return Err(incomplete.error()),
            Err(error) => return Err(error),
        }
        if let Some(increment) = increment {
            ready!(eval(context, body, increment));
        }
    }

    Ok(Evaluation::Ready(literal::unit(context, span)))
}

fn runtime_while(
    context: &mut LocalLoweringContext,
    body: &HMIRBody,
    span: &TokenRange,
    condition: HMIRExprID,
    loop_body: HMIRExprID,
    increment: Option<HMIRExprID>,
    pre_eval: bool,
) -> EvalResult<HMIRValue> {
    let condition_block = runtime::block(context, "loop.condition");
    let body_block = runtime::block(context, "loop.body");
    let increment_block = increment.map(|_| runtime::block(context, "loop.increment"));
    let exit = runtime::block(context, "loop.exit");

    let entry = if pre_eval {
        condition_block
    } else {
        body_block
    };
    runtime::jump(context, entry, span.clone());

    runtime::switch_to(context, condition_block);
    let condition = ready!(flow::catch_diverged(eval(context, body, condition)));
    let Some(condition) = condition else {
        for block in [Some(body_block), increment_block, Some(exit)] {
            let Some(block) = block else { continue };

            runtime::switch_to(context, block);
            context
                .runtime_mut()
                .emit(MIRInstructionKind::Unreachable, span.clone());
        }

        return Ok(Evaluation::Stop(RuntimeStop::Diverged.into()));
    };

    let condition = runtime::lower_value(context, &condition, span)?;
    runtime::branch(
        context,
        condition,
        MIRBlockTarget::new(body_block),
        MIRBlockTarget::new(exit),
        span.clone(),
    );

    let continue_block = increment_block.unwrap_or(condition_block);
    let scope_depth = context.runtime().open_scopes().len();
    context.runtime_mut().push_loop(LoopFrame {
        continue_block,
        break_block: exit,
        scope_depth,
    });

    runtime::switch_to(context, body_block);
    let completion = flow::catch_diverged(eval_scoped(context, body, loop_body));
    context.runtime_mut().pop_loop();
    ready!(completion);
    runtime::jump(context, continue_block, span.clone());

    if let (Some(block), Some(increment)) = (increment_block, increment) {
        runtime::switch_to(context, block);
        ready!(flow::catch_diverged(eval(context, body, increment)));
        runtime::jump(context, condition_block, span.clone());
    }

    runtime::switch_to(context, exit);
    Ok(Evaluation::Ready(literal::unit(context, span)))
}

fn eval_logical_operand(
    context: &mut LocalLoweringContext,
    body: &HMIRBody,
    span: &TokenRange,
    operand: HMIRExprID,
) -> EvalResult<HMIRValue> {
    let value = ready!(eval(context, body, operand));

    Ok(Evaluation::Ready(coerce::condition(context, value, span)?))
}

pub(crate) fn eval_short_circuit(
    context: &mut LocalLoweringContext,
    body: &HMIRBody,
    span: &TokenRange,
    op: HMIRBinaryOp,
    lhs: HMIRExprID,
    rhs: HMIRExprID,
) -> EvalResult<HMIRValue> {
    let left = ready!(eval_logical_operand(context, body, span, lhs));

    match context.context() {
        FnContext::Runtime(_) => runtime_short_circuit(context, body, span, op, left, rhs),
        FnContext::Comptime(_) => {
            let truth = comptime_truth(&left);
            if truth == (op == HMIRBinaryOp::LOr) {
                return Ok(Evaluation::Ready(left));
            }

            eval_logical_operand(context, body, span, rhs)
        }
    }
}

fn runtime_short_circuit(
    context: &mut LocalLoweringContext,
    body: &HMIRBody,
    span: &TokenRange,
    op: HMIRBinaryOp,
    left: HMIRValue,
    rhs: HMIRExprID,
) -> EvalResult<HMIRValue> {
    let condition = runtime::lower_value(context, &left, span)?;

    let rhs_block = runtime::block(context, "logical.rhs");
    let short_block = runtime::block(context, "logical.short");
    let merge = runtime::block(context, "logical.merge");

    let rhs_target = MIRBlockTarget::new(rhs_block);
    let short_target = MIRBlockTarget::new(short_block);
    let (true_target, false_target) = match op {
        HMIRBinaryOp::LAnd => (rhs_target, short_target),
        _ => (short_target, rhs_target),
    };
    runtime::branch(context, condition, true_target, false_target, span.clone());

    runtime::switch_to(context, rhs_block);
    let right = ready!(flow::catch_diverged(eval_logical_operand(
        context, body, span, rhs
    )));

    let ty = types::int(context.env_mut(), HMIRIntWidth::I1, false, span);
    let result = emit::scratch(context, ty, span)?;
    if let Some(right) = right {
        storage::initialize(context, &result, &right, span)?;
        runtime::jump(context, merge, span.clone());
    }

    runtime::switch_to(context, short_block);
    storage::initialize(context, &result, &left, span)?;
    runtime::jump(context, merge, span.clone());

    runtime::switch_to(context, merge);
    Ok(Evaluation::Ready(result))
}

pub(crate) fn eval_loop_exit(
    context: &mut LocalLoweringContext,
    span: &TokenRange,
    stop: ComptimeStop,
) -> EvalResult<HMIRValue> {
    if matches!(context.context(), FnContext::Comptime(_)) {
        return Ok(Evaluation::Stop(stop.into()));
    }

    let Some(frame) = context.runtime().current_loop() else {
        let exit = "'break' or 'continue'".into();
        return Err(hmir_error(span, &typecheck::REQUIRED_CONTEXT, (exit, "a loop".into())).into());
    };
    let target = match stop {
        ComptimeStop::Break => frame.break_block,
        ComptimeStop::Continue => frame.continue_block,
        _ => unreachable!("only 'break' and 'continue' exit a loop"),
    };

    runtime::drop_scopes(context, frame.scope_depth, span);
    runtime::jump(context, target, span.clone());
    Ok(Evaluation::Stop(RuntimeStop::Diverged.into()))
}

pub(crate) fn eval_unsafe(
    context: &mut LocalLoweringContext,
    body: &HMIRBody,
    operand: HMIRExprID,
) -> EvalResult<HMIRValue> {
    flow::with_permission(context, Permission::Unsafe, |context| {
        eval(context, body, operand)
    })
}

pub(crate) fn eval_return(
    context: &mut LocalLoweringContext,
    body: &HMIRBody,
    span: &TokenRange,
    value: Option<HMIRExprID>,
) -> EvalResult<HMIRValue> {
    let value = value.map(|value| eval(context, body, value)).transpose()?;

    return_value(context, body, value, true, span)
}

// Always stops evaluation: a runtime return diverges, and a comptime return carries its value to the caller
pub(crate) fn return_value(
    context: &mut LocalLoweringContext,
    body: &HMIRBody,
    value: Option<Evaluation<HMIRValue>>,
    explicit: bool,
    span: &TokenRange,
) -> EvalResult<HMIRValue> {
    let Some(return_type) = context.signature().map(|sig| *sig.return_type()) else {
        return Err(hmir_error(span, &mir::COMPTIME_CONTROL_ESCAPE, ()).into());
    };
    let ty = match context.return_type() {
        Some(ty) => ty,
        None => ready!(eval_type(context, body, return_type)),
    };
    let returns_void = match context.env().resolve_kind(ty) {
        HMIRTypeKind::Unreachable => {
            return Err(hmir_error(span, &typecheck::NORETURN_RETURN, ()).into());
        }
        HMIRTypeKind::Void => true,
        _ => false,
    };

    let value = match value {
        Some(_) if returns_void && explicit => {
            return Err(hmir_error(span, &typecheck::VOID_RETURN_VALUE, ()).into());
        }
        Some(value) if returns_void => match value {
            Evaluation::Ready(value)
                if matches!(context.env().resolve_kind(value.ty()), HMIRTypeKind::Void) =>
            {
                None
            }
            Evaluation::Stop(stop) => return Ok(Evaluation::Stop(stop)),
            Evaluation::Ready(_) | Evaluation::Incomplete(_) => {
                return Err(hmir_error(span, &typecheck::VOID_RETURN_VALUE, ()).into());
            }
        },
        Some(value) => {
            let value = ready!(value.complete(context, body, Some(ty)));
            Some(coerce::implicit(context, value, ty, span)?)
        }
        None if returns_void => None,
        None => return Err(hmir_error(span, &typecheck::MISSING_RETURN_VALUE, ()).into()),
    };

    match context.context() {
        FnContext::Runtime(_) => {
            let value = match &value {
                Some(value) => Some(runtime::lower_value(context, value, span)?),
                None => None,
            };

            runtime::drop_scopes(context, 0, span);
            context
                .runtime_mut()
                .emit(MIRInstructionKind::Return { value }, span.clone());
            Ok(Evaluation::Stop(RuntimeStop::Diverged.into()))
        }
        FnContext::Comptime(_) => {
            let value = value.unwrap_or_else(|| literal::unit(context, span).with_ty(ty));
            Ok(Evaluation::Stop(ComptimeStop::Return(value).into()))
        }
    }
}

pub(crate) fn fallthrough(
    context: &mut LocalLoweringContext,
    body: &HMIRBody,
    is_main: bool,
    span: &TokenRange,
) -> EvalResult<HMIRValue> {
    let Some(return_type) = context.signature().map(|sig| *sig.return_type()) else {
        unreachable!("fallthrough in a context without a signature")
    };
    let ty = match context.return_type() {
        Some(ty) => ty,
        None => ready!(eval_type(context, body, return_type)),
    };

    match context.env().resolve_kind(ty) {
        HMIRTypeKind::Void => return_value(context, body, None, false, span),
        HMIRTypeKind::Int(_) if is_main => {
            let zero = HMIRValue::constant(ty, HMIRConstant::Int { value: 0, ty });
            return_value(context, body, Some(Evaluation::Ready(zero)), false, span)
        }
        _ => {
            if !matches!(context.env().resolve_kind(ty), HMIRTypeKind::Unreachable)
                && context.env().require_explicit_return()
                && context.runtime().builder().current_block_reachable()
            {
                let name = context.runtime().builder().prototype().display_name().to_string();
                return Err(hmir_error(span, &mir::FUNCTION_RETURN, name).into());
            }
            context
                .runtime_mut()
                .emit(MIRInstructionKind::Unreachable, span.clone());
            Ok(Evaluation::Stop(RuntimeStop::Diverged.into()))
        }
    }
}
