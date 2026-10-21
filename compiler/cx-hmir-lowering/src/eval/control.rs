use cx_hmir::{
    HMIRBinaryOp, HMIRBody,
    expr::HMIRExprID,
    ty::{HMIRTypeID, HMIRTypeKind, context::HMIRTypeContext, intrinsic::HMIRIntWidth},
};
use cx_log::catalogue::typecheck;
use cx_mir::{MIRBasicBlockID, MIRBlockTarget, MIRInstructionKind};
use cx_tokens::TokenRange;

use crate::{
    emit::{self, runtime},
    env::function::{FnContext, LocalLoweringContext, runtime::LoopFrame},
    eval::{
        EvalResult,
        binding::eval_scoped,
        coerce,
        complete::{Completion, elaborate},
        eval, eval_type,
        flow::{self, ComptimeStop, EvalStop, RuntimeStop},
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
    let condition = eval(context, body, condition)?;

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
            eval_scoped(context, body, then_branch)?;
            Ok(literal::unit(context, span))
        }
        (false, Some(else_branch)) => eval_scoped(context, body, else_branch),
        (false, None) => Ok(literal::unit(context, span)),
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
        flow::catch_diverged(eval_scoped(context, body, then_branch))?;
        runtime::jump(context, merge, span.clone());

        runtime::switch_to(context, merge);
        return Ok(literal::unit(context, span));
    };

    let else_block = runtime::block(context, "if.else");
    runtime::branch(
        context,
        condition,
        MIRBlockTarget::new(then_block),
        MIRBlockTarget::new(else_block),
        span.clone(),
    );

    let mut arms = Vec::new();
    for (block, branch) in [(then_block, then_branch), (else_block, else_branch)] {
        runtime::switch_to(context, block);
        if let Some(value) = flow::catch_diverged(eval_scoped(context, body, branch))? {
            let block = runtime::current_block(context);
            arms.push(Arm { block, value });
        }
    }

    if arms.is_empty() {
        return Err(RuntimeStop::Diverged.into());
    }

    let merge = runtime::block(context, "if.merge");
    let Some(ty) = merged_type(context, &arms, span) else {
        for arm in arms {
            runtime::switch_to(context, arm.block);
            runtime::jump(context, merge, span.clone());
        }

        runtime::switch_to(context, merge);
        return Ok(literal::unit(context, span));
    };

    let result = emit::scratch(context, ty, span)?;
    for arm in arms {
        runtime::switch_to(context, arm.block);
        let value = coerce::implicit(context, arm.value, ty, span)?;
        storage::initialize(context, &result, &value, span)?;
        runtime::jump(context, merge, span.clone());
    }

    runtime::switch_to(context, merge);
    Ok(result)
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
    let result = eval(context, body, init).and_then(|_| {
        let increment = Some(increment);
        eval_while(context, body, span, condition, loop_body, increment, true)
    });
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
            let condition = eval(context, body, condition)?;
            if !comptime_truth(&condition) {
                break;
            }
        }
        checks = true;

        match eval_scoped(context, body, loop_body) {
            Ok(_) | Err(EvalStop::Comptime(ComptimeStop::Continue)) => {}
            Err(EvalStop::Comptime(ComptimeStop::Break)) => break,
            Err(stop) => return Err(stop),
        }
        if let Some(increment) = increment {
            eval(context, body, increment)?;
        }
    }

    Ok(literal::unit(context, span))
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
    let condition = flow::catch_diverged(eval(context, body, condition))?;
    let Some(condition) = condition else {
        for block in [Some(body_block), increment_block, Some(exit)] {
            let Some(block) = block else { continue };

            runtime::switch_to(context, block);
            context
                .runtime_mut()
                .emit(MIRInstructionKind::Unreachable, span.clone());
        }

        return Err(RuntimeStop::Diverged.into());
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
    completion?;
    runtime::jump(context, continue_block, span.clone());

    if let (Some(block), Some(increment)) = (increment_block, increment) {
        runtime::switch_to(context, block);
        flow::catch_diverged(eval(context, body, increment))?;
        runtime::jump(context, condition_block, span.clone());
    }

    runtime::switch_to(context, exit);
    Ok(literal::unit(context, span))
}

fn eval_logical_operand(
    context: &mut LocalLoweringContext,
    body: &HMIRBody,
    span: &TokenRange,
    operand: HMIRExprID,
) -> EvalResult<HMIRValue> {
    let value = eval(context, body, operand)?;

    Ok(coerce::condition(context, value, span)?)
}

pub(crate) fn eval_short_circuit(
    context: &mut LocalLoweringContext,
    body: &HMIRBody,
    span: &TokenRange,
    op: HMIRBinaryOp,
    lhs: HMIRExprID,
    rhs: HMIRExprID,
) -> EvalResult<HMIRValue> {
    let left = eval_logical_operand(context, body, span, lhs)?;

    match context.context() {
        FnContext::Runtime(_) => runtime_short_circuit(context, body, span, op, left, rhs),
        FnContext::Comptime(_) => {
            let truth = comptime_truth(&left);
            if truth == (op == HMIRBinaryOp::LOr) {
                return Ok(left);
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
    let right = flow::catch_diverged(eval_logical_operand(context, body, span, rhs))?;

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
    Ok(result)
}

pub(crate) fn eval_loop_exit(
    context: &mut LocalLoweringContext,
    span: &TokenRange,
    stop: ComptimeStop,
) -> EvalResult<HMIRValue> {
    if matches!(context.context(), FnContext::Comptime(_)) {
        return Err(stop.into());
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
    Err(RuntimeStop::Diverged.into())
}

pub(crate) fn eval_return(
    context: &mut LocalLoweringContext,
    body: &HMIRBody,
    span: &TokenRange,
    value: Option<HMIRExprID>,
) -> EvalResult<HMIRValue> {
    let value = match value {
        Some(value) => Some(elaborate(context, body, value)?),
        None => None,
    };

    return_value(context, body, value, span)
}

// Always stops evaluation: a runtime return diverges, and a comptime return carries its value to the caller
pub(crate) fn return_value(
    context: &mut LocalLoweringContext,
    body: &HMIRBody,
    value: Option<Completion>,
    span: &TokenRange,
) -> EvalResult<HMIRValue> {
    let Some(return_type) = context.signature().map(|sig| *sig.return_type()) else {
        unreachable!("return statement in a context without a signature")
    };
    let ty = eval_type(context, body, return_type)?;
    let returns_void = match context.env().resolve_kind(ty) {
        HMIRTypeKind::Unreachable => {
            return Err(hmir_error(span, &typecheck::NORETURN_RETURN, ()).into());
        }
        HMIRTypeKind::Void => true,
        _ => false,
    };

    let value = match value {
        Some(value) if returns_void => {
            let Completion::Ready(value) = value else {
                return Err(hmir_error(span, &typecheck::VOID_RETURN_VALUE, ()).into());
            };
            if !matches!(context.env().resolve_kind(value.ty()), HMIRTypeKind::Void) {
                return Err(hmir_error(span, &typecheck::VOID_RETURN_VALUE, ()).into());
            }

            None
        }
        Some(value) => {
            let value = value.complete(context, body, Some(ty))?;
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
            Err(RuntimeStop::Diverged.into())
        }
        FnContext::Comptime(_) => {
            let value = value.unwrap_or_else(|| literal::unit(context, span));
            Err(ComptimeStop::Return(value).into())
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
    let ty = eval_type(context, body, return_type)?;

    match context.env().resolve_kind(ty) {
        HMIRTypeKind::Void => return_value(context, body, None, span),
        HMIRTypeKind::Int(_) if is_main => {
            let zero = HMIRValue::constant(ty, HMIRConstant::Int { value: 0, ty });
            return_value(context, body, Some(Completion::Ready(zero)), span)
        }
        _ => {
            context
                .runtime_mut()
                .emit(MIRInstructionKind::Unreachable, span.clone());
            Err(RuntimeStop::Diverged.into())
        }
    }
}
