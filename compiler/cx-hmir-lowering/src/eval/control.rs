use cx_hmir::{
    HMIRBinaryOp, HMIRBody,
    expr::{HMIRExprID, constant::HMIRConstant},
    ty::{HMIRTypeID, HMIRTypeKind, context::HMIRTypeContext, intrinsic::HMIRIntWidth},
    value::{HMIRValue, HMIRValueKind},
};
use cx_log::{CXResult, catalogue::typecheck};
use cx_mir::{MIRBasicBlockID, MIRBlockTarget, MIRInstructionKind};
use cx_tokens::TokenRange;

use crate::{
    emit::{self, runtime},
    env::function::{FnContext, FnLoweringContext},
    eval::{
        EvalResult,
        binding::eval_scoped,
        coerce::{convert, truthy},
        constant, eval, eval_type,
        flow::{self, ComptimeStop, EvalStop, RuntimeStop},
        promote, types,
    },
    log::hmir_error,
};

fn eval_condition(
    context: &mut FnLoweringContext,
    body: &HMIRBody,
    span: &TokenRange,
    condition: HMIRExprID,
) -> EvalResult<HMIRValue> {
    let value = eval(context, body, condition)?;

    Ok(truthy(context, value, span)?)
}

fn comptime_truth(
    context: &mut FnLoweringContext,
    condition: HMIRValue,
    span: &TokenRange,
) -> CXResult<bool> {
    let condition = emit::read(context, condition, span)?;

    match condition.kind() {
        HMIRValueKind::Constant(HMIRConstant::Int { value, .. }) => Ok(*value != 0),
        _ => unreachable!("a comptime condition reads as an integer constant"),
    }
}

pub(crate) fn eval_if(
    context: &mut FnLoweringContext,
    body: &HMIRBody,
    span: &TokenRange,
    condition: HMIRExprID,
    then_branch: HMIRExprID,
    else_branch: Option<HMIRExprID>,
) -> EvalResult<HMIRValue> {
    let condition = eval_condition(context, body, span, condition)?;

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
    context: &mut FnLoweringContext,
    body: &HMIRBody,
    span: &TokenRange,
    condition: HMIRValue,
    then_branch: HMIRExprID,
    else_branch: Option<HMIRExprID>,
) -> EvalResult<HMIRValue> {
    match (comptime_truth(context, condition, span)?, else_branch) {
        (true, Some(_)) => eval_scoped(context, body, then_branch),
        (true, None) => {
            eval_scoped(context, body, then_branch)?;
            Ok(constant::unit(context, span))
        }
        (false, Some(else_branch)) => eval_scoped(context, body, else_branch),
        (false, None) => Ok(constant::unit(context, span)),
    }
}

struct Arm {
    block: MIRBasicBlockID,
    value: HMIRValue,
}

fn runtime_if(
    context: &mut FnLoweringContext,
    body: &HMIRBody,
    span: &TokenRange,
    condition: HMIRValue,
    then_branch: HMIRExprID,
    else_branch: Option<HMIRExprID>,
) -> EvalResult<HMIRValue> {
    let condition = runtime::materialize(context, &condition, span)?;
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
        runtime::jump(context, merge, Vec::new(), span.clone());

        runtime::switch_to(context, merge);
        return Ok(constant::unit(context, span));
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
            runtime::jump(context, merge, Vec::new(), span.clone());
        }

        runtime::switch_to(context, merge);
        return Ok(constant::unit(context, span));
    };

    let result = runtime::block_param(context, merge, ty, span)?;
    for arm in arms {
        runtime::switch_to(context, arm.block);
        let value = convert(context, arm.value, ty, span)?;
        let value = runtime::materialize(context, &value, span)?;
        runtime::jump(context, merge, vec![value], span.clone());
    }

    runtime::switch_to(context, merge);
    Ok(result)
}

fn merged_type(
    context: &mut FnLoweringContext,
    arms: &[Arm],
    span: &TokenRange,
) -> Option<HMIRTypeID> {
    let mut merged: Option<HMIRTypeID> = None;

    for arm in arms {
        let ty = arm.value.ty();
        if matches!(context.env().resolve_type_id(ty).kind(), HMIRTypeKind::Void) {
            return None;
        }

        merged = Some(match merged {
            None => ty,
            Some(current) if context.env().same_type(current, ty) => current,
            Some(current) => promote::common(context, current, ty, span).unwrap_or(current),
        });
    }

    merged
}

pub(crate) fn eval_while(
    context: &mut FnLoweringContext,
    body: &HMIRBody,
    span: &TokenRange,
    condition: HMIRExprID,
    loop_body: HMIRExprID,
    pre_eval: bool,
) -> EvalResult<HMIRValue> {
    match context.context() {
        FnContext::Runtime(_) => runtime_while(context, body, span, condition, loop_body, pre_eval),
        FnContext::Comptime(_) => {
            comptime_while(context, body, span, condition, loop_body, pre_eval)
        }
    }
}

fn comptime_while(
    context: &mut FnLoweringContext,
    body: &HMIRBody,
    span: &TokenRange,
    condition: HMIRExprID,
    loop_body: HMIRExprID,
    pre_eval: bool,
) -> EvalResult<HMIRValue> {
    let mut checks = pre_eval;

    loop {
        if checks {
            let condition = eval_condition(context, body, span, condition)?;
            if !comptime_truth(context, condition, span)? {
                break;
            }
        }
        checks = true;

        match eval_scoped(context, body, loop_body) {
            Ok(_) | Err(EvalStop::Comptime(ComptimeStop::Continue)) => {}
            Err(EvalStop::Comptime(ComptimeStop::Break)) => break,
            Err(stop) => return Err(stop),
        }
    }

    Ok(constant::unit(context, span))
}

fn runtime_while(
    context: &mut FnLoweringContext,
    body: &HMIRBody,
    span: &TokenRange,
    condition: HMIRExprID,
    loop_body: HMIRExprID,
    pre_eval: bool,
) -> EvalResult<HMIRValue> {
    let condition_block = runtime::block(context, "while.condition");
    let body_block = runtime::block(context, "while.body");
    let exit = runtime::block(context, "while.exit");

    let entry = if pre_eval {
        condition_block
    } else {
        body_block
    };
    runtime::jump(context, entry, Vec::new(), span.clone());

    runtime::switch_to(context, condition_block);
    let condition = flow::catch_diverged(eval_condition(context, body, span, condition))?;
    let Some(condition) = condition else {
        for block in [body_block, exit] {
            runtime::switch_to(context, block);
            runtime::emit(context, MIRInstructionKind::Unreachable, span.clone());
        }

        return Err(RuntimeStop::Diverged.into());
    };

    let condition = runtime::materialize(context, &condition, span)?;
    runtime::branch(
        context,
        condition,
        MIRBlockTarget::new(body_block),
        MIRBlockTarget::new(exit),
        span.clone(),
    );

    runtime::switch_to(context, body_block);
    flow::catch_diverged(eval_scoped(context, body, loop_body))?;
    runtime::jump(context, condition_block, Vec::new(), span.clone());

    runtime::switch_to(context, exit);
    Ok(constant::unit(context, span))
}

pub(crate) fn eval_short_circuit(
    context: &mut FnLoweringContext,
    body: &HMIRBody,
    span: &TokenRange,
    op: HMIRBinaryOp,
    lhs: HMIRExprID,
    rhs: HMIRExprID,
) -> EvalResult<HMIRValue> {
    let left = eval_condition(context, body, span, lhs)?;
    let left = emit::read(context, left, span)?;

    match context.context() {
        FnContext::Runtime(_) => runtime_short_circuit(context, body, span, op, left, rhs),
        FnContext::Comptime(_) => {
            let truth = comptime_truth(context, left.clone(), span)?;
            if truth == (op == HMIRBinaryOp::LOr) {
                return Ok(left);
            }

            let right = eval_condition(context, body, span, rhs)?;
            Ok(emit::read(context, right, span)?)
        }
    }
}

fn runtime_short_circuit(
    context: &mut FnLoweringContext,
    body: &HMIRBody,
    span: &TokenRange,
    op: HMIRBinaryOp,
    left: HMIRValue,
    rhs: HMIRExprID,
) -> EvalResult<HMIRValue> {
    let left = runtime::materialize(context, &left, span)?;

    let ty = types::int(context.env_mut(), HMIRIntWidth::I1, false, span);
    let rhs_block = runtime::block(context, "logical.rhs");
    let merge = runtime::block(context, "logical.merge");
    let result = runtime::block_param(context, merge, ty, span)?;

    let rhs_target = MIRBlockTarget::new(rhs_block);
    let merge_target = MIRBlockTarget::with_args(merge, vec![left.clone()]);
    let (true_target, false_target) = match op {
        HMIRBinaryOp::LAnd => (rhs_target, merge_target),
        _ => (merge_target, rhs_target),
    };
    runtime::branch(context, left, true_target, false_target, span.clone());

    runtime::switch_to(context, rhs_block);
    if let Some(right) = flow::catch_diverged(eval_condition(context, body, span, rhs))? {
        let right = runtime::materialize(context, &right, span)?;
        runtime::jump(context, merge, vec![right], span.clone());
    }

    runtime::switch_to(context, merge);
    Ok(result)
}

pub(crate) fn eval_loop_exit(
    context: &mut FnLoweringContext,
    stop: ComptimeStop,
) -> EvalResult<HMIRValue> {
    match context.context() {
        FnContext::Runtime(_) => todo!("break and continue need runtime loop targets"),
        FnContext::Comptime(_) => Err(stop.into()),
    }
}

pub(crate) fn eval_return(
    context: &mut FnLoweringContext,
    body: &HMIRBody,
    span: &TokenRange,
    value: Option<HMIRExprID>,
) -> EvalResult<HMIRValue> {
    let value = match value {
        Some(value) => Some(eval(context, body, value)?),
        None => None,
    };

    return_value(context, body, value, span)
}

// Always stops evaluation: a runtime return diverges, and a comptime return carries its value to the caller
pub(crate) fn return_value(
    context: &mut FnLoweringContext,
    body: &HMIRBody,
    value: Option<HMIRValue>,
    span: &TokenRange,
) -> EvalResult<HMIRValue> {
    let return_type = context.signature().return_type();
    let ty = eval_type(context, body, return_type)?;
    let returns_void = match context.env().resolve_type_id(ty).kind() {
        HMIRTypeKind::Unreachable => {
            return Err(hmir_error(span, &typecheck::NORETURN_RETURN, ()).into());
        }
        HMIRTypeKind::Void => true,
        _ => false,
    };

    let value = match value {
        Some(value) if returns_void => {
            if !matches!(
                context.env().resolve_type_id(value.ty()).kind(),
                HMIRTypeKind::Void
            ) {
                return Err(hmir_error(span, &typecheck::VOID_RETURN_VALUE, ()).into());
            }

            None
        }
        Some(value) => {
            let value = convert(context, value, ty, span)?;
            Some(emit::read(context, value, span)?)
        }
        None if returns_void => None,
        None => return Err(hmir_error(span, &typecheck::MISSING_RETURN_VALUE, ()).into()),
    };

    match context.context() {
        FnContext::Runtime(_) => {
            let value = match &value {
                Some(value) => Some(runtime::materialize(context, value, span)?),
                None => None,
            };

            runtime::drop_scopes(context, span);
            runtime::emit(context, MIRInstructionKind::Return { value }, span.clone());
            Err(RuntimeStop::Diverged.into())
        }
        FnContext::Comptime(_) => {
            let value = value.unwrap_or_else(|| constant::unit(context, span));
            Err(ComptimeStop::Return(value).into())
        }
    }
}

pub(crate) fn fallthrough(
    context: &mut FnLoweringContext,
    body: &HMIRBody,
    is_main: bool,
    span: &TokenRange,
) -> EvalResult<HMIRValue> {
    let return_type = context.signature().return_type();
    let ty = eval_type(context, body, return_type)?;

    match context.env().resolve_type_id(ty).kind() {
        HMIRTypeKind::Void => return_value(context, body, None, span),
        HMIRTypeKind::Int(_) if is_main => {
            let zero = HMIRValue::constant(ty, HMIRConstant::Int { value: 0, ty });
            return_value(context, body, Some(zero), span)
        }
        _ => {
            runtime::emit(context, MIRInstructionKind::Unreachable, span.clone());
            Err(RuntimeStop::Diverged.into())
        }
    }
}
