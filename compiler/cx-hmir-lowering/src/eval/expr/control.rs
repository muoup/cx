mod defer;
mod exit;
mod label;
mod loops;
mod matching;
mod switch;

pub(crate) use defer::closing;
pub(crate) use exit::{fallthrough, return_value};
pub(crate) use label::eval_label;
pub(crate) use loops::{eval_for, eval_while};
pub(crate) use matching::eval_match;
pub(crate) use switch::{eval_case, eval_switch, seek};

use cx_hmir::{
    HMIRBinaryOp, HMIRBody, HMIRControlOp,
    expr::HMIRExprID,
    ty::{HMIRTypeID, context::HMIRTypeContext, intrinsic::HMIRIntWidth},
};
use cx_mir::{MIRBasicBlockID, MIRValue};
use cx_tokens::TokenRange;

use crate::{
    emit,
    env::function::{LocalLoweringContext, Permission},
    eval::{
        EvalResult,
        binding::eval_scoped,
        coerce, eval, expr,
        flow::{self, ComptimeStop, Evaluation, ready},
        types,
    },
    lower::mir,
    symbol::composite::alike,
    value::HMIRValue,
};

pub(crate) fn eval_control(
    context: &mut LocalLoweringContext,
    body: &HMIRBody,
    span: &TokenRange,
    control: &HMIRControlOp,
) -> EvalResult<HMIRValue> {
    match control {
        HMIRControlOp::Return(value) => exit::eval_return(context, body, span, *value),
        HMIRControlOp::Yield(value) => exit::eval_yield(context, body, span, *value),
        HMIRControlOp::Break => exit::eval_loop_exit(context, span, ComptimeStop::Break),
        HMIRControlOp::Continue => exit::eval_loop_exit(context, span, ComptimeStop::Continue),
        HMIRControlOp::Goto(name) => label::eval_goto(context, span, name),
        HMIRControlOp::IndirectGoto(target) => label::eval_indirect_goto(context, body, span, *target),
        HMIRControlOp::LabelAddress { function, name } => {
            label::eval_label_address(context, span, *function, name)
        }
        HMIRControlOp::Defer(operand) => defer::eval_defer(context, body, span, *operand),
        HMIRControlOp::Unsafe(operand) => {
            let expectations = context.expectations().permitting(Permission::Unsafe);
            flow::scoped(context, expectations, |context| eval(context, body, *operand))
        }
        HMIRControlOp::Unreachable => todo!("HMIRControlOp::Unreachable"),
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
    emit::open_scope(context, span);
    let result = branch(context, body, span, condition, then_branch, else_branch);
    closing(context, body, result, span)
}

fn branch(
    context: &mut LocalLoweringContext,
    body: &HMIRBody,
    span: &TokenRange,
    condition: HMIRExprID,
    then_branch: HMIRExprID,
    else_branch: Option<HMIRExprID>,
) -> EvalResult<HMIRValue> {
    if !context.is_runtime() {
        let condition = ready!(eval(context, body, condition));
        return match (comptime_truth(&condition), else_branch) {
            (true, Some(_)) => eval_scoped(context, body, then_branch),
            (false, Some(else_branch)) => eval_scoped(context, body, else_branch),
            (true, None) => {
                ready!(eval_scoped(context, body, then_branch));
                Ok(Evaluation::Ready(expr::unit(context, span)))
            }
            (false, None) => Ok(Evaluation::Ready(expr::unit(context, span))),
        };
    }

    let then_block = mir::block(context, "if.then");
    let Some(else_branch) = else_branch else {
        let merge = mir::block(context, "if.merge");
        let condition = ready!(eval_condition(context, body, condition));
        mir::branch(context, condition, then_block, merge, span);
        mir::switch_to(context, then_block);
        ready!(flow::catch_diverged(eval_scoped(context, body, then_branch)));
        mir::jump(context, merge, span);
        mir::switch_to(context, merge);
        return Ok(Evaluation::Ready(expr::unit(context, span)));
    };

    let else_block = mir::block(context, "if.else");
    let condition = ready!(eval_condition(context, body, condition));
    mir::branch(context, condition, then_block, else_block, span);
    mir::switch_to(context, then_block);
    let then = flow::catch_diverged(eval_scoped(context, body, then_branch))?;
    let span = span.clone();
    then.and_then(context, body, None, move |context, body, then, expected| {
        let mut arms = Vec::from_iter(then.map(|value| (mir::current_block(context), value)));
        mir::switch_to(context, else_block);
        let otherwise = flow::catch_diverged(eval_scoped(context, body, else_branch))?;
        otherwise.complete(context, body, expected)?.and_then(context, body, None, move |context, _, otherwise, _| {
            arms.extend(otherwise.map(|value| (mir::current_block(context), value)));
            merge(context, arms, &span)
        })
    })
}

pub(crate) fn merge(
    context: &mut LocalLoweringContext,
    arms: Vec<(MIRBasicBlockID, HMIRValue)>,
    span: &TokenRange,
) -> EvalResult<HMIRValue> {
    if arms.is_empty() {
        return Ok(Evaluation::diverged());
    }
    let merge = mir::block(context, "if.merge");
    let ty = merged_type(context, &arms, span);
    let result = ty.map(|ty| emit::scratch(context, ty, span)).transpose()?;
    for (block, value) in arms {
        mir::switch_to(context, block);
        if let Some((ty, result)) = ty.zip(result.as_ref()) {
            let value = coerce::implicit(context, value, ty, span)?;
            emit::initialize(context, result, Some(&value), span)?;
        }
        mir::jump(context, merge, span);
    }
    mir::switch_to(context, merge);
    Ok(Evaluation::Ready(result.unwrap_or_else(|| expr::unit(context, span))))
}

fn merged_type(
    context: &mut LocalLoweringContext,
    arms: &[(MIRBasicBlockID, HMIRValue)],
    span: &TokenRange,
) -> Option<HMIRTypeID> {
    let mut merged: Option<HMIRTypeID> = None;
    for (_, value) in arms {
        let ty = value.ty();
        if context.env().is_void(ty) {
            return None;
        }
        merged = Some(match merged {
            None => ty,
            Some(current) if alike(context.env(), current, ty, true) => current,
            Some(current) => {
                let left = coerce::value_type(context, current, span);
                let right = coerce::value_type(context, ty, span);
                let env = context.env();
                match (env.pointer_inner(left), env.pointer_inner(right)) {
                    (Some(left), Some(right)) => coerce::pointer(context, left, right, span),
                    _ => coerce::common(context, current, ty, span).unwrap_or(current),
                }
            }
        });
    }
    merged
}

pub(super) fn comptime_truth(condition: &HMIRValue) -> bool {
    condition.as_int().expect("a comptime condition reads as an integer constant") != 0
}

fn eval_condition(context: &mut LocalLoweringContext, body: &HMIRBody, condition: HMIRExprID) -> EvalResult<MIRValue> {
    let span = body.expr(condition).span();
    let value = ready!(eval(context, body, condition));
    let value = coerce::condition(context, value, span)?;
    Ok(Evaluation::Ready(mir::operand(context, &value, span)?))
}

fn logical_operand(
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
    let left = ready!(logical_operand(context, body, span, lhs));
    let or = op == HMIRBinaryOp::LOr;
    if !context.is_runtime() {
        return match comptime_truth(&left) == or {
            true => Ok(Evaluation::Ready(left)),
            false => logical_operand(context, body, span, rhs),
        };
    }

    let condition = mir::operand(context, &left, span)?;
    let rhs_block = mir::block(context, "logical.rhs");
    let short_block = mir::block(context, "logical.short");
    let merge = mir::block(context, "logical.merge");
    match or {
        true => mir::branch(context, condition, short_block, rhs_block, span),
        false => mir::branch(context, condition, rhs_block, short_block, span),
    }

    mir::switch_to(context, rhs_block);
    let right = ready!(flow::catch_diverged(logical_operand(context, body, span, rhs)));
    let ty = types::int(context.env_mut(), HMIRIntWidth::I1, false, span);
    let result = emit::scratch(context, ty, span)?;
    if let Some(right) = right {
        emit::initialize(context, &result, Some(&right), span)?;
        mir::jump(context, merge, span);
    }

    mir::switch_to(context, short_block);
    emit::initialize(context, &result, Some(&left), span)?;
    mir::jump(context, merge, span);
    mir::switch_to(context, merge);
    Ok(Evaluation::Ready(result))
}
