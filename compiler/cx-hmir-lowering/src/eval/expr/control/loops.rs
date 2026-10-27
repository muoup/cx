use cx_hmir::{HMIRBody, expr::HMIRExprID};
use cx_mir::MIRInstructionKind;
use cx_tokens::TokenRange;

use super::{closing, comptime_truth, eval_condition};
use crate::{
    emit,
    env::function::{LocalLoweringContext, runtime::Exit},
    eval::{
        EvalResult,
        binding::eval_scoped,
        eval, expr,
        flow::{self, ComptimeStop, Evaluation, StopReason, ready},
    },
    lower::mir,
    value::HMIRValue,
};

pub(crate) fn eval_for(
    context: &mut LocalLoweringContext,
    body: &HMIRBody,
    span: &TokenRange,
    init: HMIRExprID,
    condition: HMIRExprID,
    increment: HMIRExprID,
    loop_body: HMIRExprID,
) -> EvalResult<HMIRValue> {
    emit::open_scope(context, span);
    let result = match eval(context, body, init) {
        Ok(Evaluation::Ready(_)) => eval_while(context, body, span, condition, loop_body, Some(increment), true),
        Ok(Evaluation::Incomplete(incomplete)) => Err(incomplete.error()),
        result => result,
    };
    closing(context, body, result, span)
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
    if !context.is_runtime() {
        return comptime_while(context, body, span, condition, loop_body, increment, pre_eval);
    }

    let condition_block = mir::block(context, "loop.condition");
    let body_block = mir::block(context, "loop.body");
    let increment_block = increment.map(|_| mir::block(context, "loop.increment"));
    let exit = mir::block(context, "loop.exit");
    mir::jump(context, if pre_eval { condition_block } else { body_block }, span);

    mir::switch_to(context, condition_block);
    let scope = emit::open_scope(context, span);
    let Some(condition) = ready!(flow::catch_diverged(eval_condition(context, body, condition))) else {
        for block in [Some(body_block), increment_block, Some(exit)].into_iter().flatten() {
            mir::switch_to(context, block);
            context.runtime_mut().emit(MIRInstructionKind::Unreachable, span.clone());
        }
        emit::close_scope(context, span);
        return Ok(Evaluation::diverged());
    };
    mir::branch(context, condition, body_block, exit, span);

    let continue_block = increment_block.unwrap_or(condition_block);
    let exits = Exit::new(exit, Some(continue_block), Vec::new());
    context.runtime_mut().scopes_mut().control_mut(scope).set_exit(exits);

    mir::switch_to(context, body_block);
    let completion = flow::catch_diverged(eval_scoped(context, body, loop_body));
    emit::close_scope(context, span);
    ready!(completion);
    mir::jump(context, continue_block, span);

    if let (Some(block), Some(increment)) = (increment_block, increment) {
        mir::switch_to(context, block);
        ready!(flow::catch_diverged(eval(context, body, increment)));
        mir::jump(context, condition_block, span);
    }

    mir::switch_to(context, exit);
    Ok(Evaluation::Ready(expr::unit(context, span)))
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
        if checks && !comptime_truth(&ready!(eval(context, body, condition))) {
            break;
        }
        checks = true;

        match eval_scoped(context, body, loop_body)? {
            Evaluation::Ready(_) | Evaluation::Stop(StopReason::Comptime(ComptimeStop::Continue)) => {}
            Evaluation::Stop(StopReason::Comptime(ComptimeStop::Break)) => break,
            Evaluation::Stop(stop) => return Ok(Evaluation::Stop(stop)),
            Evaluation::Incomplete(incomplete) => return Err(incomplete.error()),
        }
        if let Some(increment) = increment {
            ready!(eval(context, body, increment));
        }
    }
    Ok(Evaluation::Ready(expr::unit(context, span)))
}
