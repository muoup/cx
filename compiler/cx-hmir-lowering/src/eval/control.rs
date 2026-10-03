use cx_hmir::{HMIRBlockKind, HMIRExprID};
use cx_log::{CXResult, catalogue::mir};
use cx_tokens::TokenRange;

use crate::{
    eval::{EvalFrame, Flow, LOOP_LIMIT, eval, exec, static_condition},
    program::Program,
    staging_error,
    ty::TypeID,
    value::StaticValue,
};

pub(crate) fn block(
    cx: &mut Program<'_>,
    frame: &mut EvalFrame,
    kind: HMIRBlockKind,
    statements: &[HMIRExprID],
    tail: Option<HMIRExprID>,
) -> CXResult<Flow> {
    let mut last = StaticValue::Unit;
    for statement in statements.iter().copied().chain(tail) {
        match exec(cx, frame, statement, None)? {
            Flow::Normal(value) => last = value,
            Flow::Yield(value) if kind == HMIRBlockKind::Yield => return Ok(Flow::Normal(value)),
            flow => return Ok(flow),
        }
    }
    Ok(Flow::Normal(if tail.is_some() {
        last
    } else {
        StaticValue::Unit
    }))
}

pub(crate) fn conditional(
    cx: &mut Program<'_>,
    frame: &mut EvalFrame,
    condition: HMIRExprID,
    then_branch: HMIRExprID,
    else_branch: Option<HMIRExprID>,
    expect: Option<TypeID>,
    span: &TokenRange,
) -> CXResult<Flow> {
    let taken = static_condition(cx, frame, condition, span)?;
    match (taken, else_branch) {
        (true, _) => exec(cx, frame, then_branch, expect),
        (false, Some(branch)) => exec(cx, frame, branch, expect),
        (false, None) => Ok(Flow::Normal(StaticValue::Unit)),
    }
}

pub(super) fn check_condition(
    cx: &mut Program<'_>,
    frame: &mut EvalFrame,
    condition: HMIRExprID,
    message: &str,
) -> CXResult<()> {
    let span = frame.body().expr(condition).span().clone();
    if !static_condition(cx, frame, condition, &span)? {
        return Err(staging_error(&span, &mir::COMPTIME_ASSERTION, Some(message.into())));
    }
    Ok(())
}

pub(crate) fn while_loop(
    cx: &mut Program<'_>,
    frame: &mut EvalFrame,
    condition: HMIRExprID,
    body: HMIRExprID,
    pre_eval: bool,
    span: &TokenRange,
) -> CXResult<Flow> {
    let mut first = !pre_eval;
    for _ in 0..LOOP_LIMIT {
        if !first && !static_condition(cx, frame, condition, span)? {
            return Ok(Flow::Normal(StaticValue::Unit));
        }
        first = false;
        match exec(cx, frame, body, None)? {
            Flow::Break => return Ok(Flow::Normal(StaticValue::Unit)),
            Flow::Normal(_) | Flow::Continue => {}
            flow => return Ok(flow),
        }
    }
    Err(staging_error(span, &mir::COMPTIME_LOOP_LIMIT, LOOP_LIMIT))
}

pub(crate) fn for_loop(
    cx: &mut Program<'_>,
    frame: &mut EvalFrame,
    init: HMIRExprID,
    condition: HMIRExprID,
    increment: HMIRExprID,
    body: HMIRExprID,
    span: &TokenRange,
) -> CXResult<Flow> {
    eval(cx, frame, init, None)?;
    for _ in 0..LOOP_LIMIT {
        if !static_condition(cx, frame, condition, span)? {
            return Ok(Flow::Normal(StaticValue::Unit));
        }
        match exec(cx, frame, body, None)? {
            Flow::Break => return Ok(Flow::Normal(StaticValue::Unit)),
            Flow::Normal(_) | Flow::Continue => {}
            flow => return Ok(flow),
        }
        eval(cx, frame, increment, None)?;
    }
    Err(staging_error(span, &mir::COMPTIME_LOOP_LIMIT, LOOP_LIMIT))
}
