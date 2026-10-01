use cx_hmir::{HMIRBlockKind, HMIRExprID};
use cx_log::CXResult;
use cx_tokens::TokenRange;

use crate::{
    eval::{EvalFrame, Flow, LOOP_LIMIT},
    program::Program,
    staging_error,
    ty::TypeID,
    value::StaticValue,
};

pub(crate) fn block(
    program: &mut Program<'_>,
    frame: &mut EvalFrame,
    kind: HMIRBlockKind,
    statements: &[HMIRExprID],
    tail: Option<HMIRExprID>,
) -> CXResult<Flow> {
    let mut last = StaticValue::Unit;
    for statement in statements.iter().copied().chain(tail) {
        match program.exec(frame, statement, None)? {
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
    program: &mut Program<'_>,
    frame: &mut EvalFrame,
    condition: HMIRExprID,
    then_branch: HMIRExprID,
    else_branch: Option<HMIRExprID>,
    expect: Option<TypeID>,
    span: &TokenRange,
) -> CXResult<Flow> {
    let taken = program.static_condition(frame, condition, span)?;
    match (taken, else_branch) {
        (true, _) => program.exec(frame, then_branch, expect),
        (false, Some(branch)) => program.exec(frame, branch, expect),
        (false, None) => Ok(Flow::Normal(StaticValue::Unit)),
    }
}

pub(crate) fn while_loop(
    program: &mut Program<'_>,
    frame: &mut EvalFrame,
    condition: HMIRExprID,
    body: HMIRExprID,
    pre_eval: bool,
    span: &TokenRange,
) -> CXResult<Flow> {
    let mut first = !pre_eval;
    for _ in 0..LOOP_LIMIT {
        if !first && !program.static_condition(frame, condition, span)? {
            return Ok(Flow::Normal(StaticValue::Unit));
        }
        first = false;
        match program.exec(frame, body, None)? {
            Flow::Break => return Ok(Flow::Normal(StaticValue::Unit)),
            Flow::Normal(_) | Flow::Continue => {}
            flow => return Ok(flow),
        }
    }
    Err(staging_error(span, "comptime loop limit exceeded".into()))
}

pub(crate) fn for_loop(
    program: &mut Program<'_>,
    frame: &mut EvalFrame,
    init: HMIRExprID,
    condition: HMIRExprID,
    increment: HMIRExprID,
    body: HMIRExprID,
    span: &TokenRange,
) -> CXResult<Flow> {
    program.eval(frame, init)?;
    for _ in 0..LOOP_LIMIT {
        if !program.static_condition(frame, condition, span)? {
            return Ok(Flow::Normal(StaticValue::Unit));
        }
        match program.exec(frame, body, None)? {
            Flow::Break => return Ok(Flow::Normal(StaticValue::Unit)),
            Flow::Normal(_) | Flow::Continue => {}
            flow => return Ok(flow),
        }
        program.eval(frame, increment)?;
    }
    Err(staging_error(span, "comptime loop limit exceeded".into()))
}
