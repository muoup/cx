use cx_hmir::{HMIRExprID, HMIRLocalID};
use cx_log::{
    CXResult,
    catalogue::analysis,
    error::{CXError, context::from_token_range},
};
use cx_tokens::TokenRange;

use crate::{eval::EvalFrame, program::Program, value::StaticValue};

// Ownership is asserted along the path the interpreter actually takes: comptime types can depend
// on values, so there is no MIR body for the ownership analysis to check ahead of time.

fn function_name(frame: &EvalFrame) -> String {
    frame.unit.def(frame.def.def()).name().to_string()
}

fn local_name(frame: &EvalFrame, local: HMIRLocalID) -> (String, bool) {
    let name = frame
        .body()
        .local(local)
        .name()
        .map(ToString::to_string)
        .unwrap_or_else(|| local.to_string());
    let discarded = name == "_";
    (name, discarded)
}

pub(crate) fn require_live(
    frame: &EvalFrame,
    local: HMIRLocalID,
    span: &TokenRange,
) -> CXResult<()> {
    if !frame.moved.contains(&local) {
        return Ok(());
    }
    let (name, discarded) = local_name(frame, local);
    Err(CXError::new(
        analysis::AFTER_MOVE.bind((function_name(frame), name, "was used".into(), discarded)),
        from_token_range(span),
    ))
}

// 'move' and '@leak' give up the local they name
pub(super) fn consume(frame: &mut EvalFrame, operand: HMIRExprID) {
    if let Some(local) = frame.as_local(operand) {
        frame.moved.insert(local);
    }
}

pub(super) fn require_consumed(cx: &Program<'_>, frame: &EvalFrame) -> CXResult<()> {
    let mut leaked = frame
        .locals
        .iter()
        .filter(|(local, value)| {
            !frame.moved.contains(local)
                && matches!(value, StaticValue::Aggregate { ty, .. } if cx.types().is_nodrop(*ty))
        })
        .map(|(local, _)| local_name(frame, *local))
        .collect::<Vec<_>>();
    leaked.sort();
    let Some((name, discarded)) = leaked.into_iter().next() else {
        return Ok(());
    };
    let span = frame.unit.def(frame.def.def()).span();
    Err(CXError::new(
        analysis::VALUE_NOT_CONSUMED.bind((
            function_name(frame),
            "value".into(),
            name,
            "lifetime".into(),
            discarded,
        )),
        from_token_range(span),
    ))
}
