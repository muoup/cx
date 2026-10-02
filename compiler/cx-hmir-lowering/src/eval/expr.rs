use cx_hmir::{HMIRExprID, HMIRExprKind, HMIRLocalID};
use cx_log::CXResult;
use cx_tokens::TokenRange;

use crate::{
    eval::{
        EvalFrame, Flow, call_static, eval, eval_static_type, eval_type_hint, ops::coerce_static,
        types::decay,
    },
    program::Program,
    staging_error,
    value::StaticValue,
};

pub(crate) fn bind(
    cx: &mut Program<'_>,
    frame: &mut EvalFrame,
    local: HMIRLocalID,
    initializer: Option<HMIRExprID>,
    span: &TokenRange,
) -> CXResult<Flow> {
    let declared = eval_type_hint(cx, frame, frame.body().local(local).ty())?;
    let value = match initializer {
        Some(initializer) => {
            let value = eval(cx, frame, initializer, declared)?;
            match declared {
                Some(ty) => coerce_static(cx, value, ty, span)?,
                None => value,
            }
        }
        None => StaticValue::Unit,
    };
    frame.bind(local, value);
    Ok(Flow::Normal(StaticValue::Unit))
}

pub(crate) fn call(
    cx: &mut Program<'_>,
    frame: &mut EvalFrame,
    callee: HMIRExprID,
    args: &[HMIRExprID],
    span: &TokenRange,
) -> CXResult<Flow> {
    let callee = eval(cx, frame, callee, None)?;
    let mut values = Vec::with_capacity(args.len());
    for arg in args {
        values.push(match frame.body().expr(*arg).kind() {
            HMIRExprKind::Hole(_) => None,
            _ => Some(eval(cx, frame, *arg, None)?),
        });
    }
    Ok(Flow::Normal(call_static(cx, callee, values, span)?))
}

pub(crate) fn dereference(
    cx: &mut Program<'_>,
    frame: &mut EvalFrame,
    operand: HMIRExprID,
    span: &TokenRange,
) -> CXResult<StaticValue> {
    let value = eval(cx, frame, operand, None)?;
    let ty = eval_static_type(cx, &value, span)?;
    let ty = decay(cx.types_mut(), ty);
    let inner = cx
        .types()
        .pointer_inner(ty)
        .ok_or_else(|| staging_error(span, "dereferenced a non-pointer".into()))?;
    if cx.types().is_function(inner) {
        return Ok(value);
    }
    let target = cx.types_mut().reference_to(inner);
    coerce_static(cx, value, target, span)
}
