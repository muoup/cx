use cx_hmir::{HMIRExprID, HMIRLocalID};
use cx_log::CXResult;
use cx_tokens::TokenRange;

use crate::{
    eval::{EvalFrame, Flow, type_relations::decay},
    program::Program,
    staging_error,
    ty::TypeKind,
    value::StaticValue,
};

pub(crate) fn bind(
    program: &mut Program<'_>,
    frame: &mut EvalFrame,
    local: HMIRLocalID,
    initializer: Option<HMIRExprID>,
    span: &TokenRange,
) -> CXResult<Flow> {
    let declared = program.eval_type_hint(frame, frame.body().local(local).ty())?;
    let value = match initializer {
        Some(initializer) => {
            let value = program.eval_expecting(frame, initializer, declared)?;
            match declared {
                Some(ty) => program.coerce_static(value, ty, span)?,
                None => value,
            }
        }
        None => StaticValue::Unit,
    };
    frame.bind(local, value);
    Ok(Flow::Normal(StaticValue::Unit))
}

pub(crate) fn call(
    program: &mut Program<'_>,
    frame: &mut EvalFrame,
    callee: HMIRExprID,
    args: &[HMIRExprID],
    span: &TokenRange,
) -> CXResult<Flow> {
    let callee = program.eval(frame, callee)?;
    let mut values = Vec::with_capacity(args.len());
    for arg in args {
        values.push(program.eval(frame, *arg)?);
    }
    Ok(Flow::Normal(program.call_static(callee, values, span)?))
}

pub(crate) fn dereference(
    program: &mut Program<'_>,
    frame: &mut EvalFrame,
    operand: HMIRExprID,
    span: &TokenRange,
) -> CXResult<StaticValue> {
    let value = program.eval(frame, operand)?;
    let ty = program.static_type(&value, span)?;
    let ty = decay(program.types_mut(), ty);
    let inner = program
        .types()
        .pointee(ty)
        .ok_or_else(|| staging_error(span, "dereferenced a non-pointer".into()))?;
    if matches!(program.types().kind(inner), TypeKind::Function(_)) {
        return Ok(value);
    }
    let target = program.types_mut().reference(inner);
    program.coerce_static(value, target, span)
}
