use cx_hmir::{HMIRExprID, HMIRExprKind, HMIRLocalID, HMIRNativeOp, HMIRPattern, HMIRTypeOp};
use cx_log::{
    CXResult,
    catalogue::{mir, typecheck},
};
use cx_tokens::TokenRange;

use crate::{
    eval::{EvalFrame, Flow, eval, eval_static_type, exec, read_global},
    module::{member_type, variant_index},
    pattern::match_cases,
    program::Program,
    staging_error,
    ty::TypeID,
    value::StaticValue,
};

pub(crate) fn match_value(
    cx: &mut Program<'_>,
    frame: &mut EvalFrame,
    scrutinee: HMIRExprID,
    subject: HMIRLocalID,
    arms: &[(HMIRPattern, HMIRExprID)],
    expect: Option<TypeID>,
    span: &TokenRange,
) -> CXResult<Flow> {
    let value = eval(cx, frame, scrutinee, None)?;
    let value = read_global(cx, value, span)?;
    let ty = eval_static_type(cx, &value, span)?;
    if cx.types().is_pointer(ty) {
        return Err(staging_error(span, &typecheck::POINTER_PATTERN, ()));
    }
    let cases = match_cases(cx, frame, ty, arms.iter().map(|(pattern, _)| pattern), span)?;
    let tag = match &value {
        StaticValue::Int { value, .. } => Some(*value),
        StaticValue::Aggregate { fields, .. } => fields.first().map(|(index, _)| *index as i128),
        _ => None,
    };
    let owned = frame.as_local(scrutinee).is_none();
    for ((pattern, body), case) in arms.iter().zip(cases) {
        if case.is_some() && case != tag {
            continue;
        }
        frame.bind(subject, value.clone());
        bind_pattern(cx, frame, &value, pattern, owned, span)?;
        frame.moved.insert(subject);
        return Ok(match exec(cx, frame, *body, expect)? {
            Flow::Yield(value) => Flow::Normal(value),
            flow => flow,
        });
    }
    Err(staging_error(span, &mir::COMPTIME_NO_MATCH, ()))
}

fn bind_pattern(
    cx: &mut Program<'_>,
    frame: &mut EvalFrame,
    value: &StaticValue,
    pattern: &HMIRPattern,
    owned: bool,
    span: &TokenRange,
) -> CXResult<()> {
    let (local, value) = match pattern {
        HMIRPattern::Binding(local) => (*local, value.clone()),
        HMIRPattern::Variant {
            name,
            inner: Some(local),
            ..
        } => {
            let StaticValue::Aggregate { ty, fields } = value else {
                return Err(staging_error(
                    span,
                    &typecheck::TYPE_REQUIREMENT,
                    (
                        format!("variant pattern '{name}'"),
                        "a tagged union".into(),
                        Some(value.describe().into()),
                    ),
                ));
            };
            let index = variant_index(cx, *ty, name, span)?;
            let payload = member_type(cx, *ty, index, span)?;
            let value = if cx.types().is_void(payload) {
                StaticValue::Unit
            } else {
                fields
                    .iter()
                    .find(|(field, _)| *field == index)
                    .map(|(_, value)| value.clone())
                    .ok_or_else(|| staging_error(span, &mir::UNSET_PAYLOAD, ()))?
            };
            (*local, value)
        }
        _ => return Ok(()),
    };
    let declared = frame.body().local(local).ty();
    if matches!(
        frame.body().expr(declared).kind(),
        HMIRExprKind::Native(HMIRNativeOp::Type(HMIRTypeOp::Reference(_)))
    ) {
        return Err(staging_error(
            span,
            &mir::COMPTIME_INVALID_OPERATION,
            "binding a reference to compile-time storage".into(),
        ));
    }
    let ty = eval_static_type(cx, &value, span)?;
    if !owned && !cx.types().is_pod(ty) {
        return Err(staging_error(
            span,
            &typecheck::BINDING_COPIES_IN_USE,
            cx.types().display(ty),
        ));
    }
    frame.bind(local, value);
    Ok(())
}

pub(crate) fn switch(
    cx: &mut Program<'_>,
    frame: &mut EvalFrame,
    condition: HMIRExprID,
    cases: &[(HMIRExprID, HMIRExprID)],
    default: Option<HMIRExprID>,
    span: &TokenRange,
) -> CXResult<Flow> {
    let value = eval(cx, frame, condition, None)?;
    let value = read_global(cx, value, span)?;
    let ty = eval_static_type(cx, &value, span)?;
    let value = value
        .as_int()
        .ok_or_else(|| {
            staging_error(
                span,
                &typecheck::UNEXPECTED_KIND,
                ("switch condition".into(), "an integer".into()),
            )
        })?;
    let mut segments = Vec::with_capacity(cases.len() + usize::from(default.is_some()));
    for (case, body) in cases {
        let case = eval(cx, frame, *case, Some(ty))?
            .as_int()
            .ok_or_else(|| {
                staging_error(
                    span,
                    &mir::EXPECTED_CONSTANT,
                    ("switch case".into(), "integer".into()),
                )
            })?;
        if segments.iter().any(|(existing, _)| *existing == Some(case)) {
            return Err(staging_error(
                span,
                &typecheck::DUPLICATE_ITEM,
                ("case".into(), "switch".into()),
            ));
        }
        segments.push((Some(case), *body));
    }
    segments.extend(default.map(|body| (None, body)));
    segments.sort_by_key(|(_, body)| {
        frame
            .body()
            .expr(*body)
            .span()
            .source_bounds()
            .map(|(_, start, _)| start)
    });
    let start = segments
        .iter()
        .position(|(case, _)| *case == Some(value))
        .or_else(|| segments.iter().position(|(case, _)| case.is_none()));
    if let Some(start) = start {
        for (_, body) in &segments[start..] {
            match exec(cx, frame, *body, None)? {
                Flow::Normal(_) => {}
                Flow::Break => break,
                flow => return Ok(flow),
            }
        }
    }
    Ok(Flow::Normal(StaticValue::Unit))
}
