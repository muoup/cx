use cx_hmir::{HMIRExprID, HMIRExprKind, HMIRLocalID, HMIRNativeOp, HMIRPattern, HMIRTypeOp};
use cx_log::{
    CXResult,
    catalogue::{mir, typecheck},
};
use cx_tokens::TokenRange;

use crate::{
    eval::{EvalFrame, Flow, control, eval, eval_static_type, exec, read_global},
    module::{member_type, variant_index},
    pattern::match_cases,
    program::Program,
    staging_error,
    ty::TypeID,
    value::{StaticValue, normalize_int, promote_integer_type},
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
    body: HMIRExprID,
    span: &TokenRange,
) -> CXResult<Flow> {
    let value = eval(cx, frame, condition, None)?;
    let value = read_global(cx, value, span)?;
    let ty = eval_static_type(cx, &value, span)?;
    let ty = promote_integer_type(cx.types_mut(), ty);
    let value = value.as_int().ok_or_else(|| {
        staging_error(
            span,
            &typecheck::UNEXPECTED_KIND,
            ("switch condition".into(), "an integer".into()),
        )
    })?;
    let value = normalize_int(value, ty, cx.types());

    let mut labels = Vec::new();
    case_labels(frame, body, true, &mut labels)?;
    let mut seen = Vec::with_capacity(labels.len());
    let mut selected = None;
    let mut default = None;
    for (label, case) in labels {
        let Some(case) = case else {
            default = Some(label);
            continue;
        };
        let case_span = frame.body().expr(case).span().clone();
        let case = eval(cx, frame, case, Some(ty))?.as_int().ok_or_else(|| {
            staging_error(
                &case_span,
                &mir::EXPECTED_CONSTANT,
                ("switch case".into(), "integer".into()),
            )
        })?;
        let case = normalize_int(case, ty, cx.types());
        if seen.contains(&case) {
            return Err(staging_error(
                &case_span,
                &typecheck::DUPLICATE_ITEM,
                ("case".into(), "switch".into()),
            ));
        }
        seen.push(case);
        if case == value {
            selected = Some(label);
        }
    }

    let Some(label) = selected.or(default) else {
        return Ok(Flow::Normal(StaticValue::Unit));
    };
    Ok(match enter(cx, frame, body, label)? {
        Some(Flow::Normal(_) | Flow::Break) | None => Flow::Normal(StaticValue::Unit),
        Some(flow) => flow,
    })
}

// The labels of a switch with this body, each with its case value. Only labels the body can be
// entered at are 'direct'; any other is rejected.
fn case_labels(
    frame: &EvalFrame,
    id: HMIRExprID,
    direct: bool,
    labels: &mut Vec<(HMIRExprID, Option<HMIRExprID>)>,
) -> CXResult<()> {
    let expr = frame.body().expr(id);
    match expr.kind() {
        HMIRExprKind::Case { .. } if !direct => Err(staging_error(
            expr.span(),
            &mir::COMPTIME_INVALID_OPERATION,
            "a case label inside a statement nested in its switch".into(),
        )),
        HMIRExprKind::Case { value, body } => {
            labels.push((id, *value));
            case_labels(frame, *body, direct, labels)
        }
        HMIRExprKind::Label { body, .. } => case_labels(frame, *body, direct, labels),
        HMIRExprKind::Block {
            statements, tail, ..
        } => statements
            .iter()
            .chain(tail)
            .try_for_each(|statement| case_labels(frame, *statement, direct, labels)),
        HMIRExprKind::If {
            then_branch,
            else_branch,
            ..
        } => std::iter::once(then_branch)
            .chain(else_branch)
            .try_for_each(|branch| case_labels(frame, *branch, false, labels)),
        HMIRExprKind::While { body, .. } | HMIRExprKind::For { body, .. } => {
            case_labels(frame, *body, false, labels)
        }
        _ => Ok(()),
    }
}

// Runs 'id' from 'label' onwards, when the label is within it
fn enter(
    cx: &mut Program<'_>,
    frame: &mut EvalFrame,
    id: HMIRExprID,
    label: HMIRExprID,
) -> CXResult<Option<Flow>> {
    match frame.body().expr(id).kind().clone() {
        HMIRExprKind::Case { body, .. } if id == label => exec(cx, frame, body, None).map(Some),
        HMIRExprKind::Case { body, .. } | HMIRExprKind::Label { body, .. } => {
            enter(cx, frame, body, label)
        }
        HMIRExprKind::Block {
            kind,
            statements,
            tail,
        } => {
            for (index, statement) in statements.iter().enumerate() {
                return match enter(cx, frame, *statement, label)? {
                    Some(Flow::Normal(_)) => {
                        control::block(cx, frame, kind, &statements[index + 1..], tail).map(Some)
                    }
                    Some(flow) => Ok(Some(flow)),
                    None => continue,
                };
            }
            match tail {
                Some(tail) => enter(cx, frame, tail, label),
                None => Ok(None),
            }
        }
        _ => Ok(None),
    }
}
