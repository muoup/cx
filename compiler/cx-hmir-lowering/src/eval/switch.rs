use cx_hmir::{
    HMIRBody,
    expr::{HMIRExprID, HMIRExprKind},
    ty::{HMIRTypeKind, context::HMIRTypeContext, intrinsic::HMIRIntType},
};
use cx_log::{
    CXResult,
    catalogue::{mir, typecheck},
};
use cx_mir::{MIRBasicBlockID, MIRBlockTarget, MIRInstructionKind};
use cx_tokens::TokenRange;

use crate::{
    emit::runtime,
    env::function::{
        FnContext, LocalLoweringContext,
        runtime::{Emission, ExitFrame},
    },
    eval::{
        EvalResult,
        binding::{eval_block, eval_scoped},
        coerce,
        control::eval_while,
        eval, eval_comptime,
        flow::{self, ComptimeStop, Evaluation, StopReason, ready},
        literal, types,
    },
    log::hmir_error,
    value::{HMIRValue, HMIRValueKind, constant::HMIRConstant, scalar::int},
};

struct CaseLabel {
    id: HMIRExprID,
    value: Option<i128>,
}

pub(crate) fn eval_switch(
    context: &mut LocalLoweringContext,
    body: &HMIRBody,
    span: &TokenRange,
    condition: HMIRExprID,
    switch_body: HMIRExprID,
) -> EvalResult<HMIRValue> {
    let condition = eval(context, body, condition)?;
    let span = span.clone();
    condition.and_then(context, body, None, move |context, body, condition, _| {
        let condition = coerce::std_rvalue(context, condition, &span)?;
        let found = condition.ty();
        if !context.env().is_int(found) {
            return Err(hmir_error(
                &span,
                &typecheck::TYPE_MISMATCH,
                (
                    "switch condition".into(),
                    "integer type".into(),
                    types::name(context.env(), found),
                ),
            )
            .into());
        }

        let condition = coerce::arithmetic(context, condition, &span)?;
        let HMIRTypeKind::Int(integer) = context.env().resolve_kind(condition.ty()) else {
            unreachable!("an integer switch condition has an integer promotion")
        };
        let integer = *integer;

        match context.context() {
            FnContext::Runtime(_) => {
                runtime_switch(context, body, &span, switch_body, condition, integer)
            }
            FnContext::Comptime(_) => {
                comptime_switch(context, body, &span, switch_body, condition, integer)
            }
        }
    })
}

pub(crate) fn eval_case(
    context: &mut LocalLoweringContext,
    body: &HMIRBody,
    id: HMIRExprID,
    statement: HMIRExprID,
) -> EvalResult<HMIRValue> {
    let span = body.expr(id).span();
    match context.context() {
        FnContext::Comptime(comptime) if comptime.in_switch() => eval(context, body, statement),
        FnContext::Comptime(_) => Err(hmir_error(
            span,
            &typecheck::REQUIRED_CONTEXT,
            ("case label".into(), "an enclosing switch statement".into()),
        )
        .into()),
        FnContext::Runtime(_) => {
            let Some((target, depth)) = context.runtime().case_block(id) else {
                return Err(hmir_error(
                    span,
                    &typecheck::REQUIRED_CONTEXT,
                    ("case label".into(), "an enclosing switch statement".into()),
                )
                .into());
            };
            if context.runtime().defers_past(depth) {
                return Err(hmir_error(
                    span,
                    &typecheck::INVALID_CONTEXT,
                    ("case label".into(), "the scope of a deferred expression".into()),
                ));
            }
            if context.runtime().emission() == Emission::Enabled {
                runtime::jump(context, target, span.clone());
            }
            runtime::switch_to(context, target);

            let span = span.clone();
            let statement = eval(context, body, statement)?;
            statement.and_then(context, body, None, move |context, _, _, _| {
                Ok(Evaluation::Ready(literal::unit(context, &span)))
            })
        }
    }
}

fn children(body: &HMIRBody, id: HMIRExprID) -> Vec<HMIRExprID> {
    match body.expr(id).kind() {
        HMIRExprKind::Block { statements, .. } => statements.clone(),
        HMIRExprKind::If {
            then_branch,
            else_branch,
            ..
        } => std::iter::once(*then_branch).chain(*else_branch).collect(),
        HMIRExprKind::Case { body: child, .. }
        | HMIRExprKind::While { body: child, .. }
        | HMIRExprKind::For { body: child, .. }
        | HMIRExprKind::Label { body: child, .. } => vec![*child],
        _ => Vec::new(),
    }
}

fn collect_labels(
    context: &mut LocalLoweringContext,
    body: &HMIRBody,
    id: HMIRExprID,
    condition_type: HMIRIntType,
    labels: &mut Vec<CaseLabel>,
) -> CXResult<()> {
    if let HMIRExprKind::Case { value, .. } = body.expr(id).kind() {
        let case_value = if let Some(value) = value {
            let span = body.expr(*value).span();
            let value = eval_comptime(context, body, *value, None)?;
            let (
                HMIRTypeKind::Int(source_type),
                HMIRValueKind::Constant(HMIRConstant::Int { value, .. }),
            ) = (context.env().resolve_kind(value.ty()), value.kind())
            else {
                return Err(hmir_error(
                    span,
                    &mir::EXPECTED_CONSTANT,
                    ("switch case".into(), "integer".into()),
                ));
            };
            let value = int::normalize(*value, *source_type);
            let value = int::normalize(value, condition_type);
            if labels.iter().any(|label| label.value == Some(value)) {
                return Err(hmir_error(
                    span,
                    &typecheck::DUPLICATE_ITEM,
                    ("case".into(), "switch".into()),
                ));
            }
            Some(value)
        } else {
            if labels.iter().any(|label| label.value.is_none()) {
                return Err(hmir_error(
                    body.expr(id).span(),
                    &typecheck::DUPLICATE_ITEM,
                    ("default label".into(), "switch statement".into()),
                ));
            }
            None
        };
        labels.push(CaseLabel {
            id,
            value: case_value,
        });
    }
    for child in children(body, id) {
        collect_labels(context, body, child, condition_type, labels)?;
    }
    Ok(())
}

fn runtime_switch(
    context: &mut LocalLoweringContext,
    body: &HMIRBody,
    span: &TokenRange,
    switch_body: HMIRExprID,
    condition: HMIRValue,
    integer: HMIRIntType,
) -> EvalResult<HMIRValue> {
    let mut labels = Vec::new();
    collect_labels(context, body, switch_body, integer, &mut labels)?;

    let condition = runtime::lower_value(context, &condition, span)?;
    let exit = runtime::block(context, "switch.exit");
    let blocks = labels
        .iter()
        .map(|label| {
            let name = if label.value.is_some() {
                "switch.case"
            } else {
                "switch.default"
            };
            (label.id, runtime::block(context, name))
        })
        .collect::<Vec<_>>();
    let entry = runtime::block(context, "switch.body");
    let cases = labels
        .iter()
        .zip(&blocks)
        .filter_map(|(label, (_, block))| {
            label
                .value
                .map(|value| (value, MIRBlockTarget::new(*block)))
        })
        .collect();
    let default = labels
        .iter()
        .zip(&blocks)
        .find_map(|(label, (_, block))| label.value.is_none().then_some(*block))
        .unwrap_or(exit);
    context.runtime_mut().emit(
        MIRInstructionKind::CaseBranch {
            value: condition,
            signed: int::signed(integer),
            cases,
            default: Some(MIRBlockTarget::new(default)),
        },
        span.clone(),
    );

    runtime::switch_to(context, entry);
    let scope_depth = context.runtime().scope_depth();
    context
        .runtime_mut()
        .push_exit(ExitFrame::new(exit, None, scope_depth, blocks));
    let completion = eval_scoped(context, body, switch_body);
    let span = span.clone();
    let finish_span = span.clone();
    let completion = flow::finally(context, body, completion, move |context, _, _| {
        finish_runtime_switch(context, exit, &finish_span);
        Ok(())
    });
    let completion = flow::catch_diverged(completion)?;
    completion.and_then(context, body, None, move |context, _, _, _| {
        Ok(Evaluation::Ready(literal::unit(context, &span)))
    })
}

fn finish_runtime_switch(
    context: &mut LocalLoweringContext,
    exit: MIRBasicBlockID,
    span: &TokenRange,
) {
    context.runtime_mut().pop_exit();
    match context.runtime().emission() {
        Emission::Enabled => {
            runtime::jump(context, exit, span.clone());
        }
        Emission::Suppressed | Emission::Unevaluated => {}
    }
    runtime::switch_to(context, exit);
}

pub(crate) fn contains_case(
    context: &LocalLoweringContext,
    body: &HMIRBody,
    id: HMIRExprID,
) -> bool {
    match body.expr(id).kind() {
        HMIRExprKind::Case { .. } => context.runtime().case_block(id).is_some(),
        _ => children(body, id)
            .into_iter()
            .any(|child| contains_case(context, body, child)),
    }
}

fn comptime_switch(
    context: &mut LocalLoweringContext,
    body: &HMIRBody,
    span: &TokenRange,
    switch_body: HMIRExprID,
    condition: HMIRValue,
    integer: HMIRIntType,
) -> EvalResult<HMIRValue> {
    let HMIRValueKind::Constant(HMIRConstant::Int { value, .. }) = condition.kind() else {
        unreachable!("a comptime integer condition is constant")
    };
    let value = int::normalize(*value, integer);
    let mut labels = Vec::new();
    collect_labels(context, body, switch_body, integer, &mut labels)?;
    let selected = labels
        .iter()
        .find(|label| label.value == Some(value))
        .or_else(|| labels.iter().find(|label| label.value.is_none()));
    let Some(selected) = selected else {
        return Ok(Evaluation::Ready(literal::unit(context, span)));
    };
    context.comptime_mut().begin_switch(selected.id);
    let span = span.clone();
    let result = eval(context, body, switch_body).and_then(|result| {
        let unit = literal::unit(context, &span);
        result
            .catch(move |stop| match stop {
                StopReason::Comptime(ComptimeStop::Break) => Ok(Evaluation::Ready(unit)),
                stop => Ok(Evaluation::Stop(stop)),
            })?
            .and_then(context, body, None, move |context, _, _, _| {
                Ok(Evaluation::Ready(literal::unit(context, &span)))
            })
    });
    flow::finally(context, body, result, |context, _, _| {
        context.comptime_mut().end_switch();
        Ok(())
    })
}

pub(crate) fn seek(
    context: &mut LocalLoweringContext,
    body: &HMIRBody,
    id: HMIRExprID,
    selected: HMIRExprID,
) -> EvalResult<HMIRValue> {
    if id == selected {
        context.comptime_mut().activate_case();
        return eval(context, body, id);
    }
    let span = body.expr(id).span();
    match body.expr(id).kind() {
        HMIRExprKind::Case {
            body: statement, ..
        }
        | HMIRExprKind::Label {
            body: statement, ..
        } => seek(context, body, *statement, selected),
        HMIRExprKind::Block {
            statements,
            scoped,
            yields,
        } => eval_block(context, body, span, statements, *scoped, *yields),
        HMIRExprKind::If {
            then_branch,
            else_branch,
            ..
        } => {
            let value = ready!(eval_scoped(context, body, *then_branch));
            if context.comptime().selected_case().is_none() {
                return Ok(Evaluation::Ready(value));
            }
            match else_branch {
                Some(otherwise) => eval_scoped(context, body, *otherwise),
                None => Ok(Evaluation::Ready(literal::unit(context, span))),
            }
        }
        HMIRExprKind::While {
            condition,
            body: loop_body,
            ..
        } => seek_loop(context, body, *condition, *loop_body, None, span),
        HMIRExprKind::For {
            condition,
            increment,
            body: loop_body,
            ..
        } => seek_loop(
            context,
            body,
            *condition,
            *loop_body,
            Some(*increment),
            span,
        ),
        _ => Ok(Evaluation::Ready(literal::unit(context, span))),
    }
}

fn seek_loop(
    context: &mut LocalLoweringContext,
    body: &HMIRBody,
    condition: HMIRExprID,
    loop_body: HMIRExprID,
    increment: Option<HMIRExprID>,
    span: &TokenRange,
) -> EvalResult<HMIRValue> {
    let first = eval_scoped(context, body, loop_body)?;
    match first {
        Evaluation::Ready(_) if context.comptime().selected_case().is_some() => {
            return Ok(Evaluation::Ready(literal::unit(context, span)));
        }
        Evaluation::Stop(StopReason::Comptime(ComptimeStop::Break)) => {
            return Ok(Evaluation::Ready(literal::unit(context, span)));
        }
        Evaluation::Ready(_) | Evaluation::Stop(StopReason::Comptime(ComptimeStop::Continue)) => {}
        other => return Ok(other),
    }
    if let Some(increment) = increment {
        ready!(eval(context, body, increment));
    }
    eval_while(context, body, span, condition, loop_body, increment, true)
}
