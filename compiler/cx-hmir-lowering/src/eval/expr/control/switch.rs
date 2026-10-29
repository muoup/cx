use cx_hmir::{
    HMIRBody,
    expr::{HMIRExprID, HMIRExprKind},
    ty::{HMIRTypeKind, context::HMIRTypeContext, intrinsic::HMIRIntType},
};
use cx_log::{
    CXResult,
    catalogue::{self, typecheck},
};
use cx_mir::{MIRBlockTarget, MIRInstructionKind};
use cx_tokens::TokenRange;

use super::{closing, loops::eval_while};
use crate::{
    emit,
    env::function::{
        FnContext, LocalLoweringContext,
        runtime::{Emission, Exit},
    },
    eval::{
        EvalResult,
        binding::{eval_block, eval_scoped},
        coerce, eval, eval_comptime, expr,
        flow::{self, ComptimeStop, EvalExit, Evaluation, StopReason},
        types,
    },
    log::hmir_error,
    lower::mir,
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
) -> EvalResult<Evaluation<HMIRValue>> {
    let span = span.clone();
    eval(context, body, condition)?.and_then(
        context,
        body,
        None,
        move |context, body, condition, _| {
            let condition = coerce::std_rvalue(context, condition, &span)?;
            if !context.env().is_int(condition.ty()) {
                let found = types::name(context.env(), condition.ty());
                let mismatch = ("switch condition".into(), "integer type".into(), found);
                return Err(hmir_error(&span, &typecheck::TYPE_MISMATCH, mismatch).into());
            }
            let condition = coerce::arithmetic(context, condition, &span)?;
            let HMIRTypeKind::Int(integer) = *context.env().resolve_kind(condition.ty()) else {
                unreachable!("an integer switch condition has an integer promotion")
            };
            let mut labels = Vec::new();
            collect(context, body, switch_body, integer, &mut labels)?;
            match context.is_runtime() {
                true => runtime_switch(
                    context,
                    body,
                    &span,
                    switch_body,
                    condition,
                    integer,
                    labels,
                ),
                false => comptime_switch(
                    context,
                    body,
                    &span,
                    switch_body,
                    condition,
                    integer,
                    labels,
                ),
            }
        },
    )
}

pub(crate) fn eval_case(
    context: &mut LocalLoweringContext,
    body: &HMIRBody,
    id: HMIRExprID,
    statement: HMIRExprID,
) -> EvalResult<Evaluation<HMIRValue>> {
    let span = body.expr(id).span();
    let target = match context.context() {
        FnContext::Comptime(comptime) if comptime.switch().is_some() => {
            return eval(context, body, statement);
        }
        FnContext::Comptime(_) => None,
        FnContext::Runtime(runtime) => {
            runtime.target(|scope| scope.exit().and_then(|exit| exit.case_block(id)))
        }
    };
    let Some((depth, target)) = target else {
        let required = ("case label".into(), "an enclosing switch statement".into());
        return Err(hmir_error(span, &typecheck::REQUIRED_CONTEXT, required).into());
    };
    if context.runtime().defers_past(depth) {
        let invalid = (
            "case label".into(),
            "the scope of a deferred expression".into(),
        );
        return Err(hmir_error(span, &typecheck::INVALID_CONTEXT, invalid).into());
    }
    if context.runtime().emission() == Emission::Enabled {
        mir::jump(context, target, span);
    }
    mir::switch_to(context, target);
    let span = span.clone();
    eval(context, body, statement)?.map(context, move |context, _| Ok(expr::unit(context, &span)))
}

fn collect(
    context: &mut LocalLoweringContext,
    body: &HMIRBody,
    id: HMIRExprID,
    integer: HMIRIntType,
    labels: &mut Vec<CaseLabel>,
) -> CXResult<()> {
    let children = match body.expr(id).kind() {
        HMIRExprKind::Case {
            value: expr,
            body: child,
        } => {
            let value = expr
                .map(|expr| case_value(context, body, expr, integer))
                .transpose()?;
            if labels.iter().any(|label| label.value == value) {
                let duplicate = match value {
                    Some(_) => ("case".into(), "switch".into()),
                    None => ("default label".into(), "switch statement".into()),
                };
                return Err(hmir_error(
                    body.expr(expr.unwrap_or(id)).span(),
                    &typecheck::DUPLICATE_ITEM,
                    duplicate,
                ));
            }
            labels.push(CaseLabel { id, value });
            vec![*child]
        }
        HMIRExprKind::Block { statements, .. } => statements.clone(),
        HMIRExprKind::If {
            then_branch,
            else_branch,
            ..
        } => std::iter::once(*then_branch).chain(*else_branch).collect(),
        HMIRExprKind::While { body: child, .. }
        | HMIRExprKind::For { body: child, .. }
        | HMIRExprKind::Label { body: child, .. } => vec![*child],
        _ => Vec::new(),
    };
    children
        .into_iter()
        .try_for_each(|child| collect(context, body, child, integer, labels))
}

fn case_value(
    context: &mut LocalLoweringContext,
    body: &HMIRBody,
    expr: HMIRExprID,
    integer: HMIRIntType,
) -> CXResult<i128> {
    let value = eval_comptime(context, body, expr, None)?;
    match (context.env().resolve_kind(value.ty()), value.kind()) {
        (HMIRTypeKind::Int(source), HMIRValueKind::Constant(HMIRConstant::Int(value))) => {
            Ok(int::normalize(int::normalize(*value, *source), integer))
        }
        _ => {
            let expected = ("switch case".into(), "integer".into());
            Err(hmir_error(
                body.expr(expr).span(),
                &catalogue::mir::EXPECTED_CONSTANT,
                expected,
            ))
        }
    }
}

fn runtime_switch(
    context: &mut LocalLoweringContext,
    body: &HMIRBody,
    span: &TokenRange,
    switch_body: HMIRExprID,
    condition: HMIRValue,
    integer: HMIRIntType,
    labels: Vec<CaseLabel>,
) -> EvalResult<Evaluation<HMIRValue>> {
    let condition = mir::operand(context, &condition, span)?;
    let exit = mir::block(context, "switch.exit");
    let mut blocks = Vec::new();
    let mut cases = Vec::new();
    let mut default = exit;
    for label in &labels {
        let block = mir::block(
            context,
            if label.value.is_some() {
                "switch.case"
            } else {
                "switch.default"
            },
        );
        match label.value {
            Some(value) => cases.push((value, MIRBlockTarget::new(block))),
            None => default = block,
        }
        blocks.push((label.id, block));
    }
    let entry = mir::block(context, "switch.body");
    let branch = MIRInstructionKind::CaseBranch {
        value: condition,
        signed: int::signed(integer),
        cases,
        default: Some(MIRBlockTarget::new(default)),
    };
    context.runtime_mut().emit(branch, span.clone());

    mir::switch_to(context, entry);
    let scope = emit::open_scope(context, span);
    context
        .runtime_mut()
        .scopes_mut()
        .control_mut(scope)
        .set_exit(Exit::new(exit, None, blocks));
    let completion = eval_scoped(context, body, switch_body, false);
    let finish = span.clone();
    let completion = flow::finally(context, body, completion, move |context, _, _| {
        emit::close_scope(context, &finish);
        if context.runtime().emission() == Emission::Enabled {
            mir::jump(context, exit, &finish);
        }
        mir::switch_to(context, exit);
        Ok(())
    });
    let span = span.clone();
    flow::catch_diverged(completion)?.map(context, move |context, _| Ok(expr::unit(context, &span)))
}

fn comptime_switch(
    context: &mut LocalLoweringContext,
    body: &HMIRBody,
    span: &TokenRange,
    switch_body: HMIRExprID,
    condition: HMIRValue,
    integer: HMIRIntType,
    labels: Vec<CaseLabel>,
) -> EvalResult<Evaluation<HMIRValue>> {
    let value = int::normalize(
        condition
            .as_int()
            .expect("a comptime integer condition is constant"),
        integer,
    );
    let selected = labels
        .iter()
        .find(|label| label.value == Some(value))
        .or_else(|| labels.iter().find(|label| label.value.is_none()));
    let unit = expr::unit(context, span);
    let Some(selected) = selected else {
        return Ok(Evaluation::Ready(unit));
    };
    let scope = emit::open_scope(context, span);
    context
        .comptime_mut()
        .scopes_mut()
        .control_mut(scope)
        .begin_switch(selected.id);
    let result = eval(context, body, switch_body).and_then(|result| {
        let finished = unit.clone();
        result.map(context, move |_, _| Ok(finished))
    });
    let result = flow::catch(result, move |stop| match stop {
        StopReason::Comptime(ComptimeStop::Break) => Ok(Evaluation::Ready(unit)),
        stop => Err(EvalExit::Stop(stop)),
    });
    closing(context, body, result, span)
}

pub(crate) fn seek(
    context: &mut LocalLoweringContext,
    body: &HMIRBody,
    id: HMIRExprID,
    selected: HMIRExprID,
) -> EvalResult<Evaluation<HMIRValue>> {
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
            let value = eval_scoped(context, body, *then_branch, false)?.ready()?;
            match else_branch {
                _ if context.comptime().selected_case().is_none() => Ok(Evaluation::Ready(value)),
                Some(otherwise) => eval_scoped(context, body, *otherwise, false),
                None => Ok(Evaluation::Ready(expr::unit(context, span))),
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
        _ => Ok(Evaluation::Ready(expr::unit(context, span))),
    }
}

fn seek_loop(
    context: &mut LocalLoweringContext,
    body: &HMIRBody,
    condition: HMIRExprID,
    loop_body: HMIRExprID,
    increment: Option<HMIRExprID>,
    span: &TokenRange,
) -> EvalResult<Evaluation<HMIRValue>> {
    match eval_scoped(context, body, loop_body, false) {
        Ok(Evaluation::Ready(_)) if context.comptime().selected_case().is_some() => {
            return Ok(Evaluation::Ready(expr::unit(context, span)));
        }
        Ok(Evaluation::Incomplete(incomplete)) => return Ok(Evaluation::Incomplete(incomplete)),
        Err(EvalExit::Stop(StopReason::Comptime(ComptimeStop::Break))) => {
            return Ok(Evaluation::Ready(expr::unit(context, span)));
        }
        Err(EvalExit::Stop(StopReason::Comptime(ComptimeStop::Continue)))
        | Ok(Evaluation::Ready(_)) => {}
        Err(exit) => return Err(exit),
    }
    if let Some(increment) = increment {
        eval(context, body, increment)?.ready()?;
    }
    eval_while(context, body, span, condition, loop_body, increment, true)
}
