use cx_hmir::{
    HMIRBody,
    expr::{HMIRExprID, HMIRExprKind},
};
use cx_log::{
    CXResult,
    catalogue::{mir, typecheck},
};
use cx_tokens::TokenRange;

use crate::{
    emit::{self, runtime},
    env::function::{FnContext, LocalLoweringContext, runtime::Emission},
    eval::{
        EvalResult,
        binding::{eval_block, eval_scoped},
        flow::Evaluation,
        literal, unevaluated,
    },
    log::hmir_error,
    value::HMIRValue,
};

pub(crate) fn eval_defer(
    context: &mut LocalLoweringContext,
    body: &HMIRBody,
    span: &TokenRange,
    operand: HMIRExprID,
) -> EvalResult<HMIRValue> {
    if matches!(context.context(), FnContext::Comptime(_)) {
        return Err(hmir_error(span, &mir::COMPTIME_INVALID_OPERATION, "'defer'".into()));
    }
    if context.expectations().deferred() {
        return Err(hmir_error(
            span,
            &typecheck::INVALID_CONTEXT,
            ("'defer'".into(), "a deferred expression".into()),
        ));
    }
    let expectations = context.expectations().deferring();
    let operand_span = body.expr(operand).span();
    let mut child = unevaluated::detached(context, Emission::Enabled, operand_span)?;
    let result = child.with_expectations(expectations, |child| deferred(child, body, operand))?;
    match result {
        Evaluation::Ready(_) => {}
        Evaluation::Incomplete(incomplete) => return Err(incomplete.error()),
        Evaluation::Stop(_) => {
            return Err(hmir_error(operand_span, &typecheck::DEFER_FALLTHROUGH, ()));
        }
    }
    drop(child);

    context.runtime_mut().defer(operand, expectations);
    Ok(Evaluation::Ready(literal::unit(context, span)))
}

pub(crate) fn close(
    context: &mut LocalLoweringContext,
    body: &HMIRBody,
    completed: bool,
    span: &TokenRange,
) -> CXResult<()> {
    if completed && let FnContext::Runtime(runtime) = context.context() {
        expand(context, body, runtime.scope_depth() - 1)?;
    }
    emit::close_scope(context, span.clone());

    Ok(())
}

pub(crate) fn exit(
    context: &mut LocalLoweringContext,
    body: &HMIRBody,
    depth: usize,
    span: &TokenRange,
) -> CXResult<()> {
    for index in (depth..context.runtime().scope_depth()).rev() {
        expand(context, body, index)?;
        runtime::drop_scope(context, index, span);
    }

    Ok(())
}

fn expand(context: &mut LocalLoweringContext, body: &HMIRBody, index: usize) -> CXResult<()> {
    let defers = context.runtime().defers(index).to_vec();
    for (expr, expectations) in defers.into_iter().rev() {
        let lifts = context.runtime().lift_count();
        let result =
            context.with_expectations(expectations, |context| deferred(context, body, expr))?;
        if let Evaluation::Incomplete(incomplete) = result {
            return Err(incomplete.error());
        }
        context.runtime_mut().release_lifts(lifts);
    }

    Ok(())
}

fn deferred(
    context: &mut LocalLoweringContext,
    body: &HMIRBody,
    expr: HMIRExprID,
) -> EvalResult<HMIRValue> {
    let node = body.expr(expr);
    match node.kind() {
        HMIRExprKind::Block {
            statements,
            scoped: true,
            ..
        } => eval_block(context, body, node.span(), statements, true, false),
        _ => eval_scoped(context, body, expr),
    }
}
