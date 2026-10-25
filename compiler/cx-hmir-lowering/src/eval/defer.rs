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
    env::function::{
        FnContext, LocalLoweringContext,
        runtime::{Defer, Emission, RuntimeScope},
    },
    eval::{
        EvalResult,
        binding::{eval_block, eval_scoped},
        flow::Evaluation,
        literal, unevaluated,
    },
    log::hmir_error,
    value::{Bindings, HMIRValue},
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
    if context.runtime().deferred() {
        return Err(hmir_error(
            span,
            &typecheck::INVALID_CONTEXT,
            ("'defer'".into(), "a deferred expression".into()),
        ));
    }
    let operand_span = body.expr(operand).span();
    let mut child = unevaluated::detached(context, Emission::Enabled, true, operand_span)?;
    match deferred(&mut child, body, operand)? {
        Evaluation::Ready(_) => {}
        Evaluation::Incomplete(incomplete) => return Err(incomplete.error()),
        Evaluation::Stop(_) => {
            return Err(hmir_error(operand_span, &typecheck::DEFER_FALLTHROUGH, ()));
        }
    }
    drop(child);

    let defer = Defer::new(
        operand,
        context.expectations(),
        context.source(),
        context.visible_through(),
    );
    let runtime = context.runtime_mut();
    let top = runtime.scopes().top();
    runtime.scopes_mut().control_mut(top).defer(defer);
    Ok(Evaluation::Ready(literal::unit(context, span)))
}

pub(crate) fn close(
    context: &mut LocalLoweringContext,
    completed: bool,
    span: &TokenRange,
) -> CXResult<()> {
    if completed && let FnContext::Runtime(runtime) = context.context() {
        expand(context, runtime.scopes().top())?;
    }
    emit::close_scope(context, span.clone());

    Ok(())
}

pub(crate) fn exit(
    context: &mut LocalLoweringContext,
    depth: usize,
    span: &TokenRange,
) -> CXResult<()> {
    for index in (depth..context.runtime().scopes().len()).rev() {
        expand(context, index)?;
        runtime::drop_scope(context, index, span);
    }

    Ok(())
}

fn expand(context: &mut LocalLoweringContext, index: usize) -> CXResult<()> {
    let defers = context.runtime().scopes().control(index).defers().to_vec();
    for defer in defers.into_iter().rev() {
        let unit = context.unit(defer.source());
        let body = unit
            .body(defer.expr().body())
            .expect("a deferred expression belongs to its source");
        let scopes = context.runtime_mut().scopes_mut();
        scopes.push(RuntimeScope::new(None, true), Some(index), Bindings::new());
        let result = context.relocated(defer.source(), defer.visible_through(), |context| {
            context.with_expectations(defer.expectations(), |context| {
                deferred(context, body, defer.expr())
            })
        });
        context.runtime_mut().scopes_mut().pop();
        if let Evaluation::Incomplete(incomplete) = result? {
            return Err(incomplete.error());
        }
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
