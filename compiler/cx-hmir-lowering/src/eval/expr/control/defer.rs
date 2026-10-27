use cx_hmir::{
    HMIRBody,
    expr::{HMIRExprID, HMIRExprKind},
};
use cx_log::{
    CXResult,
    catalogue::{self, typecheck},
};
use cx_tokens::TokenRange;

use crate::{
    emit,
    env::function::{
        FnContext, LocalLoweringContext,
        runtime::{Defer, Emission, RuntimeScope},
    },
    eval::{
        EvalResult,
        binding::{eval_block, eval_scoped},
        deduce, expr,
        flow::{self, Evaluation},
    },
    log::hmir_error,
    lower::mir,
    value::{Bindings, HMIRValue},
};

pub(super) fn eval_defer(
    context: &mut LocalLoweringContext,
    body: &HMIRBody,
    span: &TokenRange,
    operand: HMIRExprID,
) -> EvalResult<HMIRValue> {
    if !context.is_runtime() {
        return Err(hmir_error(span, &catalogue::mir::COMPTIME_INVALID_OPERATION, "'defer'".into()));
    }
    if context.runtime().deferred() {
        let invalid = ("'defer'".into(), "a deferred expression".into());
        return Err(hmir_error(span, &typecheck::INVALID_CONTEXT, invalid));
    }
    let operand_span = body.expr(operand).span();
    let mut child = deduce::detached(context, Emission::Enabled, true, operand_span)?;
    match deferred(&mut child, body, operand)? {
        Evaluation::Ready(_) => {}
        Evaluation::Incomplete(incomplete) => return Err(incomplete.error()),
        Evaluation::Stop(_) => return Err(hmir_error(operand_span, &typecheck::DEFER_FALLTHROUGH, ())),
    }
    drop(child);

    let defer = Defer::new(operand, context.expectations(), context.source(), context.visible_through());
    let runtime = context.runtime_mut();
    let top = runtime.scopes().top();
    runtime.scopes_mut().control_mut(top).defer(defer);
    Ok(Evaluation::Ready(expr::unit(context, span)))
}

pub(crate) fn closing<T: 'static>(
    context: &mut LocalLoweringContext,
    body: &HMIRBody,
    result: EvalResult<T>,
    span: &TokenRange,
) -> EvalResult<T> {
    let span = span.clone();
    flow::finally(context, body, result, move |context, _, completed| {
        if completed && let FnContext::Runtime(runtime) = context.context() {
            expand(context, runtime.scopes().top())?;
        }
        emit::close_scope(context, &span);
        Ok(())
    })
}

pub(super) fn leave(context: &mut LocalLoweringContext, depth: usize, span: &TokenRange) -> CXResult<()> {
    for index in (depth..context.runtime().scopes().len()).rev() {
        expand(context, index)?;
        mir::drop_scope(context, index, span);
    }
    Ok(())
}

fn expand(context: &mut LocalLoweringContext, index: usize) -> CXResult<()> {
    let defers = context.runtime().scopes().control(index).defers().to_vec();
    for defer in defers.into_iter().rev() {
        let unit = context.unit(defer.source());
        let body = unit.body(defer.expr().body()).expect("a deferred expression belongs to its source");
        let scopes = context.runtime_mut().scopes_mut();
        scopes.push(RuntimeScope::new(None, true), Some(index), Bindings::new());
        let result = context.relocated(defer.source(), defer.visible_through(), |context| {
            context.with_expectations(defer.expectations(), |context| deferred(context, body, defer.expr()))
        });
        context.runtime_mut().scopes_mut().pop();
        if let Evaluation::Incomplete(incomplete) = result? {
            return Err(incomplete.error());
        }
    }
    Ok(())
}

fn deferred(context: &mut LocalLoweringContext, body: &HMIRBody, expr: HMIRExprID) -> EvalResult<HMIRValue> {
    let node = body.expr(expr);
    match node.kind() {
        HMIRExprKind::Block { statements, scoped: true, .. } => {
            eval_block(context, body, node.span(), statements, true, false)
        }
        _ => eval_scoped(context, body, expr),
    }
}
