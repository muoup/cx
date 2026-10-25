use cx_hmir::{
    HMIRBody,
    expr::HMIRExprKind,
    ty::{HMIRTypeID, HMIRTypeKind, context::HMIRTypeContext},
};
use cx_log::catalogue::{mir, typecheck};
use cx_tokens::TokenRange;

use crate::{
    emit::{self, runtime},
    env::function::{FnContext, LocalLoweringContext, runtime::RuntimeScope},
    eval::{
        EvalResult,
        binding::eval_block,
        coerce, control, defer, eval,
        flow::{self, Evaluation, RuntimeStop, StopReason},
        literal, storage, types,
    },
    log::hmir_error,
    value::{HMIRValue, HMIRValueKind, constant::HMIRConstant, staged::Quote},
};

pub(crate) fn materialize(
    context: &mut LocalLoweringContext,
    code: &HMIRValue,
    args: &[HMIRValue],
) -> EvalResult<HMIRValue> {
    let HMIRValueKind::Constant(HMIRConstant::Quote(node)) = code.kind() else {
        let subject = "this staged value".into();
        return Err(hmir_error(
            &TokenRange::internal(),
            &mir::COMPTIME_UNAVAILABLE,
            subject,
        ));
    };
    let (params, result) = super::shape(context.env(), code.ty()).expect("a quote has a staged type");
    let quote = node.get();
    let unit = context.unit(quote.source());
    let body = unit
        .body(quote.root().body())
        .expect("a quoted body exists");
    let span = body.expr(quote.root()).span().clone();
    if params.len() != args.len() {
        let count = ("staged expression".into(), params.len(), args.len(), false);
        return Err(hmir_error(&span, &typecheck::ARGUMENT_COUNT, count));
    }
    if !matches!(context.context(), FnContext::Runtime(_)) {
        let operation = "materializing runtime code at compile time".into();
        return Err(hmir_error(
            &span,
            &mir::COMPTIME_INVALID_OPERATION,
            operation,
        ));
    }

    let void = matches!(context.env().resolve_kind(result), HMIRTypeKind::Void);
    let mir = context.runtime_mut().builder_mut().add_scope(span.clone());
    let mut scope = RuntimeScope::new(Some(mir), false);
    if !void {
        scope.begin_yield();
    }
    let scopes = context.runtime_mut().scopes_mut();
    let index = scopes.push(scope, None, quote.captures().clone());

    let (source, point) = (quote.source(), quote.visible_through());
    let completion = context.relocated(source, point, |context| {
        let typed = params.into_iter().zip(args.iter().cloned());
        evaluate(
            context,
            body,
            quote,
            typed,
            (!void).then_some(result),
            &span,
        )
    });
    let completion = conclude(context, index, completion, span.clone());
    let finished = flow::finally(context, body, completion, move |context, _, completed| {
        defer::close(context, completed, &span)
    })?;
    Ok(match finished {
        Evaluation::Incomplete(pending) => {
            Evaluation::Incomplete(pending.in_source(source, point, quote.root().body()))
        }
        finished => finished,
    })
}

fn evaluate(
    context: &mut LocalLoweringContext,
    body: &HMIRBody,
    quote: &Quote,
    args: impl Iterator<Item = (HMIRTypeID, HMIRValue)>,
    result: Option<HMIRTypeID>,
    span: &TokenRange,
) -> EvalResult<HMIRValue> {
    for (local, (target, value)) in quote.params().iter().zip(args) {
        let value = coerce::implicit(context, value, target, span)?;
        let value = match context.env().is_reference(target) {
            true => value,
            false => {
                let name = body.local(*local).name().cloned();
                let storage = emit::allocate(context, target, name, span)?;
                storage::initialize(context, &storage, &value, span)?;
                let ty = types::reference(context.env_mut(), target, None, span);
                storage.with_ty(ty)
            }
        };
        emit::storage::bind_local(context, *local, value);
    }

    let root = quote.root();
    let value = match body.expr(root).kind() {
        HMIRExprKind::Block {
            statements, scoped, ..
        } => eval_block(context, body, span, statements, *scoped, false),
        _ => eval(context, body, root),
    };
    let span = span.clone();
    value?.complete(context, body, result)?.and_then(
        context,
        body,
        None,
        move |context, _, value, _| {
            let value = match result {
                Some(result) => coerce::implicit(context, value, result, &span)?,
                None => literal::unit(context, &span),
            };
            Ok(Evaluation::Ready(value))
        },
    )
}

fn conclude(
    context: &mut LocalLoweringContext,
    index: usize,
    completion: EvalResult<HMIRValue>,
    span: TokenRange,
) -> EvalResult<HMIRValue> {
    let completion = match completion {
        Ok(Evaluation::Incomplete(pending)) => {
            let pending = pending
                .settle(move |context, _, completion| conclude(context, index, completion, span));
            return Ok(Evaluation::Incomplete(pending));
        }
        completion => completion,
    };
    if !context.runtime().scopes().control(index).yields() {
        return completion;
    }
    let block = runtime::current_block(context);
    let mut arms = context
        .runtime_mut()
        .scopes_mut()
        .control_mut(index)
        .end_yield();
    if arms.is_empty() {
        return completion;
    }
    match completion? {
        Evaluation::Ready(value) => arms.push((block, value)),
        Evaluation::Stop(StopReason::Runtime(RuntimeStop::Diverged)) => {}
        stopped => return Ok(stopped),
    }
    control::merge(context, arms, &span)
}
