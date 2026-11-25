use chl_hmir::{
    HMIRBodyContext,
    body::HMIRLocalID,
    expr::{HMIRExprID, HMIRExprKind},
    ty::HMIRTypeID,
};
use chl_log::{
    CHLResult,
    catalogue::{mir, typecheck},
};
use chl_tokens::TokenRange;

use super::{Shape, captures, materialize, shape, signature};
use crate::{
    emit,
    env::function::{
        ComptimeContext, FnContext, LocalLoweringContext, comptime::ComptimeScope,
        runtime::RuntimeScope, scope::Bindings,
    },
    eval::{
        EvalResult, coerce, comptime, control,
        deduce::{self, Incomplete},
        eval, eval_type,
        flow::{self, Evaluation},
        types,
    },
    log::hmir_error,
    value::{HMIRValue, staged::Quote},
};

// Emitting code that is already being emitted changes nothing, so it stands as the code itself
pub(crate) fn eval_quote(
    context: &mut LocalLoweringContext,
    body: &HMIRBodyContext,
    params: &[HMIRLocalID],
    root: HMIRExprID,
    span: &TokenRange,
) -> EvalResult<Evaluation<HMIRValue>> {
    match context.is_runtime() && params.is_empty() {
        true => eval(context, body, root),
        false => capture(context, body, params, root, span),
    }
}

pub(super) fn capture(
    context: &mut LocalLoweringContext,
    body: &HMIRBodyContext,
    params: &[HMIRLocalID],
    root: HMIRExprID,
    span: &TokenRange,
) -> EvalResult<Evaluation<HMIRValue>> {
    let code = closure(context, body, params, root)?;
    let span = span.clone();
    let pending =
        Incomplete::synthesizing(
            context,
            &span.clone(),
            move |context, _, expected| match expected {
                Some(expected) => resolve(context, code, expected, &span),
                None => synthesized(context, code, &span),
            },
        );
    Ok(Evaluation::Incomplete(pending))
}

// The quote a closure makes where it is evaluated, before it is typed
pub(crate) fn closure(
    context: &mut LocalLoweringContext,
    body: &HMIRBodyContext,
    params: &[HMIRLocalID],
    root: HMIRExprID,
) -> CHLResult<Quote> {
    let captures = captures::collect(context, body, params, root)?;
    Ok(Quote::new(
        context.source(),
        root,
        params.to_vec(),
        captures,
    ))
}

// A quote with nothing expected of it is typed by its declared parameters and the result its body has with them
fn synthesized(
    context: &mut LocalLoweringContext,
    code: Quote,
    span: &TokenRange,
) -> EvalResult<Evaluation<HMIRValue>> {
    let declared = declared(context, &code, None)?;
    let Some(types) = declared.into_iter().collect::<Option<Vec<_>>>() else {
        return Err(hmir_error(span, &typecheck::CANNOT_INFER, "this type".into()).into());
    };
    let result = synthesize(context, &code, &types, span)?.ready()?;
    let ty = signature(context, types, result, span);
    Ok(Evaluation::Ready(HMIRValue::new(ty, code.into_kind())))
}

pub(crate) fn synthesize(
    context: &mut LocalLoweringContext,
    quote: &Quote,
    types: &[HMIRTypeID],
    span: &TokenRange,
) -> EvalResult<Evaluation<HMIRTypeID>> {
    deduce::typed(context, span, false, |context| {
        let mut args = Vec::with_capacity(types.len());
        for &param in types {
            args.push(deduce::placeholder(context, param, span)?);
        }
        let quote = deduce::shadowed(context, quote, span)?;
        expand(context, &quote, &args, None)
    })
}

fn resolve(
    context: &mut LocalLoweringContext,
    code: Quote,
    expected: HMIRTypeID,
    span: &TokenRange,
) -> EvalResult<Evaluation<HMIRValue>> {
    let (Shape { params, result }, insert) = match shape(context.env(), expected) {
        Some(shape) => (shape, false),
        None if !code.params().is_empty() => {
            let conversion = ("a staged closure".into(), types::name(context.env(), expected));
            return Err(hmir_error(span, &typecheck::INVALID_CONVERSION, conversion).into());
        }
        None => (
            Shape {
                params: Vec::new(),
                result: expected,
            },
            true,
        ),
    };
    if params.len() != code.params().len() {
        let count = (
            "staged expression".into(),
            params.len(),
            code.params().len(),
            false,
        );
        return Err(hmir_error(span, &typecheck::ARGUMENT_COUNT, count).into());
    }
    let declared = declared(context, &code, Some(&params))?;
    for (actual, expected) in declared.into_iter().zip(&params) {
        let Some(actual) = actual else {
            continue;
        };
        if !context.env().same_type(actual, *expected) {
            let names = (
                "staged parameter".into(),
                types::name(context.env(), *expected),
                types::name(context.env(), actual),
            );
            return Err(hmir_error(span, &typecheck::TYPE_MISMATCH, names).into());
        }
    }
    let ty = signature(context, params, result, span);
    let value = HMIRValue::new(ty, code.into_kind());
    match insert {
        true => materialize(context, value, span),
        false => Ok(Evaluation::Ready(value)),
    }
}

pub(crate) fn declared(
    context: &mut LocalLoweringContext,
    quote: &Quote,
    expected: Option<&[HMIRTypeID]>,
) -> CHLResult<Vec<Option<HMIRTypeID>>> {
    let unit = context.unit(quote.source());
    let body = unit
        .body(quote.root().body())
        .expect("a quoted body exists");
    let captures = quote
        .captures()
        .iter()
        .filter_map(|(local, value)| Some((*local, comptime::known(context, value)?)))
        .collect::<Vec<_>>();
    let evaluation = context.evaluation();
    let comptime = FnContext::Comptime(ComptimeContext::new());
    let mut child = context
        .child(None, comptime, Some(evaluation))
        .sourced(quote.source());
    for (local, value) in captures {
        emit::storage::bind_local(&mut child, local, value);
    }
    let mut types = Vec::with_capacity(quote.params().len());
    for (index, local) in quote.params().iter().enumerate() {
        let declared = body.local(*local).ty();
        if matches!(body.expr(declared).kind(), HMIRExprKind::Hole(_)) {
            types.push(None);
            continue;
        }
        let expected = expected.map(|expected| expected[index]);
        let ty = flow::top(
            eval_type(&mut child, body, declared)
                .and_then(|declared| declared.try_complete(&mut child, body, expected))
                .and_then(Evaluation::description),
        )?;
        types.push(Some(ty));
    }
    Ok(types)
}

pub(super) fn expand<V: Clone + Into<HMIRValue>>(
    context: &mut LocalLoweringContext,
    quote: &Quote<V>,
    args: &[HMIRValue],
    expected: Option<HMIRTypeID>,
) -> EvalResult<Evaluation<HMIRValue>> {
    let unit = context.unit(quote.source());
    let body = unit
        .body(quote.root().body())
        .expect("a quoted body exists");
    let span = body.expr(quote.root()).span();
    let bindings: Bindings = quote
        .captures()
        .iter()
        .map(|(&local, value)| (local, value.clone().into()))
        .collect();
    let allows_escape = context.expectations().allows_escape();
    match context.context_mut() {
        FnContext::Runtime(runtime) => {
            let sealed = !runtime.quoting() && !allows_escape;
            let scope = RuntimeScope::new(false).quoted(body.id(), sealed);
            runtime.scopes_mut().push(scope, None, bindings);
        }
        FnContext::Comptime(comptime) => {
            comptime.scopes_mut().push(ComptimeScope::quoted(), None, bindings);
        }
    }

    let source = quote.source();
    let completion = context.relocated(source, |context| {
        for (local, value) in quote.params().iter().zip(args.iter().cloned()) {
            emit::storage::bind_local(context, *local, value);
        }
        let span = span.clone();
        eval(context, body, quote.root())?
            .try_complete(context, body, expected)?
            .map(context, move |context, value| match expected {
                Some(expected) => coerce::implicit::standard(context, value, expected, &span),
                None => Ok(value),
            })
    });
    let completion = if context.is_comptime() {
        let span = span.clone();
        flow::catch(completion, move |_| {
            Err(hmir_error(&span, &mir::COMPTIME_CONTROL_ESCAPE, ()).into())
        })
    } else {
        completion
    };
    Ok(match control::closing(context, body, completion, span)? {
        Evaluation::Incomplete(pending) => {
            Evaluation::Incomplete(pending.in_source(source, quote.root().body()))
        }
        finished => finished,
    })
}
