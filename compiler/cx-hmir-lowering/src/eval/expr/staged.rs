mod captures;

pub(crate) use captures::references;

use cx_hmir::{
    HMIRBodyContext, HMIRContract, HMIRFnSignature, HMIRFunctionStage,
    body::HMIRLocalID,
    expr::{HMIRExprID, HMIRExprKind},
    ty::{HMIRTypeID, HMIRTypeKind, context::HMIRTypeContext},
    unit::function::HMIRFnParam,
};
use cx_log::{
    CXResult,
    catalogue::{mir, typecheck},
};
use cx_tokens::TokenRange;

use crate::{
    emit,
    env::{
        HMIREnvironment,
        function::{ComptimeContext, FnContext, LocalLoweringContext, runtime::RuntimeScope},
    },
    eval::{
        EvalResult, coerce, comptime, control,
        deduce::{self, Incomplete, IncompleteReason},
        eval, eval_type,
        flow::{self, EvalExit, Evaluation},
        types,
    },
    log::hmir_error,
    value::{
        self, HMIRValue, HMIRValueKind,
        application::HMIRApplication,
        immediate::HMIRImmValue,
        staged::{Quote, QuoteRef},
    },
};

// The parameters and result carried by a staged type, '^T' or a comptime function to code
struct Shape {
    params: Vec<HMIRTypeID>,
    result: HMIRTypeID,
}

fn shape(env: &HMIREnvironment, ty: HMIRTypeID) -> Option<Shape> {
    let carried = |ty: HMIRTypeID| match env.resolve_kind(ty) {
        HMIRTypeKind::Staged(inner) => Some(*inner),
        _ => None,
    };
    let HMIRTypeKind::Function { signature, .. } = env.resolve_kind(ty) else {
        return Some(Shape {
            params: Vec::new(),
            result: carried(ty)?,
        });
    };
    if signature.stage() != HMIRFunctionStage::Comptime || signature.is_variadic() {
        return None;
    }
    Some(Shape {
        params: signature.params().iter().map(|param| *param.ty()).collect(),
        result: carried(*signature.return_type())?,
    })
}

// There is no stage above comptime, so staging staged code leaves it as it is
pub(crate) fn lift(env: &mut HMIREnvironment, inner: HMIRTypeID, span: &TokenRange) -> HMIRTypeID {
    match env.resolve_kind(inner) {
        HMIRTypeKind::Staged(_) => inner,
        _ => types::realize(env, HMIRTypeKind::Staged(inner), span),
    }
}

fn signature(
    context: &mut LocalLoweringContext,
    params: Vec<HMIRTypeID>,
    result: HMIRTypeID,
    span: &TokenRange,
) -> HMIRTypeID {
    let result = lift(context.env_mut(), result, span);
    if params.is_empty() {
        return result;
    }
    let params = params
        .into_iter()
        .map(|param| HMIRFnParam::new(false, param))
        .collect();
    let signature = HMIRFnSignature::new(
        HMIRFunctionStage::Comptime,
        params,
        result,
        false,
        HMIRContract::default(),
    );
    types::realize(
        context.env_mut(),
        HMIRTypeKind::Function {
            signature,
            decays: false,
        },
        span,
    )
}

pub(crate) fn is_code(context: &LocalLoweringContext, ty: HMIRTypeID) -> bool {
    shape(context.env(), context.env().referent(ty)).is_some()
}

pub(crate) fn adapts(env: &HMIREnvironment, underlying: HMIRTypeID, exposed: HMIRTypeID) -> bool {
    let HMIRTypeKind::Function { signature, .. } = env.resolve_kind(env.referent(underlying))
    else {
        return false;
    };
    signature.stage() == HMIRFunctionStage::Runtime
        && !matches!(
            env.resolve_kind(*signature.return_type()),
            HMIRTypeKind::Staged(_)
        )
        && shape(env, env.referent(exposed)).is_some()
}

pub(crate) fn applies(context: &LocalLoweringContext, value: &HMIRValue) -> bool {
    let env = context.env();
    if matches!(env.resolve_kind(value.ty()), HMIRTypeKind::Staged(_)) {
        return true;
    }
    if let Some(application) = value::as_application(value) {
        return adapts(env, application.callee().ty(), value.ty())
            || applies(context, application.callee());
    }
    matches!(
        value.kind(),
        HMIRValueKind::Immediate(HMIRImmValue::Quote(_) | HMIRImmValue::Opaque)
    )
}

pub(crate) fn eval_splice(
    context: &mut LocalLoweringContext,
    body: &HMIRBodyContext,
    span: &TokenRange,
    quote: HMIRExprID,
    args: &[HMIRExprID],
) -> EvalResult<Evaluation<HMIRValue>> {
    let literal = match body.expr(quote).kind() {
        HMIRExprKind::Comptime(inner) => *inner,
        _ => quote,
    };
    let HMIRExprKind::Quote {
        params: quoted,
        body: root,
    } = body.expr(literal).kind()
    else {
        let code = (eval(context, body, quote))?.ready()?;
        let code = comptime::converge(context, code, span)?;
        return apply(context, body, code, args, span);
    };
    let (quoted, root) = (quoted.clone(), *root);
    let (args, span) = (args.to_vec(), span.clone());
    let pending = Incomplete::new(
        context,
        &span.clone(),
        IncompleteReason::ExpectedType,
        None,
        move |context, body, expected| {
            let mut params = Vec::new();
            for arg in &args {
                let ty = deduce::subject(context, body, *arg)?.ready()?;
                let ty = types::qualify::value(context, ty, &span);
                params.push(lift(context.env_mut(), ty, &span));
            }
            let ty = signature(context, params, expected, &span);
            let code =
                self::quote(context, body, &quoted, root)?.complete(context, body, Some(ty))?;
            apply(context, body, code, &args, &span)
        },
    );
    Ok(Evaluation::Incomplete(pending))
}

pub(crate) fn splice(
    context: &mut LocalLoweringContext,
    value: HMIRValue,
    span: &TokenRange,
) -> EvalResult<Evaluation<HMIRValue>> {
    let value = if context.is_runtime() && value::as_application(&value).is_some() {
        value
    } else {
        comptime::converge(context, value, span)?
    };
    let staged = matches!(
        context.env().resolve_kind(value.ty()),
        HMIRTypeKind::Staged(_)
    );
    match value.kind() {
        HMIRValueKind::Immediate(HMIRImmValue::Opaque) => Ok(Evaluation::Ready(value)),
        _ if staged => materialize(context, &value, &[], span),
        _ => Ok(Evaluation::Ready(value)),
    }
}

// Emitting code that is already being emitted changes nothing, so it stands as the code itself
pub(crate) fn eval_quote(
    context: &mut LocalLoweringContext,
    body: &HMIRBodyContext,
    params: &[HMIRLocalID],
    root: HMIRExprID,
) -> EvalResult<Evaluation<HMIRValue>> {
    match context.is_runtime() && params.is_empty() {
        true => eval(context, body, root),
        false => quote(context, body, params, root),
    }
}

fn quote(
    context: &mut LocalLoweringContext,
    body: &HMIRBodyContext,
    params: &[HMIRLocalID],
    root: HMIRExprID,
) -> EvalResult<Evaluation<HMIRValue>> {
    let code = QuoteRef::new(closure(context, body, params, root)?);
    let span = body.expr(root).span().clone();
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
) -> CXResult<Quote> {
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
    code: QuoteRef,
    span: &TokenRange,
) -> EvalResult<Evaluation<HMIRValue>> {
    let declared = declared(context, code.get(), None)?;
    let Some(types) = declared.into_iter().collect::<Option<Vec<_>>>() else {
        return Err(hmir_error(span, &typecheck::CANNOT_INFER, "this type".into()).into());
    };
    let result = synthesize(context, code.get(), &types, span)?.ready()?;
    let ty = signature(context, types, result, span);
    Ok(Evaluation::Ready(HMIRValue::new(
        ty,
        HMIRImmValue::Quote(code),
    )))
}

pub(crate) fn synthesize(
    context: &mut LocalLoweringContext,
    quote: &Quote,
    types: &[HMIRTypeID],
    span: &TokenRange,
) -> EvalResult<Evaluation<HMIRTypeID>> {
    deduce::typed(context, span, false, |context| {
        let mut args = Vec::with_capacity(types.len());
        for param in types {
            let ty = match context.env().resolve_kind(*param) {
                HMIRTypeKind::Staged(inner) => *inner,
                _ => *param,
            };
            args.push(deduce::placeholder(context, ty, span)?);
        }
        let quote = deduce::shadowed(context, quote, span)?;
        expand(context, &quote, &args, None)
    })
}

fn resolve(
    context: &mut LocalLoweringContext,
    code: QuoteRef,
    expected: HMIRTypeID,
    span: &TokenRange,
) -> EvalResult<Evaluation<HMIRValue>> {
    let (Shape { params, result }, insert) = match shape(context.env(), expected) {
        Some(shape) => (shape, false),
        None => (
            Shape {
                params: Vec::new(),
                result: expected,
            },
            true,
        ),
    };
    if params.len() != code.get().params().len() {
        let count = (
            "staged expression".into(),
            params.len(),
            code.get().params().len(),
            false,
        );
        return Err(hmir_error(span, &typecheck::ARGUMENT_COUNT, count).into());
    }
    check_params(context, code.get(), &params, span)?;
    let ty = signature(context, params, result, span);
    let value = HMIRValue::new(ty, HMIRImmValue::Quote(code));
    match insert {
        true => materialize(context, &value, &[], span),
        false => Ok(Evaluation::Ready(value)),
    }
}

fn check_params(
    context: &mut LocalLoweringContext,
    quote: &Quote,
    expected: &[HMIRTypeID],
    span: &TokenRange,
) -> CXResult<()> {
    let declared = declared(context, quote, Some(expected))?;
    for (actual, expected) in declared.into_iter().zip(expected) {
        let Some(actual) = actual else {
            continue;
        };
        if !context.env().same_type(actual, *expected) {
            let names = (
                "staged parameter".into(),
                types::name(context.env(), *expected),
                types::name(context.env(), actual),
            );
            return Err(hmir_error(span, &typecheck::TYPE_MISMATCH, names));
        }
    }
    Ok(())
}

pub(crate) fn declared(
    context: &mut LocalLoweringContext,
    quote: &Quote,
    expected: Option<&[HMIRTypeID]>,
) -> CXResult<Vec<Option<HMIRTypeID>>> {
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

pub(crate) fn argument(
    context: &mut LocalLoweringContext,
    body: &HMIRBodyContext,
    arg: HMIRExprID,
    target: HMIRTypeID,
) -> EvalResult<Evaluation<HMIRValue>> {
    let mut operand = arg;
    let mut piped = false;
    while let HMIRExprKind::Piped(inner) = body.expr(operand).kind() {
        operand = *inner;
        piped = true;
    }
    let value = match body.expr(operand).kind() {
        HMIRExprKind::Quote { params, body: root } => quote(context, body, params, *root)?,
        _ if !matches!(context.env().resolve_kind(target), HMIRTypeKind::Staged(_)) => {
            eval(context, body, operand)?
        }
        _ if piped => quote(context, body, &[], operand)?,
        _ => comptime::freeze(context, body, operand, Some(target)).map_err(|exit| match exit {
            EvalExit::Diagnostic(error) => {
                let hint = "to pass it as runtime code, write 'emit' before it or pipe it in";
                EvalExit::Diagnostic(error.with_note(hint, None))
            }
            exit => exit,
        })?,
    };
    let span = body.expr(operand).span().clone();
    value
        .try_complete(context, body, Some(target))?
        .map(context, move |context, value| {
            coerce::implicit::standard(context, value, target, &span)
        })
}

pub(crate) fn apply(
    context: &mut LocalLoweringContext,
    body: &HMIRBodyContext,
    code: HMIRValue,
    args: &[HMIRExprID],
    span: &TokenRange,
) -> EvalResult<Evaluation<HMIRValue>> {
    let Some(Shape { params, result }) = shape(context.env(), code.ty()) else {
        let kind = ("applied value".into(), "a staged expression".into());
        return Err(hmir_error(span, &typecheck::UNEXPECTED_KIND, kind).into());
    };
    if args.len() > params.len() || (args.is_empty() && !params.is_empty()) {
        let count = ("staged expression".into(), params.len(), args.len(), false);
        return Err(hmir_error(span, &typecheck::ARGUMENT_COUNT, count).into());
    }
    let mut values = Vec::new();
    for (arg, param) in args.iter().zip(params.iter().copied()) {
        let value = match is_code(context, param) {
            true => argument(context, body, *arg, param)?,
            false => comptime::freeze(context, body, *arg, Some(param))?,
        };
        values.push(value.ready()?);
    }
    if matches!(code.kind(), HMIRValueKind::Immediate(HMIRImmValue::Opaque)) {
        let ty = signature(context, params[values.len()..].to_vec(), result, span);
        let value = HMIRValue::new(ty, HMIRImmValue::Opaque);
        return match context.is_runtime() && values.len() == params.len() {
            true => materialize(context, &value, &[], span),
            false => Ok(Evaluation::Ready(value)),
        };
    }
    let supplied = values.len();
    let mut application = value::as_application(&code)
        .unwrap_or_else(|| HMIRApplication::new(code, vec![None; params.len()]));
    application.bind(values).map_err(|remaining| {
        hmir_error(
            span,
            &typecheck::ARGUMENT_COUNT,
            ("staged expression".into(), remaining, supplied, false),
        )
    })?;
    let ty = signature(context, params[supplied..].to_vec(), result, span);
    let value = value::application(ty, application);
    match context.is_runtime() && supplied == params.len() {
        true => materialize(context, &value, &[], span),
        false => Ok(Evaluation::Ready(value)),
    }
}

pub(crate) fn materialize(
    context: &mut LocalLoweringContext,
    code: &HMIRValue,
    args: &[HMIRValue],
    span: &TokenRange,
) -> EvalResult<Evaluation<HMIRValue>> {
    let Shape { params, result } =
        shape(context.env(), code.ty()).expect("materialized code has a staged type");
    if params.len() != args.len() {
        let count = ("staged expression".into(), params.len(), args.len(), false);
        return Err(hmir_error(span, &typecheck::ARGUMENT_COUNT, count).into());
    }
    if matches!(code.kind(), HMIRValueKind::Immediate(HMIRImmValue::Opaque)) {
        return opaque(context, result);
    }
    let (callee, values) = match value::as_application(code) {
        Some(mut application) => {
            application.bind(args.to_vec()).map_err(|remaining| {
                hmir_error(
                    span,
                    &typecheck::ARGUMENT_COUNT,
                    ("staged expression".into(), remaining, args.len(), false),
                )
            })?;
            if application.remaining() != 0 {
                let count = (
                    "staged expression".into(),
                    application.remaining(),
                    0,
                    false,
                );
                return Err(hmir_error(span, &typecheck::ARGUMENT_COUNT, count).into());
            }
            let (callee, values) = application.into_parts();
            (callee, values.into_iter().flatten().collect::<Vec<_>>())
        }
        None => match code.kind() {
            HMIRValueKind::Immediate(HMIRImmValue::Quote(_)) => (code.clone(), args.to_vec()),
            HMIRValueKind::Immediate(immediate) if params.is_empty() => {
                return Ok(Evaluation::Ready(HMIRValue::new(result, immediate.clone())));
            }
            _ => {
                return Err(hmir_error(
                    span,
                    &mir::COMPTIME_UNAVAILABLE,
                    "this staged value".into(),
                )
                .into());
            }
        },
    };
    if !context.is_runtime() {
        return Err(hmir_error(
            span,
            &mir::COMPTIME_INVALID_OPERATION,
            "materializing runtime code at compile time".into(),
        )
        .into());
    }
    let evaluated = match callee.kind() {
        HMIRValueKind::Immediate(HMIRImmValue::Quote(node)) => {
            let quote = node.get();
            let expected = (!context.env().is_void(result)).then_some(result);
            expand(context, quote, &values, expected)?
        }
        _ => super::call::hmir_call(context, span, callee, values)?,
    };
    let span = span.clone();
    evaluated.map(context, move |context, value| {
        if context.env().is_void(result) {
            Ok(value::unit(context.env()))
        } else {
            coerce::implicit::standard(context, value, result, &span)
        }
    })
}

// Unknown code stands for an unknown value of its result
fn opaque(
    context: &mut LocalLoweringContext,
    result: HMIRTypeID,
) -> EvalResult<Evaluation<HMIRValue>> {
    if context.env().is_void(result) {
        return Ok(Evaluation::Ready(value::unit(context.env())));
    }
    let value = deduce::placeholder(context, result, &TokenRange::internal())?;
    Ok(Evaluation::Ready(value))
}

fn expand(
    context: &mut LocalLoweringContext,
    quote: &Quote,
    args: &[HMIRValue],
    expected: Option<HMIRTypeID>,
) -> EvalResult<Evaluation<HMIRValue>> {
    let unit = context.unit(quote.source());
    let body = unit
        .body(quote.root().body())
        .expect("a quoted body exists");
    let span = body.expr(quote.root()).span();
    let sealed = !context.runtime().quoting() && !context.expectations().allows_escape();
    let scope = RuntimeScope::new(false).quoted(body.id(), sealed);
    context
        .runtime_mut()
        .scopes_mut()
        .push(scope, None, quote.captures().clone());

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
    Ok(match control::closing(context, body, completion, span)? {
        Evaluation::Incomplete(pending) => {
            Evaluation::Incomplete(pending.in_source(source, quote.root().body()))
        }
        finished => finished,
    })
}
