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
        deduce::{Incomplete, IncompleteReason},
        eval, eval_type, expr,
        flow::{self, Evaluation},
        types,
    },
    log::hmir_error,
    value::{
        self, HMIRValue, HMIRValueKind,
        immediate::HMIRImmediate,
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
    let params = params.into_iter().map(|param| HMIRFnParam::new(false, param)).collect();
    let signature = HMIRFnSignature::new(
        HMIRFunctionStage::Comptime,
        params,
        result,
        false,
        HMIRContract::default(),
    );
    types::realize(context.env_mut(), HMIRTypeKind::Function { signature, decays: false }, span)
}

pub(crate) fn is_code(context: &LocalLoweringContext, ty: HMIRTypeID) -> bool {
    shape(context.env(), context.env().referent(ty)).is_some()
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
    let HMIRExprKind::Quote { params: quoted, body: root } = body.expr(literal).kind() else {
        let code = (eval(context, body, quote))?.ready()?;
        let code = comptime::converge(context, code, span)?;
        let staged = matches!(context.env().resolve_kind(code.ty()), HMIRTypeKind::Staged(_));
        if args.is_empty() && !staged {
            return Ok(Evaluation::Ready(code));
        }
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
                let value = (eval(context, body, *arg))?.ready()?;
                let ty = types::qualify::value(context, value.ty(), &span);
                params.push(lift(context.env_mut(), ty, &span));
            }
            let ty = signature(context, params, expected, &span);
            let code = self::quote(context, body, &quoted, root)?.complete(context, body, Some(ty))?;
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
    let value = comptime::converge(context, value, span)?;
    let staged = matches!(
        context.env().resolve_kind(value.ty()),
        HMIRTypeKind::Staged(_)
    );
    match value.kind() {
        HMIRValueKind::Immediate(HMIRImmediate::Quote(_)) if staged => {
            materialize(context, &value, &[])
        }
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
    let captures = captures::collect(context, body, params, root)?;
    let quote = Quote::new(context.source(), root, params.to_vec(), captures);
    let code = QuoteRef::new(quote);
    let span = body.expr(root).span().clone();
    let pending = Incomplete::new(
        context,
        &span.clone(),
        IncompleteReason::ExpectedType,
        None,
        move |context, _, expected| resolve(context, code, expected, &span),
    );
    Ok(Evaluation::Incomplete(pending))
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
    let quoted = code.get().params().len();
    if params.len() != quoted {
        let count = ("staged expression".into(), params.len(), quoted, false);
        return Err(hmir_error(span, &typecheck::ARGUMENT_COUNT, count).into());
    }
    check_params(context, code.get(), &params, span)?;
    let ty = signature(context, params, result, span);
    let value = HMIRValue::new(ty, HMIRImmediate::Quote(code));
    match insert {
        true => materialize(context, &value, &[]),
        false => Ok(Evaluation::Ready(value)),
    }
}

fn check_params(
    context: &mut LocalLoweringContext,
    quote: &Quote,
    expected: &[HMIRTypeID],
    span: &TokenRange,
) -> CXResult<()> {
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
    for (local, expected) in quote.params().iter().zip(expected) {
        let declared = body.local(*local).ty();
        if matches!(body.expr(declared).kind(), HMIRExprKind::Hole(_)) {
            continue;
        }
        let actual = flow::top(
            eval_type(&mut child, body, declared)
                .and_then(|declared| declared.try_complete(&mut child, body, Some(*expected)))
                .and_then(Evaluation::description),
        )?;
        if !child.env().same_type(actual, *expected) {
            let names = (
                "staged parameter".into(),
                types::name(child.env(), *expected),
                types::name(child.env(), actual),
            );
            return Err(hmir_error(span, &typecheck::TYPE_MISMATCH, names));
        }
    }
    Ok(())
}

pub(crate) fn argument(
    context: &mut LocalLoweringContext,
    body: &HMIRBodyContext,
    arg: HMIRExprID,
    target: HMIRTypeID,
) -> EvalResult<Evaluation<HMIRValue>> {
    let mut operand = arg;
    while let HMIRExprKind::Piped(inner) = body.expr(operand).kind() {
        operand = *inner;
    }
    let value = match body.expr(operand).kind() {
        HMIRExprKind::Quote { params, body: root } => quote(context, body, params, *root)?,
        _ if !matches!(context.env().resolve_kind(target), HMIRTypeKind::Staged(_)) => {
            eval(context, body, operand)?
        }
        HMIRExprKind::Local(local)
            if context
                .get_local(local)
                .is_some_and(|value| is_code(context, value.ty())) =>
        {
            eval(context, body, operand)?
        }
        _ => quote(context, body, &[], operand)?,
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
    if params.len() != args.len() {
        let count = ("staged expression".into(), params.len(), args.len(), false);
        return Err(hmir_error(span, &typecheck::ARGUMENT_COUNT, count).into());
    }

    // Code is spliced where it is used; any other argument is evaluated once, at compile time
    let mut values = Vec::new();
    for (arg, param) in args.iter().zip(params) {
        let value = match is_code(context, param) {
            true => argument(context, body, *arg, param)?,
            false => comptime::freeze(context, body, *arg, Some(param))?,
        };
        values.push(value.ready()?);
    }
    if context.is_runtime() {
        return materialize(context, &code, &values);
    }
    let HMIRValueKind::Immediate(HMIRImmediate::Quote(node)) = code.kind() else {
        unreachable!("compile-time code is a quote")
    };
    let source = node.get();
    let mut captures = source.captures().clone();
    captures.extend(source.params().iter().copied().zip(values));
    let applied = Quote::new(source.source(), source.root(), Vec::new(), captures);
    let ty = signature(context, Vec::new(), result, span);
    Ok(Evaluation::Ready(HMIRValue::new(
        ty,
        HMIRImmediate::Quote(QuoteRef::new(applied)),
    )))
}

pub(crate) fn materialize(
    context: &mut LocalLoweringContext,
    code: &HMIRValue,
    args: &[HMIRValue],
) -> EvalResult<Evaluation<HMIRValue>> {
    let HMIRValueKind::Immediate(HMIRImmediate::Quote(node)) = code.kind() else {
        let subject = "this staged value".into();
        return Err(
            hmir_error(&TokenRange::internal(), &mir::COMPTIME_UNAVAILABLE, subject).into(),
        );
    };
    let Shape { params, result } =
        shape(context.env(), code.ty()).expect("a quote has a staged type");
    let quote = node.get();
    let unit = context.unit(quote.source());
    let body = unit
        .body(quote.root().body())
        .expect("a quoted body exists");
    let span = body.expr(quote.root()).span();
    if params.len() != args.len() {
        let count = ("staged expression".into(), params.len(), args.len(), false);
        return Err(hmir_error(span, &typecheck::ARGUMENT_COUNT, count).into());
    }
    if !context.is_runtime() {
        let operation = "materializing runtime code at compile time".into();
        return Err(hmir_error(span, &mir::COMPTIME_INVALID_OPERATION, operation).into());
    }

    let result = (!context.env().is_void(result)).then_some(result);
    let scope = RuntimeScope::new(false).quoted(body.id());
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
            .try_complete(context, body, result)?
            .map(context, move |context, value| match result {
                Some(result) => coerce::implicit::standard(context, value, result, &span),
                None => Ok(value::unit(context.env())),
            })
    });
    Ok(match control::closing(context, body, completion, span)? {
        Evaluation::Incomplete(pending) => {
            Evaluation::Incomplete(pending.in_source(source, quote.root().body()))
        }
        finished => finished,
    })
}
