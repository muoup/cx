mod captures;

pub(crate) use captures::references;

use cx_hmir::{
    HMIRBody, HMIRContract, HMIRFnSignature, HMIRFunctionStage,
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
        flow::{self, Evaluation, ready},
        types,
    },
    log::hmir_error,
    value::{
        HMIRValue, HMIRValueKind,
        constant::HMIRConstant,
        staged::{Quote, QuoteRef},
    },
};

// The parameters and result carried by a staged type, 'expr(T)' or a comptime function from code to code
struct Shape {
    params: Vec<HMIRTypeID>,
    result: HMIRTypeID,
}

fn shape(env: &HMIREnvironment, ty: HMIRTypeID) -> Option<Shape> {
    let carried = |ty: HMIRTypeID| match env.resolve_kind(ty) {
        HMIRTypeKind::Staged(inner) => Some(*inner),
        _ => None,
    };
    let HMIRTypeKind::Function(signature) = env.resolve_kind(ty) else {
        return Some(Shape { params: Vec::new(), result: carried(ty)? });
    };
    if signature.stage() != HMIRFunctionStage::Comptime || signature.is_variadic() {
        return None;
    }
    let params = signature
        .params()
        .iter()
        .map(|param| carried(*param.ty()).filter(|_| !param.comptime()))
        .collect::<Option<_>>()?;
    Some(Shape { params, result: carried(*signature.return_type())? })
}

fn signature(
    context: &mut LocalLoweringContext,
    params: Vec<HMIRTypeID>,
    result: HMIRTypeID,
    span: &TokenRange,
) -> HMIRTypeID {
    let result = types::realize(context.env_mut(), HMIRTypeKind::Staged(result), span);
    if params.is_empty() {
        return result;
    }
    let params = params
        .into_iter()
        .map(|param| HMIRFnParam::new(false, types::realize(context.env_mut(), HMIRTypeKind::Staged(param), span)))
        .collect();
    let signature = HMIRFnSignature::new(HMIRFunctionStage::Comptime, params, result, false, HMIRContract::default());
    types::realize(context.env_mut(), HMIRTypeKind::Function(signature), span)
}

pub(crate) fn is_code(context: &LocalLoweringContext, ty: HMIRTypeID) -> bool {
    shape(context.env(), context.env().referent(ty)).is_some()
}

pub(crate) fn eval_splice(
    context: &mut LocalLoweringContext,
    body: &HMIRBody,
    span: &TokenRange,
    quote: HMIRExprID,
    args: &[HMIRExprID],
) -> EvalResult<HMIRValue> {
    let literal = match body.expr(quote).kind() {
        HMIRExprKind::Comptime(inner) => *inner,
        _ => quote,
    };
    if !matches!(body.expr(literal).kind(), HMIRExprKind::Quote { .. }) {
        let code = ready!(eval(context, body, quote));
        return apply(context, body, code, args, span);
    }
    let (args, span) = (args.to_vec(), span.clone());
    let pending = Incomplete::new(context, &span.clone(), IncompleteReason::ExpectedType, None, move |context, body, expected| {
        let mut params = Vec::new();
        for arg in &args {
            let value = ready!(eval(context, body, *arg));
            params.push(coerce::value_type(context, value.ty(), &span));
        }
        let ty = signature(context, params, expected, &span);
        let code = ready!(eval(context, body, literal)?.complete(context, body, Some(ty)));
        apply(context, body, code, &args, &span)
    });
    Ok(Evaluation::Incomplete(pending))
}

pub(crate) fn splice(context: &mut LocalLoweringContext, value: HMIRValue, span: &TokenRange) -> EvalResult<HMIRValue> {
    let value = ready!(comptime::export(context, value, span));
    let staged = matches!(context.env().resolve_kind(value.ty()), HMIRTypeKind::Staged(_));
    match value.kind() {
        HMIRValueKind::Constant(HMIRConstant::Quote(_)) if staged => materialize(context, &value, &[]),
        _ => Ok(Evaluation::Ready(value)),
    }
}

pub(crate) fn capture(
    context: &mut LocalLoweringContext,
    body: &HMIRBody,
    params: &[HMIRLocalID],
    root: HMIRExprID,
) -> EvalResult<HMIRValue> {
    let captures = captures::collect(context, body, params, root)?;
    let quote = Quote::new(context.source(), root, params.to_vec(), captures, context.visible_through());
    let code = QuoteRef::new(quote);
    let span = body.expr(root).span().clone();
    let pending = Incomplete::new(context, &span.clone(), IncompleteReason::ExpectedType, None, move |context, _, expected| {
        resolve(context, code, expected, &span)
    });
    Ok(Evaluation::Incomplete(pending))
}

fn resolve(
    context: &mut LocalLoweringContext,
    code: QuoteRef,
    expected: HMIRTypeID,
    span: &TokenRange,
) -> EvalResult<HMIRValue> {
    let (Shape { params, result }, insert) = match shape(context.env(), expected) {
        Some(shape) => (shape, false),
        None => (Shape { params: Vec::new(), result: expected }, true),
    };
    let quoted = code.get().params().len();
    if params.len() != quoted {
        let count = ("staged expression".into(), params.len(), quoted, false);
        return Err(hmir_error(span, &typecheck::ARGUMENT_COUNT, count));
    }
    check_params(context, code.get(), &params, span)?;
    let ty = signature(context, params, result, span);
    let value = HMIRValue::constant(ty, HMIRConstant::Quote(code));
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
    let body = unit.body(quote.root().body()).expect("a quoted body exists");
    let captures = quote
        .captures()
        .iter()
        .filter_map(|(local, value)| Some((*local, comptime::known(context, value)?)))
        .collect::<Vec<_>>();
    let evaluation = context.evaluation();
    let comptime = FnContext::Comptime(ComptimeContext::new());
    let mut child = context
        .child(None, comptime, Some(evaluation))
        .sourced(quote.source())
        .at(quote.visible_through());
    for (local, value) in captures {
        emit::bind_local(&mut child, local, value);
    }
    for (local, expected) in quote.params().iter().zip(expected) {
        let declared = body.local(*local).ty();
        if matches!(body.expr(declared).kind(), HMIRExprKind::Hole(_)) {
            continue;
        }
        let declared = eval_type(&mut child, body, declared)?;
        let actual = flow::top(declared.complete(&mut child, body, Some(*expected))?.description())?;
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
    body: &HMIRBody,
    arg: HMIRExprID,
    target: HMIRTypeID,
) -> EvalResult<HMIRValue> {
    let mut operand = arg;
    while let HMIRExprKind::Piped(inner) = body.expr(operand).kind() {
        operand = *inner;
    }
    let value = match body.expr(operand).kind() {
        HMIRExprKind::Quote { .. } => eval(context, body, operand)?,
        HMIRExprKind::Local(local) if context.get_local(local).is_some_and(|value| is_code(context, value.ty())) => {
            eval(context, body, operand)?
        }
        _ => capture(context, body, &[], operand)?,
    };
    let span = body.expr(operand).span().clone();
    value
        .complete(context, body, Some(target))?
        .map(context, move |context, value| coerce::implicit(context, value, target, &span))
}

pub(crate) fn read(context: &mut LocalLoweringContext, value: HMIRValue, span: &TokenRange) -> CXResult<HMIRValue> {
    match context.env().is_reference(value.ty()) && is_code(context, value.ty()) {
        true => emit::copy(context, value, span),
        false => Ok(value),
    }
}

pub(crate) fn apply(
    context: &mut LocalLoweringContext,
    body: &HMIRBody,
    code: HMIRValue,
    args: &[HMIRExprID],
    span: &TokenRange,
) -> EvalResult<HMIRValue> {
    let Some(Shape { params, result }) = shape(context.env(), code.ty()) else {
        let kind = ("applied value".into(), "a staged expression".into());
        return Err(hmir_error(span, &typecheck::UNEXPECTED_KIND, kind));
    };
    if params.len() != args.len() {
        let count = ("staged expression".into(), params.len(), args.len(), false);
        return Err(hmir_error(span, &typecheck::ARGUMENT_COUNT, count));
    }
    let runtime = context.is_runtime();
    let mut values = Vec::new();
    for (arg, param) in args.iter().zip(params) {
        let value = match runtime {
            true => {
                let value = ready!(eval(context, body, *arg));
                coerce::implicit(context, value, param, body.expr(*arg).span())?
            }
            false => {
                let target = signature(context, Vec::new(), param, span);
                ready!(argument(context, body, *arg, target))
            }
        };
        values.push(value);
    }
    if runtime {
        return materialize(context, &code, &values);
    }
    let HMIRValueKind::Constant(HMIRConstant::Quote(node)) = code.kind() else {
        unreachable!("compile-time code is a quote")
    };
    let source = node.get();
    let mut captures = source.captures().clone();
    captures.extend(source.params().iter().copied().zip(values));
    let applied = Quote::new(source.source(), source.root(), Vec::new(), captures, source.visible_through());
    let ty = signature(context, Vec::new(), result, span);
    Ok(Evaluation::Ready(HMIRValue::constant(ty, HMIRConstant::Quote(QuoteRef::new(applied)))))
}

pub(crate) fn materialize(
    context: &mut LocalLoweringContext,
    code: &HMIRValue,
    args: &[HMIRValue],
) -> EvalResult<HMIRValue> {
    let HMIRValueKind::Constant(HMIRConstant::Quote(node)) = code.kind() else {
        let subject = "this staged value".into();
        return Err(hmir_error(&TokenRange::internal(), &mir::COMPTIME_UNAVAILABLE, subject));
    };
    let Shape { params, result } = shape(context.env(), code.ty()).expect("a quote has a staged type");
    let quote = node.get();
    let unit = context.unit(quote.source());
    let body = unit.body(quote.root().body()).expect("a quoted body exists");
    let span = body.expr(quote.root()).span();
    if params.len() != args.len() {
        let count = ("staged expression".into(), params.len(), args.len(), false);
        return Err(hmir_error(span, &typecheck::ARGUMENT_COUNT, count));
    }
    if !context.is_runtime() {
        let operation = "materializing runtime code at compile time".into();
        return Err(hmir_error(span, &mir::COMPTIME_INVALID_OPERATION, operation));
    }

    let result = (!context.env().is_void(result)).then_some(result);
    let mir = context.runtime_mut().builder_mut().add_scope(span.clone());
    let scope = RuntimeScope::new(Some(mir), false).quoted(body.id());
    context.runtime_mut().scopes_mut().push(scope, None, quote.captures().clone());

    let (source, point) = (quote.source(), quote.visible_through());
    let completion = context.relocated(source, point, |context| {
        for (local, (target, value)) in quote.params().iter().zip(params.into_iter().zip(args.iter().cloned())) {
            let value = coerce::implicit(context, value, target, span)?;
            let value = match context.env().is_reference(target) {
                true => value,
                false => {
                    let name = body.local(*local).name().cloned();
                    let reference = types::reference(context.env_mut(), target, None, span);
                    let storage = emit::allocate(context, target, name, span)?;
                    emit::initialize(context, &storage, Some(&value), span)?;
                    storage.with_ty(reference)
                }
            };
            emit::bind_local(context, *local, value);
        }
        let span = span.clone();
        eval(context, body, quote.root())?.complete(context, body, result)?.map(context, move |context, value| {
            match result {
                Some(result) => coerce::implicit(context, value, result, &span),
                None => Ok(expr::unit(context, &span)),
            }
        })
    });
    Ok(match control::closing(context, body, completion, span)? {
        Evaluation::Incomplete(pending) => Evaluation::Incomplete(pending.in_source(source, point, quote.root().body())),
        finished => finished,
    })
}
