mod captures;
mod materialize;

use cx_hmir::{
    HMIRBody, HMIRContract, HMIRFnSignature, HMIRFunctionStage,
    body::HMIRLocalID,
    expr::{HMIRExprID, HMIRExprKind},
    ty::{HMIRType, HMIRTypeID, HMIRTypeKind, context::HMIRTypeContext},
    unit::function::HMIRFnParam,
};
use cx_log::{CXResult, catalogue::typecheck};
use cx_tokens::TokenRange;

use crate::{
    emit,
    env::{
        HMIREnvironment,
        function::{ComptimeContext, FnContext, LocalLoweringContext},
    },
    eval::{
        EvalResult, coerce, comptime, eval, eval_type,
        flow::{self, Evaluation, Incomplete, IncompleteReason, ready},
        types,
    },
    log::hmir_error,
    value::{
        HMIRValue, HMIRValueKind,
        constant::HMIRConstant,
        staged::{Quote, QuoteRef},
    },
};

pub(crate) use captures::references;
pub(crate) use materialize::materialize;

fn signature(
    context: &mut LocalLoweringContext,
    params: Vec<HMIRTypeID>,
    result: HMIRTypeID,
    span: &TokenRange,
) -> HMIRTypeID {
    let mut realize = |kind| {
        let ty = HMIRType::new(kind, Default::default(), span.clone(), None);
        context.env_mut().resolver_mut().realize_type(ty)
    };
    let result = realize(HMIRTypeKind::Staged(result));
    if params.is_empty() {
        return result;
    }
    let params = params
        .into_iter()
        .map(|param| HMIRFnParam::new(false, realize(HMIRTypeKind::Staged(param))))
        .collect();
    let stage = HMIRFunctionStage::Comptime;
    let signature = HMIRFnSignature::new(stage, params, result, false, HMIRContract::default());
    realize(HMIRTypeKind::Function(signature))
}

// The parameters and result carried by a staged type, 'expr(T)' or a comptime function from code to code
pub(crate) fn shape(env: &HMIREnvironment, ty: HMIRTypeID) -> Option<(Vec<HMIRTypeID>, HMIRTypeID)> {
    let carried = |ty: HMIRTypeID| match env.resolve_kind(ty) {
        HMIRTypeKind::Staged(inner) => Some(*inner),
        _ => None,
    };
    let HMIRTypeKind::Function(signature) = env.resolve_kind(ty) else {
        return Some((Vec::new(), carried(ty)?));
    };
    if signature.stage() != HMIRFunctionStage::Comptime || signature.is_variadic() {
        return None;
    }
    let params = signature
        .params()
        .iter()
        .map(|param| carried(*param.ty()).filter(|_| !param.comptime()))
        .collect::<Option<_>>()?;
    Some((params, carried(*signature.return_type())?))
}

pub(crate) fn is_code(context: &LocalLoweringContext, ty: HMIRTypeID) -> bool {
    let ty = context.env().reference_inner(ty).unwrap_or(ty);
    shape(context.env(), ty).is_some()
}

pub(crate) fn splice(
    context: &mut LocalLoweringContext,
    value: HMIRValue,
    span: &TokenRange,
) -> EvalResult<HMIRValue> {
    let value = ready!(comptime::export(context, value, span));
    let staged = matches!(context.env().resolve_kind(value.ty()), HMIRTypeKind::Staged(_));
    match value.kind() {
        HMIRValueKind::Constant(HMIRConstant::Quote(_)) if staged => {
            materialize(context, &value, &[])
        }
        _ => Ok(Evaluation::Ready(value)),
    }
}

// A quote spliced where it is written takes its staged type from its arguments and the type
// expected of the splice
pub(crate) fn splice_quote(
    context: &mut LocalLoweringContext,
    quote: HMIRExprID,
    args: &[HMIRExprID],
    span: &TokenRange,
) -> EvalResult<HMIRValue> {
    let args = args.to_vec();
    let span = span.clone();
    let reason = IncompleteReason::ExpectedType;
    let pending = Incomplete::new(
        context,
        &span.clone(),
        reason,
        None,
        move |context, body, expected| {
            let mut params = Vec::new();
            for arg in &args {
                let value = ready!(eval(context, body, *arg));
                params.push(coerce::value_type(context, value.ty(), &span));
            }
            let ty = signature(context, params, expected, &span);
            let code = ready!(eval(context, body, quote)?.complete(context, body, Some(ty)));
            apply(context, body, code, &args, &span)
        },
    );
    Ok(Evaluation::Incomplete(pending))
}

pub(crate) fn capture(
    context: &mut LocalLoweringContext,
    body: &HMIRBody,
    params: &[HMIRLocalID],
    root: HMIRExprID,
) -> EvalResult<HMIRValue> {
    let captures = captures::collect(context, body, params, root)?;
    let quote = Quote::new(
        context.source(),
        root,
        params.to_vec(),
        captures,
        context.visible_through(),
    );
    let code = QuoteRef::new(quote);
    let span = body.expr(root).span().clone();
    let reason = IncompleteReason::ExpectedType;
    let pending = Incomplete::new(
        context,
        &span.clone(),
        reason,
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
) -> EvalResult<HMIRValue> {
    let (params, result, insert) = match shape(context.env(), expected) {
        Some((params, result)) => (params, result, false),
        None => (Vec::new(), expected, true),
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
        .sourced(quote.source())
        .at(quote.visible_through());
    for (local, value) in captures {
        emit::storage::bind_local(&mut child, local, value);
    }
    for (local, expected) in quote.params().iter().zip(expected) {
        let declared = body.local(*local).ty();
        if matches!(body.expr(declared).kind(), HMIRExprKind::Hole(_)) {
            continue;
        }
        let declared = eval_type(&mut child, body, declared)?;
        let actual = flow::top(
            declared
                .complete(&mut child, body, Some(*expected))?
                .description(),
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
        HMIRExprKind::Local(local)
            if context
                .get_local(local)
                .is_some_and(|value| is_code(context, value.ty())) =>
        {
            eval(context, body, operand)?
        }
        _ => capture(context, body, &[], operand)?,
    };
    let span = body.expr(operand).span().clone();
    value.complete(context, body, Some(target))?.and_then(
        context,
        body,
        None,
        move |context, _, value, _| {
            Ok(Evaluation::Ready(coerce::implicit(
                context, value, target, &span,
            )?))
        },
    )
}

pub(crate) fn read(
    context: &mut LocalLoweringContext,
    value: HMIRValue,
    span: &TokenRange,
) -> CXResult<HMIRValue> {
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
    let Some((params, result)) = shape(context.env(), code.ty()) else {
        let kind = ("applied value".into(), "a staged expression".into());
        return Err(hmir_error(span, &typecheck::UNEXPECTED_KIND, kind));
    };
    if params.len() != args.len() {
        let count = ("staged expression".into(), params.len(), args.len(), false);
        return Err(hmir_error(span, &typecheck::ARGUMENT_COUNT, count));
    }
    let runtime = matches!(context.context(), FnContext::Runtime(_));
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
    let applied = Quote::new(
        source.source(),
        source.root(),
        Vec::new(),
        captures,
        source.visible_through(),
    );
    let ty = signature(context, Vec::new(), result, span);
    Ok(Evaluation::Ready(HMIRValue::constant(
        ty,
        HMIRConstant::Quote(QuoteRef::new(applied)),
    )))
}
