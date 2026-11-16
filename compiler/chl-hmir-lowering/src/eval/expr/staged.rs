mod captures;
mod quote;

pub(crate) use captures::references;
pub(crate) use quote::{closure, declared, eval_quote, synthesize};

use chl_hmir::{
    HMIRBodyContext, HMIRContract, HMIRFnSignature, HMIRFunctionStage,
    expr::{HMIRExprID, HMIRExprKind},
    ty::{HMIRTypeID, HMIRTypeKind, context::HMIRTypeContext},
    unit::function::HMIRFnParam,
};
use chl_log::catalogue::{mir, typecheck};
use chl_tokens::TokenRange;

use crate::{
    env::{HMIREnvironment, function::LocalLoweringContext},
    eval::{
        EvalResult, coerce, comptime,
        deduce::{self, Incomplete, IncompleteReason},
        eval,
        flow::{EvalExit, Evaluation},
        types,
    },
    log::hmir_error,
    value::{self, HMIRApplication, HMIRValue, HMIRValueKind, immediate::HMIRImmValue},
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
    let mut immediate = match value.kind() {
        HMIRValueKind::Application(application) => {
            return adapts(env, application.callee.ty(), value.ty())
                || applies(context, &application.callee);
        }
        HMIRValueKind::Immediate(immediate) => immediate,
        _ => return false,
    };
    let mut ty = value.ty();
    while let HMIRImmValue::Application(application) = immediate {
        let callee = &application.callee;
        if adapts(env, callee.ty(), ty)
            || matches!(env.resolve_kind(callee.ty()), HMIRTypeKind::Staged(_))
        {
            return true;
        }
        ty = callee.ty();
        immediate = callee.value();
    }
    matches!(immediate, HMIRImmValue::Quote(_) | HMIRImmValue::Opaque)
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
                quote::capture(context, body, &quoted, root, body.expr(literal).span())?
                    .complete(context, body, Some(ty))?;
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
    let value = if context.is_runtime()
        && matches!(
            value.kind(),
            HMIRValueKind::Application(_) | HMIRValueKind::Immediate(HMIRImmValue::Application(_))
        ) {
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
        _ if staged => materialize(context, value, span),
        _ => Ok(Evaluation::Ready(value)),
    }
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
        HMIRExprKind::Quote { params, body: root } => {
            quote::capture(context, body, params, *root, body.expr(operand).span())?
        }
        _ if !matches!(context.env().resolve_kind(target), HMIRTypeKind::Staged(_)) => {
            eval(context, body, operand)?
        }
        _ if piped => quote::capture(context, body, &[], operand, body.expr(operand).span())?,
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
    let full = args.len() == params.len();
    let ty = signature(context, params[args.len()..].to_vec(), result, span);
    let value = if args.is_empty() {
        code.with_ty(ty)
    } else {
        let opaque = matches!(code.kind(), HMIRValueKind::Immediate(HMIRImmValue::Opaque));
        let mut application =
            value::into_application(code).unwrap_or_else(|callee| HMIRApplication {
                callee: Box::new(callee),
                args: vec![None; params.len()],
            });
        let mut slots = application.args.iter_mut().filter(|slot| slot.is_none());
        for (arg, param) in args.iter().zip(params) {
            let value = match is_code(context, param) {
                true => argument(context, body, *arg, param)?,
                false => comptime::freeze(context, body, *arg, Some(param))?,
            };
            *slots
                .next()
                .expect("the application has a slot for each parameter") = Some(value.ready()?);
        }
        match opaque {
            true => HMIRValue::new(ty, HMIRImmValue::Opaque),
            false => value::application(ty, application),
        }
    };
    match context.is_runtime() && full {
        true => materialize(context, value, span),
        false => Ok(Evaluation::Ready(value)),
    }
}

pub(crate) fn materialize(
    context: &mut LocalLoweringContext,
    code: HMIRValue,
    span: &TokenRange,
) -> EvalResult<Evaluation<HMIRValue>> {
    let HMIRTypeKind::Staged(result) = *context.env().resolve_kind(code.ty()) else {
        unreachable!("materialization requires code");
    };
    let (callee, values) = match value::into_application(code) {
        Ok(application) => {
            let args = application
                .args
                .into_iter()
                .collect::<Option<Vec<_>>>()
                .expect("a code application is fully bound");
            (*application.callee, args)
        }
        Err(code) => match code.kind() {
            HMIRValueKind::Immediate(HMIRImmValue::Quote(_)) => (code, Vec::new()),
            HMIRValueKind::Immediate(HMIRImmValue::Opaque) => return opaque(context, result),
            HMIRValueKind::Immediate(_) => return Ok(Evaluation::Ready(code.with_ty(result))),
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
            let expected = (!context.env().is_void(result)).then_some(result);
            quote::expand(context, node.get(), &values, expected)?
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
