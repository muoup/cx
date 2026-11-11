pub(crate) mod argument;
pub(crate) mod bind;
pub(crate) mod contract;
pub(crate) mod dependent;
mod specialize;

use cx_hmir::{
    HMIRBodyContext, HMIRFnSignature, HMIRFunctionStage,
    expr::HMIRExprID,
    ty::{HMIRTypeID, HMIRTypeKind, context::HMIRTypeContext},
};
use cx_log::{
    CXResult,
    catalogue::{mir, typecheck},
};
use cx_mir::MIRInstructionKind;
use cx_tokens::TokenRange;
use cx_util::identifier::CXIdent;

use crate::{
    emit,
    env::function::LocalLoweringContext,
    eval::{
        EvalResult,
        aggregate::variant,
        coerce, comptime, eval, expr,
        flow::{self, Evaluation},
        helpers, staged, types,
    },
    log::hmir_error,
    value::{
        self, HMIRValue, HMIRValueKind,
        immediate::{Addressable, HMIRImmediate},
    },
};

pub(crate) fn eval_call(
    context: &mut LocalLoweringContext,
    body: &HMIRBodyContext,
    span: &TokenRange,
    callee: HMIRExprID,
    args: &[HMIRExprID],
    named: &[(CXIdent, HMIRExprID)],
) -> EvalResult<Evaluation<HMIRValue>> {
    let (span, args, named) = (span.clone(), args.to_vec(), named.to_vec());
    eval(context, body, callee)?.chain(
        context,
        body,
        None,
        None,
        move |context, body, callee, expected| {
            let callee = coerce::collapse(context, callee, &span)?;
            apply(context, body, &span, callee, &args, &named)?
                .try_complete(context, body, expected)
        },
    )
}

// Decides what each argument of a call is evaluated as from the parameter that it binds, and calls the callee
// with the values, or curries it with them where they leave parameters open
fn apply(
    context: &mut LocalLoweringContext,
    body: &HMIRBodyContext,
    span: &TokenRange,
    callee: HMIRValue,
    args: &[HMIRExprID],
    named: &[(CXIdent, HMIRExprID)],
) -> EvalResult<Evaluation<HMIRValue>> {
    if matches!(
        callee.kind(),
        HMIRValueKind::Immediate(HMIRImmediate::Quote(_))
    ) {
        if let Some((_, argument)) = named.first() {
            let named = ("a named argument".into(), "a staged expression call".into());
            let span = body.expr(*argument).span();
            return Err(hmir_error(span, &typecheck::INVALID_CONTEXT, named).into());
        }
        return staged::apply(context, body, callee, args, span);
    }

    let (callee, applied) = value::uncurried(callee);
    let (callee, signature) = coerce_callee(context, callee, span)?;
    if context.is_unevaluated()
        && signature.stage() == HMIRFunctionStage::Comptime
        && value::as_function(&callee).is_some()
    {
        let mut child = comptime::child(context);
        let callee = value::curried(callee.ty(), callee, applied);
        return apply(&mut child, body, span, callee, args, named);
    }

    let parameters = bind::parameters(context, &callee, &signature);
    let binding = bind::bind(
        &parameters,
        signature.is_variadic(),
        &name(context, &callee),
        body,
        applied,
        args,
        named,
        span,
    )?;
    let partial = binding.is_partial();
    let span = span.clone();
    argument::evaluate(context, body, &callee, &signature, binding, &span)?.and_then(
        context,
        body,
        move |context, _, values| match partial {
            true => curry(context, callee, &signature, values, &span),
            false => hmir_call(
                context,
                &span,
                callee,
                values.into_iter().flatten().collect(),
            ),
        },
    )
}

// Calls 'callee', which is not curried, with a value for each of its parameters
pub(crate) fn hmir_call(
    context: &mut LocalLoweringContext,
    span: &TokenRange,
    callee: HMIRValue,
    args: Vec<HMIRValue>,
) -> EvalResult<Evaluation<HMIRValue>> {
    let (callee, signature) = coerce_callee(context, callee, span)?;
    let params = signature.params();
    if args.len() < params.len() || (args.len() > params.len() && !signature.is_variadic()) {
        let counts = (
            format!("'{}'", name(context, &callee)),
            params.len(),
            args.len(),
            signature.is_variadic(),
        );
        return Err(hmir_error(span, &typecheck::ARGUMENT_COUNT, counts).into());
    }

    let constructor = match *callee.kind() {
        HMIRValueKind::Immediate(HMIRImmediate::Constructor { ty, variant }) => Some((ty, variant)),
        _ => None,
    };
    if constructor.is_none() {
        context_require(context, span, signature.contract().is_safe(), false)?;

        let referent = context.env().referent(callee.ty());
        if dependent::applies(context, referent) {
            let Some(def) = value::as_function(&callee) else {
                let unavailable = "this function".into();
                return Err(hmir_error(span, &mir::COMPTIME_UNAVAILABLE, unavailable).into());
            };
            return dependent::apply(context, def, args.into_iter().map(Some).collect(), span);
        }
    }

    let mut values = Vec::with_capacity(args.len());
    for (index, value) in args.into_iter().enumerate() {
        values.push(match params.get(index) {
            Some(param) => coerce::implicit::standard(context, value, *param.ty(), span)?,
            None => coerce::default_argument(context, value, span)?,
        });
    }
    let return_type = *signature.return_type();
    match (constructor, value::as_function(&callee)) {
        (Some((ty, variant)), _) if !context.is_runtime() => {
            let payload = values.into_iter().next();
            let value = variant::construct(context, ty, variant, payload, span)?;
            Ok(Evaluation::Ready(value))
        }
        (None, Some(def)) if !context.is_unevaluated() => {
            contract::call(context, def, &values, span, |context| {
                finish(context, &callee, &values, return_type, span)
            })
        }
        _ => finish(context, &callee, &values, return_type, span),
    }
}

// A partial call binds 'values' to the parameters of 'callee'. Once they bind every comptime parameter of a
// function, the result is the function generated for them
fn curry(
    context: &mut LocalLoweringContext,
    callee: HMIRValue,
    signature: &HMIRFnSignature<HMIRTypeID>,
    values: Vec<Option<HMIRValue>>,
    span: &TokenRange,
) -> EvalResult<Evaluation<HMIRValue>> {
    let open = signature.params().iter().zip(&values);
    let curries = signature.stage() == HMIRFunctionStage::Comptime
        || open
            .clone()
            .any(|(param, value)| value.is_none() && param.comptime());
    if !curries && let Some(def) = value::as_function(&callee) {
        return dependent::apply(context, def, values, span);
    }
    let residual = types::residual(signature, |index| values[index].is_some());
    let ty = types::realize(context.env_mut(), HMIRTypeKind::Function(residual), span);
    Ok(Evaluation::Ready(value::curried(ty, callee, values)))
}

// To simplify semantics, all function callees are unified into a @fn& types. Whether a direct or indirect
// call is generated at runtime depends on the underlying MIRValue produced, not on the type.
fn coerce_callee(
    context: &mut LocalLoweringContext,
    callee: HMIRValue,
    span: &TokenRange,
) -> CXResult<(HMIRValue, HMIRFnSignature<HMIRTypeID>)> {
    let ty = callee.ty();
    let (subject, ty) = match *context.env().resolve_kind(ty) {
        HMIRTypeKind::ReferenceTo { inner, .. } => (inner, ty),
        HMIRTypeKind::PointerTo(base) => {
            helpers::null_check(context, callee.clone(), span)?;
            (base, types::reference(context.env_mut(), base, None, span))
        }
        _ => (ty, types::reference(context.env_mut(), ty, None, span)),
    };
    let HMIRTypeKind::Function(signature) = context.env().resolve_kind(subject) else {
        let kind = (types::name(context.env(), callee.ty()), "callable".into());
        return Err(hmir_error(span, &typecheck::UNEXPECTED_KIND, kind));
    };
    Ok((callee.with_ty(ty), signature.clone()))
}

fn name(context: &LocalLoweringContext, callee: &HMIRValue) -> String {
    match callee.kind() {
        HMIRValueKind::Immediate(HMIRImmediate::Address(address))
            if let Addressable::Def(def) = address.base() =>
        {
            context
                .env()
                .symbols()
                .resolve_definition(*def)
                .name()
                .to_string()
        }

        HMIRValueKind::Immediate(HMIRImmediate::Constructor { ty, variant }) => {
            variant::name(context.env(), *ty, *variant)
        }

        _ => "function".into(),
    }
}

fn context_require(
    context: &LocalLoweringContext,
    span: &TokenRange,
    safe: bool,
    pure: bool,
) -> CXResult<()> {
    if !safe {
        expr::require_nonsafe(context, span, "call to non-safe function")?;
    }

    if !pure {
        expr::require_impure(context, span, "call to a user-defined function")?;
    }

    Ok(())
}

fn finish(
    context: &mut LocalLoweringContext,
    callee: &HMIRValue,
    args: &[HMIRValue],
    return_type: HMIRTypeID,
    span: &TokenRange,
) -> EvalResult<Evaluation<HMIRValue>> {
    if context.is_unevaluated() {
        todo!("call::finish -- context.is_unevaluated()");
    }
    
    let result = emit::call(context, callee, args, return_type, span)?;

    match context.env().resolve_kind(return_type) {
        HMIRTypeKind::Unreachable => {
            context
                .runtime_mut()
                .emit(MIRInstructionKind::Unreachable, span.clone());

            flow::diverged()
        },

        _ => Ok(Evaluation::Ready(result))
    }
}
