pub(crate) mod contract;
mod dependent;
mod specialize;

pub(crate) use dependent::{Frame, body, dependent, parameter_name};

use cx_hmir::{
    HMIRBodyContext, HMIRDefKind, HMIRFunctionStage,
    expr::HMIRExprID,
    ty::{HMIRTypeID, HMIRTypeKind, context::HMIRTypeContext},
};
use cx_log::{CXResult, catalogue::typecheck};
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
        staged, types,
    },
    log::hmir_error,
    lower, symbol,
    value::{
        HMIRValue, HMIRValueKind,
        address::{Addressable, HMIRAddress},
        constant::{HMIRConstant, HMIRFunctionRef},
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
    let (args, named, span) = (args.to_vec(), named.to_vec(), span.clone());

    eval(context, body, callee)?.and_then(
        context,
        body,
        None,
        move |context, body, callee, expected| {
            evaluate(context, body, &span, callee, &args, &named)?
                .try_complete(context, body, expected)
        },
    )
}

fn evaluate(
    context: &mut LocalLoweringContext,
    body: &HMIRBodyContext,
    span: &TokenRange,
    callee: HMIRValue,
    args: &[HMIRExprID],
    named: &[(CXIdent, HMIRExprID)],
) -> EvalResult<Evaluation<HMIRValue>> {
    let callee = staged::read(context, callee, span)?;
    if matches!(
        callee.kind(),
        HMIRValueKind::Constant(HMIRConstant::Quote(_))
    ) {
        if let Some((_, value)) = named.first() {
            let context = ("a named argument".into(), "a staged expression call".into());
            return Err(hmir_error(
                body.expr(*value).span(),
                &typecheck::INVALID_CONTEXT,
                context,
            )
            .into());
        }
        return staged::apply(context, body, callee, args, span);
    }
    let callee = operand(context, callee, span)?;
    let (callee, applied) = match callee.as_function().map(HMIRValue::kind) {
        Some(HMIRValueKind::Constant(HMIRConstant::Curried { callee, args })) => {
            let (inner, args) = (callee.as_ref().clone(), args.clone());
            (operand(context, inner, span)?, args)
        }
        _ => (callee, Vec::new()),
    };
    let target = callee.as_function().unwrap_or(&callee).clone();
    if context.is_unevaluated()
        && let HMIRValueKind::Constant(HMIRConstant::Function(HMIRFunctionRef::Def(def))) =
            target.kind()
    {
        let source = context.env().symbols().resolve_definition(*def);
        let selected = symbol::select(source, None)?;
        if let Some(HMIRDefKind::Function(function)) = source.declarations()[selected].kind()
            && function.signature().stage() == HMIRFunctionStage::Comptime
        {
            let mut child = comptime::child(context);
            return dependent(&mut child, &target, body, args, named, &applied, span);
        }
    }
    let referent = context.env().referent(callee.ty());
    if (!context.is_runtime() && constructs(&target).is_none())
        || types::comptime_only(context.env(), referent)
    {
        return dependent(context, &target, body, args, named, &applied, span);
    }

    let env = context.env();
    let subject = || match target.kind() {
        HMIRValueKind::Constant(HMIRConstant::Function(HMIRFunctionRef::Def(def))) => {
            format!("'{}'", env.symbols().resolve_definition(*def).name())
        }
        HMIRValueKind::Constant(HMIRConstant::Function(HMIRFunctionRef::Constructor {
            ty,
            variant,
        })) => {
            format!("'{}'", variant::name(env, *ty, *variant))
        }
        _ => "function".into(),
    };
    if let Some((name, value)) = named.first() {
        let unknown = (subject(), name.to_string());
        return Err(hmir_error(
            body.expr(*value).span(),
            &typecheck::UNKNOWN_PARAMETER,
            unknown,
        )
        .into());
    }
    let HMIRTypeKind::Function(signature) = env.resolve_kind(referent) else {
        let kind = (types::name(env, callee.ty()), "callable".into());
        return Err(hmir_error(span, &typecheck::UNEXPECTED_KIND, kind).into());
    };
    let signature = signature.clone();
    let params = signature.params();
    let applied = applied.into_iter().flatten().collect::<Vec<_>>();
    let count = applied.len() + args.len();
    if count < params.len() || (!signature.is_variadic() && count > params.len()) {
        let counts = (subject(), params.len(), count, signature.is_variadic());
        return Err(hmir_error(span, &typecheck::ARGUMENT_COUNT, counts).into());
    }
    let source_defined = constructs(&target).is_none();
    require(
        context,
        span,
        !source_defined || signature.contract().is_safe(),
        source_defined,
    )?;

    let mut values = applied;
    for arg in args {
        let span = body.expr(*arg).span();
        let param = params.get(values.len()).map(|param| *param.ty());
        let value = eval(context, body, *arg)?.complete(context, body, param)?;
        values.push(match param {
            Some(param) => coerce::implicit::standard(context, value, param, span)?,
            None => coerce::default_argument(context, value, span)?,
        });
    }

    let return_type = *signature.return_type();
    match target.kind() {
        HMIRValueKind::Constant(HMIRConstant::Function(HMIRFunctionRef::Def(def)))
            if context.is_runtime() && !context.is_unevaluated() =>
        {
            contract::call(context, *def, &values, span, |context| {
                finish(context, &callee, &values, return_type, span)
            })
        }
        _ => finish(context, &callee, &values, return_type, span),
    }
}

fn require(
    context: &LocalLoweringContext,
    span: &TokenRange,
    safe: bool,
    source_defined: bool,
) -> CXResult<()> {
    if !safe {
        expr::require_nonsafe(context, span, "call to non-safe function")?;
    }
    if source_defined {
        expr::require_impure(context, span, "call to a user-defined function")?;
    }
    Ok(())
}

fn finish(
    context: &mut LocalLoweringContext,
    callee: &HMIRValue,
    values: &[HMIRValue],
    return_type: HMIRTypeID,
    span: &TokenRange,
) -> EvalResult<Evaluation<HMIRValue>> {
    if context.is_unevaluated() {
        return Ok(Evaluation::Ready(lower::expr::placeholder(
            context,
            return_type,
            span,
        )?));
    }
    if !context.is_runtime()
        && let Some((ty, variant)) = callee.as_function().and_then(constructs)
    {
        let value = variant::construct(context, ty, variant, values.first().cloned(), span)?;
        return Ok(Evaluation::Ready(value));
    }
    let result = emit::call(context, callee, values, return_type, span)?;
    if matches!(
        context.env().resolve_kind(return_type),
        HMIRTypeKind::Unreachable
    ) {
        context
            .runtime_mut()
            .emit(MIRInstructionKind::Unreachable, span.clone());
        return flow::diverged();
    }
    Ok(Evaluation::Ready(result))
}

fn operand(
    context: &mut LocalLoweringContext,
    callee: HMIRValue,
    span: &TokenRange,
) -> CXResult<HMIRValue> {
    let env = context.env();
    let function = |ty| matches!(env.resolve_kind(ty), HMIRTypeKind::Function(_));
    if env.reference_inner(callee.ty()).is_some_and(function) {
        return Ok(callee);
    }
    if function(callee.ty()) {
        let ty = types::reference(context.env_mut(), callee.ty(), None, span);
        let callee = context.env_mut().canonical_value(callee);
        let address = HMIRAddress::new(Addressable::Function(Box::new(callee)), 0);
        return Ok(HMIRValue::constant(ty, HMIRConstant::Address(address)));
    }
    let pointer = coerce::promotion::std_rvalue(context, callee, span)?;
    match context.env().pointer_inner(pointer.ty()) {
        Some(inner) if matches!(context.env().resolve_kind(inner), HMIRTypeKind::Function(_)) => {
            let storage = emit::dereference(context, pointer, inner, None, span)?;
            emit::reference(context, &storage, span)
        }
        _ => Ok(pointer),
    }
}

fn constructs(callee: &HMIRValue) -> Option<(HMIRTypeID, usize)> {
    match callee.kind() {
        HMIRValueKind::Constant(HMIRConstant::Function(HMIRFunctionRef::Constructor {
            ty,
            variant,
        })) => Some((*ty, *variant)),
        _ => None,
    }
}
