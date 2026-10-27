pub(crate) mod contract;
mod dependent;
mod specialize;

pub(crate) use dependent::{Frame, body, dependent};

use cx_hmir::{
    HMIRBody, HMIRDefKind, HMIRFunctionStage,
    expr::HMIRExprID,
    ty::{HMIRTypeID, HMIRTypeKind, context::HMIRTypeContext},
};
use cx_log::{CXResult, catalogue::typecheck};
use cx_mir::MIRInstructionKind;
use cx_tokens::TokenRange;

use crate::{
    emit,
    env::function::LocalLoweringContext,
    eval::{
        EvalResult, aggregate::variant, coerce, comptime, eval, expr,
        flow::{self, Evaluation},
        staged, types,
    },
    log::hmir_error,
    symbol,
    value::{
        HMIRValue, HMIRValueKind,
        constant::{HMIRConstant, HMIRFunctionRef},
    },
};

pub(crate) fn eval_call(
    context: &mut LocalLoweringContext,
    body: &HMIRBody,
    span: &TokenRange,
    callee: HMIRExprID,
    args: &[HMIRExprID],
) -> EvalResult<Evaluation<HMIRValue>> {
    let evaluated = eval(context, body, callee)?;
    let (args, span) = (args.to_vec(), span.clone());
    evaluated.and_then(context, body, None, move |context, body, callee, expected| {
        evaluate(context, body, &span, callee, &args)?.try_complete(context, body, expected)
    })
}

fn evaluate(
    context: &mut LocalLoweringContext,
    body: &HMIRBody,
    span: &TokenRange,
    callee: HMIRValue,
    args: &[HMIRExprID],
) -> EvalResult<Evaluation<HMIRValue>> {
    let callee = staged::read(context, callee, span)?;
    if matches!(callee.kind(), HMIRValueKind::Constant(HMIRConstant::Quote(_))) {
        return staged::apply(context, body, callee, args, span);
    }
    let callee = coerce::std_rvalue(context, callee, span)?;
    let (callee, applied) = match callee.kind() {
        HMIRValueKind::Constant(HMIRConstant::Curried { callee, args }) => {
            (callee.as_ref().clone(), args.clone())
        }
        _ => (callee, Vec::new()),
    };
    if context.is_unevaluated()
        && let HMIRValueKind::Constant(HMIRConstant::Function(HMIRFunctionRef::Def(def))) = callee.kind()
    {
        let source = context.env().symbols().resolve_definition(*def);
        let selected = symbol::select(source, None)?;
        if let Some(HMIRDefKind::Function(function)) = source.declarations()[selected].kind()
            && function.signature().stage() == HMIRFunctionStage::Comptime
        {
            let mut child = comptime::child(context);
            return dependent(&mut child, &callee, body, args, &applied, span);
        }
    }
    if (!context.is_runtime() && constructs(&callee).is_none())
        || types::comptime_only(context.env(), callee.ty())
    {
        return dependent(context, &callee, body, args, &applied, span);
    }

    let env = context.env();
    let function = env.pointer_inner(callee.ty()).map(|inner| env.resolve_kind(inner));
    let Some(HMIRTypeKind::Function(signature)) = function else {
        let kind = (types::name(env, callee.ty()), "callable".into());
        return Err(hmir_error(span, &typecheck::UNEXPECTED_KIND, kind).into());
    };
    let signature = signature.clone();
    let params = signature.params();
    let count = applied.len() + args.len();
    if count < params.len() || (!signature.is_variadic() && count > params.len()) {
        let subject = match callee.kind() {
            HMIRValueKind::Constant(HMIRConstant::Function(HMIRFunctionRef::Def(def))) => {
                format!("'{}'", env.symbols().resolve_definition(*def).name())
            }
            HMIRValueKind::Constant(HMIRConstant::Function(HMIRFunctionRef::Constructor { ty, variant })) => {
                format!("'{}'", variant::name(env, *ty, *variant))
            }
            _ => "function".into(),
        };
        let counts = (subject, params.len(), count, signature.is_variadic());
        return Err(hmir_error(span, &typecheck::ARGUMENT_COUNT, counts).into());
    }
    let source_defined = constructs(&callee).is_none();
    require(context, span, !source_defined || signature.contract().is_safe(), source_defined)?;

    let mut values = applied;
    for arg in args {
        let span = body.expr(*arg).span();
        let param = params.get(values.len()).map(|param| *param.ty());
        let value = eval(context, body, *arg)?.complete(context, body, param)?;
        values.push(match param {
            Some(param) => coerce::implicit(context, value, param, span)?,
            None => coerce::default_argument(context, value, span)?,
        });
    }

    let return_type = *signature.return_type();
    match callee.kind() {
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
        expr::require_pure(context, span, "call to a user-defined function")?;
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
        return Ok(Evaluation::Ready(emit::scratch(context, return_type, span)?));
    }
    if !context.is_runtime()
        && let Some((ty, variant)) = constructs(callee)
    {
        let value = variant::construct(context, ty, variant, values.first().cloned(), span)?;
        return Ok(Evaluation::Ready(value));
    }
    let result = emit::call(context, callee, values, return_type, span)?;
    if matches!(context.env().resolve_kind(return_type), HMIRTypeKind::Unreachable) {
        context.runtime_mut().emit(MIRInstructionKind::Unreachable, span.clone());
        return flow::diverged();
    }
    Ok(Evaluation::Ready(result))
}

fn constructs(callee: &HMIRValue) -> Option<(HMIRTypeID, usize)> {
    match callee.kind() {
        HMIRValueKind::Constant(HMIRConstant::Function(HMIRFunctionRef::Constructor { ty, variant })) => {
            Some((*ty, *variant))
        }
        _ => None,
    }
}
