use cx_hmir::{
    HMIRBody,
    expr::HMIRExprID,
    ty::{HMIRTypeID, HMIRTypeKind, context::HMIRTypeContext},
};
use cx_log::catalogue::typecheck;
use cx_mir::MIRInstructionKind;
use cx_tokens::TokenRange;

use crate::{
    emit,
    env::function::{FnContext, LocalLoweringContext},
    eval::{
        EvalResult, coerce, constructor, eval,
        flow::{Evaluation, RuntimeStop, ready},
        function, types,
    },
    log::hmir_error,
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
) -> EvalResult<HMIRValue> {
    let callee = ready!(eval(context, body, callee));
    let callee = coerce::std_rvalue(context, callee, span)?;
    if (matches!(context.context(), FnContext::Comptime(_)) && constructs(&callee).is_none())
        || matches!(
            context.env().resolve_kind(callee.ty()),
            HMIRTypeKind::FunctionItem(_)
        )
    {
        return function::call(context, &callee, body, args, span);
    }

    let env = context.env();
    let function = env
        .pointer_inner(callee.ty())
        .map(|inner| env.resolve_kind(inner));
    let Some(HMIRTypeKind::Function(signature)) = function else {
        return Err(hmir_error(
            span,
            &typecheck::UNEXPECTED_KIND,
            (types::name(context.env(), callee.ty()), "callable".into()),
        )
        .into());
    };
    let signature = signature.clone();

    let params = signature.params();
    if args.len() < params.len() || (!signature.is_variadic() && args.len() > params.len()) {
        let subject = match callee.kind() {
            HMIRValueKind::Constant(HMIRConstant::Function { function, .. }) => match function {
                HMIRFunctionRef::Def(def) => {
                    format!("'{}'", env.symbols().resolve_definition(*def).name())
                }
                HMIRFunctionRef::Constructor { ty, variant } => {
                    format!("'{}'", constructor::name(env, *ty, *variant))
                }
            },
            _ => "function".into(),
        };
        let counts = (subject, params.len(), args.len(), signature.is_variadic());

        return Err(hmir_error(span, &typecheck::ARGUMENT_COUNT, counts).into());
    }

    let mut values = Vec::with_capacity(args.len());
    for (index, arg) in args.iter().enumerate() {
        let span = body.expr(*arg).span();
        let param = params.get(index).map(|param| *param.ty());
        let evaluated = eval(context, body, *arg)?;
        let value = ready!(evaluated.complete(context, body, param));
        let value = match param {
            Some(param) => coerce::implicit(context, value, param, span)?,
            None => coerce::default_argument(context, value, span)?,
        };

        values.push(value);
    }

    finish(context, &callee, &values, *signature.return_type(), span)
}

pub(crate) fn finish(
    context: &mut LocalLoweringContext,
    callee: &HMIRValue,
    values: &[HMIRValue],
    return_type: HMIRTypeID,
    span: &TokenRange,
) -> EvalResult<HMIRValue> {
    if matches!(context.context(), FnContext::Comptime(_))
        && let Some((ty, variant)) = constructs(callee)
    {
        let payload = values.first().cloned();
        let value = constructor::construct(context, ty, variant, payload, span)?;
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
        return Ok(Evaluation::Stop(RuntimeStop::Diverged.into()));
    }

    Ok(Evaluation::Ready(result))
}

fn constructs(callee: &HMIRValue) -> Option<(HMIRTypeID, usize)> {
    match callee.kind() {
        HMIRValueKind::Constant(HMIRConstant::Function {
            function: HMIRFunctionRef::Constructor { ty, variant },
            ..
        }) => Some((*ty, *variant)),
        _ => None,
    }
}
