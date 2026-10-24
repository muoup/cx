use cx_hmir::{
    HMIRBody,
    expr::HMIRExprID,
    ty::{HMIRTypeKind, context::HMIRTypeContext},
};
use cx_log::catalogue::typecheck;
use cx_mir::MIRInstructionKind;
use cx_tokens::TokenRange;

use crate::{
    emit,
    env::function::{FnContext, LocalLoweringContext},
    eval::{
        EvalResult,
        aggregate::initializer,
        coerce, eval,
        flow::{Evaluation, RuntimeStop, ready},
        function, types,
    },
    log::hmir_error,
    value::{HMIRValue, HMIRValueKind, constant::HMIRConstant},
};

pub(crate) fn eval_call(
    context: &mut LocalLoweringContext,
    body: &HMIRBody,
    span: &TokenRange,
    callee: HMIRExprID,
    args: &[HMIRExprID],
) -> EvalResult<HMIRValue> {
    let callee = ready!(eval(context, body, callee));
    if let HMIRValueKind::Constant(HMIRConstant::Constructor { ty, variant }) = callee.kind() {
        if args.len() != 1 {
            return Err(hmir_error(
                span,
                &typecheck::ARGUMENT_COUNT,
                ("variant constructor".into(), 1, args.len(), false),
            ));
        }
        return initializer::initialize(
            context,
            body,
            span,
            *ty,
            &[(Some(variant.clone()), args[0])],
        );
    }
    let callee = coerce::std_rvalue(context, callee, span)?;

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
            HMIRValueKind::Constant(HMIRConstant::Function { def, .. }) => {
                format!("'{}'", env.symbols().resolve_definition(*def).name())
            }
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

    let return_type = *signature.return_type();
    if matches!(context.context(), FnContext::Comptime(_)) {
        let value = function::call(context, &callee, &signature, &values, span)?;
        return Ok(Evaluation::Ready(value));
    }
    let result = emit::call(context, &callee, &values, return_type, span)?;
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
