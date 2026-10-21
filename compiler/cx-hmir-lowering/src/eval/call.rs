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
    env::function::LocalLoweringContext,
    eval::{EvalResult, coerce, complete::elaborate, eval, flow::RuntimeStop, types},
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
    let callee = eval(context, body, callee)?;
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
        let value = elaborate(context, body, *arg)?.complete(context, body, param)?;
        let value = match param {
            Some(param) => coerce::implicit(context, value, param, span)?,
            None => coerce::default_argument(context, value, span)?,
        };

        values.push(value);
    }

    let return_type = *signature.return_type();
    let result = emit::call(context, &callee, &values, return_type, span)?;
    if matches!(
        context.env().resolve_kind(return_type),
        HMIRTypeKind::Unreachable
    ) {
        context
            .runtime_mut()
            .emit(MIRInstructionKind::Unreachable, span.clone());
        return Err(RuntimeStop::Diverged.into());
    }

    Ok(result)
}
