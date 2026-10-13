use cx_hmir::{
    HMIRBody,
    expr::{HMIRExprID, constant::HMIRConstant},
    ty::{HMIRTypeKind, context::HMIRTypeContext},
    value::{HMIRValue, HMIRValueKind},
};
use cx_log::catalogue::typecheck;
use cx_mir::MIRInstructionKind;
use cx_tokens::TokenRange;

use crate::{
    emit,
    env::function::LocalLoweringContext,
    eval::{
        EvalResult,
        coerce::{convert, decay},
        eval,
        flow::RuntimeStop,
        promote,
    },
    log::hmir_error,
};

pub(crate) fn eval_call(
    context: &mut LocalLoweringContext,
    body: &HMIRBody,
    span: &TokenRange,
    callee: HMIRExprID,
    args: &[HMIRExprID],
) -> EvalResult<HMIRValue> {
    let callee = eval(context, body, callee)?;
    let callee = emit::referent(context, callee, span)?;
    let callee = decay(context, callee, span)?;
    let callee = emit::read(context, callee, span)?;

    let env = context.env();
    let function = env
        .pointer_inner(&callee.ty())
        .map(|inner| env.resolve_kind(inner));
    let Some(HMIRTypeKind::Function(signature)) = function else {
        return Err(hmir_error(
            span,
            &typecheck::UNEXPECTED_KIND,
            (todo!("type display"), "callable".into()),
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
        let value = eval(context, body, *arg)?;
        let value = match params.get(index) {
            Some(param) => convert(context, value, *param.ty(), span)?,
            None => {
                let value = emit::referent(context, value, span)?;
                let value = decay(context, value, span)?;
                let promoted = promote::argument(context, value.ty(), span);

                convert(context, value, promoted, span)?
            }
        };

        values.push(emit::read(context, value, span)?);
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
