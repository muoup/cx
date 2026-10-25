use cx_hmir::{
    HMIRBody,
    expr::HMIRExprID,
    ty::{HMIRTypeKind, context::HMIRTypeContext},
};
use cx_log::catalogue::typecheck;
use cx_tokens::TokenRange;

use crate::{
    env::function::{FnContext, LocalLoweringContext, runtime::Emission},
    eval::{
        EvalResult, coerce, defer, eval,
        flow::{ComptimeStop, Evaluation, RuntimeStop},
        literal, storage,
    },
    log::hmir_error,
    value::HMIRValue,
};

pub(crate) fn eval_yield(
    context: &mut LocalLoweringContext,
    body: &HMIRBody,
    span: &TokenRange,
    value: Option<HMIRExprID>,
) -> EvalResult<HMIRValue> {
    let available = match context.context() {
        FnContext::Runtime(runtime) => runtime.yielding(),
        FnContext::Comptime(comptime) => comptime.yielding(),
    };
    if !available && context.expectations().deferred() {
        return Err(hmir_error(span, &typecheck::DEFER_JUMP, "yield".into()));
    }
    if !available {
        return Err(hmir_error(
            span,
            &typecheck::REQUIRED_CONTEXT,
            ("yield".into(), "a yielding expression".into()),
        ));
    }
    let value = match value {
        Some(expr) => eval(context, body, expr)?,
        None => Evaluation::Ready(literal::unit(context, span)),
    };
    let span = span.clone();
    value.and_then(context, body, None, move |context, body, value, _| {
        let value = coerce::std_rvalue(context, value, &span)?;
        match context.context() {
            FnContext::Comptime(_) => Ok(Evaluation::Stop(ComptimeStop::Yield(value).into())),
            FnContext::Runtime(_) => {
                let value = if matches!(context.env().resolve_kind(value.ty()), HMIRTypeKind::Void)
                {
                    value
                } else {
                    storage::temporary(context, value, &span)?
                };
                let depth = context.runtime().yield_depth();
                defer::exit(context, body, depth, &span)?;
                context.runtime_mut().yield_value(value);
                context.runtime_mut().set_emission(Emission::Suppressed);
                Ok(Evaluation::Stop(RuntimeStop::Diverged.into()))
            }
        }
    })
}
