use chl_hmir::{
    HMIRBodyContext,
    expr::HMIRExprID,
    ty::{HMIRTypeKind, context::HMIRTypeContext},
};
use chl_log::catalogue::typecheck;
use chl_tokens::TokenRange;

use crate::{
    env::function::LocalLoweringContext,
    eval::{EvalResult, comptime, eval, flow::Evaluation, types},
    log::hmir_error,
    value::{self, HMIRValue},
};

pub(crate) fn concat(
    context: &mut LocalLoweringContext,
    body: &HMIRBodyContext,
    span: &TokenRange,
    lhs: HMIRExprID,
    rhs: HMIRExprID,
) -> EvalResult<Evaluation<HMIRValue>> {
    let evaluate = |context: &mut LocalLoweringContext| {
        let mut text = operand(context, body, lhs)?;
        let rhs = operand(context, body, rhs)?;
        text.push_str(&rhs);
        let ty = types::realize(context.env_mut(), HMIRTypeKind::Str(None), span);
        let ty = types::reference(context.env_mut(), ty, None, span);
        Ok(Evaluation::Ready(value::string(ty, text)))
    };
    if context.is_runtime() {
        comptime::frozen(context, span, evaluate)
    } else {
        evaluate(context)
    }
}

fn operand(
    context: &mut LocalLoweringContext,
    body: &HMIRBodyContext,
    expr: HMIRExprID,
) -> EvalResult<String> {
    let value = eval(context, body, expr)?.complete(context, body, None)?;
    let env = context.env();
    let ty = value.ty();
    if env.is_str(env.referent(ty))
        && let Some(text) = value::as_string(&value)
    {
        return Ok(text.to_owned());
    }
    let span = body.expr(expr).span();
    let subject = format!("'{}' operand of '@str.concat'", types::name(env, ty));
    let expected = "a comptime string backed by a whole literal".into();
    Err(hmir_error(span, &typecheck::UNEXPECTED_KIND, (subject, expected)).into())
}
