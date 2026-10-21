mod binary;
mod ty;
mod unary;

pub(crate) use binary::{binary_operands, eval_binary};
pub(crate) use ty::{eval_declared_type, eval_type_op, specify};
pub(crate) use unary::eval_unary;

use cx_hmir::{HMIRBody, expr::HMIRExprID};
use cx_tokens::TokenRange;

use crate::{
    env::function::LocalLoweringContext,
    eval::{
        EvalResult, coerce, eval,
        flow::{Evaluation, ready},
    },
    value::HMIRValue,
};

pub(crate) fn eval_operand(
    context: &mut LocalLoweringContext,
    body: &HMIRBody,
    span: &TokenRange,
    expr: HMIRExprID,
) -> EvalResult<HMIRValue> {
    let value = ready!(eval(context, body, expr));

    Ok(Evaluation::Ready(coerce::arithmetic(context, value, span)?))
}
