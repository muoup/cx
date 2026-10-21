mod binary;
mod ty;
mod unary;

pub(crate) use binary::{binary_operands, eval_binary};
pub(crate) use ty::{eval_declared_type, eval_type_op, specify};
pub(crate) use unary::eval_unary;

use cx_hmir::{
    HMIRBody,
    expr::HMIRExprID,
    ty::{HMIRTypeID, HMIRTypeKind, context::HMIRTypeContext},
};
use cx_log::CXResult;
use cx_tokens::TokenRange;

use crate::{
    emit,
    env::function::LocalLoweringContext,
    eval::{
        EvalResult,
        coerce::{cast, decay},
        eval, promote,
    },
    value::HMIRValue,
};

fn kind(context: &LocalLoweringContext, ty: HMIRTypeID) -> HMIRTypeKind {
    context.env().resolve_kind(ty).clone()
}

pub(crate) fn promote_operand(
    context: &mut LocalLoweringContext,
    value: HMIRValue,
    span: &TokenRange,
) -> CXResult<HMIRValue> {
    let value = emit::referent(context, value, span)?;
    let value = decay(context, value, span)?;

    let promoted = promote::integer(context, value.ty(), span);
    cast(context, value, promoted, span)
}

pub(crate) fn eval_operand(
    context: &mut LocalLoweringContext,
    body: &HMIRBody,
    span: &TokenRange,
    expr: HMIRExprID,
) -> EvalResult<HMIRValue> {
    let value = eval(context, body, expr)?;

    Ok(promote_operand(context, value, span)?)
}
