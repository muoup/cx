mod binary;
mod ty;
mod unary;

pub(crate) use binary::{binary_operands, eval_binary};
pub(crate) use ty::eval_type_op;
pub(crate) use unary::eval_unary;

use cx_hmir::{
    HMIRBody,
    expr::HMIRExprID,
    ty::{HMIRTypeID, HMIRTypeKind, context::HMIRTypeContext},
    value::HMIRValue,
};
use cx_log::CXResult;
use cx_tokens::TokenRange;

use crate::{
    emit,
    env::function::LocalLoweringContext,
    eval::{EvalResult, coerce::convert, eval, promote},
};

fn kind(context: &LocalLoweringContext, ty: HMIRTypeID) -> HMIRTypeKind {
    context.env().resolve_type_id(ty).kind().clone()
}

pub(crate) fn promote_operand(
    context: &mut LocalLoweringContext,
    value: HMIRValue,
    span: &TokenRange,
) -> CXResult<HMIRValue> {
    let value = emit::referent(context, value, span)?;
    if matches!(
        kind(context, value.ty()),
        HMIRTypeKind::Array { .. } | HMIRTypeKind::Str | HMIRTypeKind::Function(_)
    ) {
        todo!("array, string or function operand decay")
    }

    let promoted = promote::integer(context, value.ty(), span);
    convert(context, value, promoted, span)
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
