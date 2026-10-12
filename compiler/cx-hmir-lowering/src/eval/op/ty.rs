use cx_hmir::{
    HMIRBody,
    expr::{HMIRExprID, constant::HMIRConstant, op::HMIRTypeOp},
    ty::{HMIRTypeID, HMIRTypeKind},
};
use cx_tokens::TokenRange;

use crate::{
    env::function::LocalLoweringContext,
    eval::{EvalResult, eval_comptime, eval_type},
};

pub(crate) fn eval_type_op(
    context: &mut LocalLoweringContext,
    body: &HMIRBody,
    span: &TokenRange,
    op: &HMIRTypeOp,
) -> EvalResult<HMIRTypeID> {
    let kind = match op {
        HMIRTypeOp::Pointer(inner) => HMIRTypeKind::PointerTo(eval_type(context, body, *inner)?),
        HMIRTypeOp::Reference(inner) => {
            HMIRTypeKind::ReferenceTo(eval_type(context, body, *inner)?)
        }
        HMIRTypeOp::Array { element, length } => {
            let element = eval_type(context, body, *element)?;
            let length = length
                .map(|length| array_length(context, body, length))
                .transpose()?;

            HMIRTypeKind::Array { element, length }
        }

        HMIRTypeOp::Const(_) => todo!("HMIRType has no const qualifier to realize 'type.const'"),
        _ => todo!("HMIRTypeOp: {}", op.path()),
    };

    Ok(context.env_mut().resolver_mut().realize_type(kind, span))
}

fn array_length(
    context: &mut LocalLoweringContext,
    body: &HMIRBody,
    length: HMIRExprID,
) -> EvalResult<u64> {
    match eval_comptime(context, body, length)? {
        HMIRConstant::Int { value, .. } if value >= 0 => Ok(value as u64),
        _ => todo!("Error: array length is not a non-negative integer constant"),
    }
}
