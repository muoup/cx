use cx_log::{
    CXResult,
    error::{CXErr, context::CXInternalContext, message::CXStdErrMessage},
};
use cx_mir::MIRConstant;
use cx_mir_comptime::{MIRComptimeValue, evaluate_comptime_expr};
use cx_thir::thir::expression::THIRExpression;

use crate::builder::MIRBuilder;

fn constant_error(context: &str) -> CXErr {
    CXErr::new(
        CXStdErrMessage::error(
            "COMPTIME ERROR",
            format!("expression in {context} did not evaluate to a MIR constant"),
        ),
        CXInternalContext::error("comptime evaluation produced no constant"),
    )
}

pub(crate) fn evaluate_integer(
    builder: &mut MIRBuilder<'_>,
    expression: &THIRExpression,
    context: &str,
) -> CXResult<usize> {
    let value = evaluate_comptime_expr(builder, expression)?;
    match value {
        MIRComptimeValue::Integer { val, .. } => {
            usize::try_from(val).map_err(|_| constant_error(context))
        }
        _ => Err(constant_error(context)),
    }
}

pub(crate) fn evaluate(
    builder: &mut MIRBuilder<'_>,
    expression: &THIRExpression,
) -> CXResult<MIRConstant> {
    let value = evaluate_comptime_expr(builder, expression)?;
    match value {
        MIRComptimeValue::Integer { val, _ty, signed } => Ok(MIRConstant::Integer {
            value: val,
            ty: _ty,
            signed,
        }),
        MIRComptimeValue::Float { val, _ty } => Ok(MIRConstant::Float {
            value: val,
            ty: _ty,
        }),
        MIRComptimeValue::FunctionReference(id) => Ok(MIRConstant::Function(id)),
        MIRComptimeValue::Staged { .. } => Err(constant_error("staged expression")),
    }
}

