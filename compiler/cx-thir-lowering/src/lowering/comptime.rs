use cx_log::{
    CXResult,
    error::{CXErr, context::CXInternalContext, message::CXStdErrMessage},
};
use cx_mir::MIRConstant;
use cx_mir_comptime::{MIRComptimeValue, evaluate_compite_expr};
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
    let value = evaluate_compite_expr(builder, expression)?;
    match value {
        MIRComptimeValue::Constant(MIRConstant::Integer { value, .. }) => {
            usize::try_from(value).map_err(|_| constant_error(context))
        }
        _ => Err(constant_error(context)),
    }
}

pub(crate) fn evaluate(
    builder: &mut MIRBuilder<'_>,
    expression: &THIRExpression,
) -> CXResult<MIRConstant> {
    let value = evaluate_compite_expr(builder, expression)?;
    match value {
        MIRComptimeValue::Constant(value) => Ok(value),
        MIRComptimeValue::Staged(_) => Err(constant_error("staged expression")),
    }
}
