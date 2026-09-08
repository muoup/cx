use cx_log::{CXResult, catalogue::mir};
use cx_mir::MIRConstant;
use cx_mir_comptime::{MIRComptimeValue, evaluate_comptime_function};
use cx_thir::thir::expression::THIRExpression;

use crate::{builder::MIRBuilder, log::mir_error, lowering::capture::capture_expression};

pub(crate) fn evaluate_comptime_expr(
    context: &mut MIRBuilder,
    expr: &THIRExpression,
) -> CXResult<MIRComptimeValue> {
    let captured = capture_expression(context, expr)?;

    evaluate_comptime_function(context, &captured, &[])
}

fn constant_error(expression: &THIRExpression, context: &str) -> cx_log::error::CXError {
    mir_error(
        &expression.token_range,
        (&mir::MIR_CONSTANT_VALUE, context.to_owned()),
    )
}

pub(crate) fn evaluate_integer(
    builder: &mut MIRBuilder<'_>,
    expression: &THIRExpression,
    context: &str,
) -> CXResult<usize> {
    let value = evaluate_comptime_expr(builder, expression)?;
    match value {
        MIRComptimeValue::Constant(MIRConstant::Integer { value, .. }) => {
            usize::try_from(value).map_err(|_| constant_error(expression, context))
        }
        _ => Err(constant_error(expression, context)),
    }
}

pub(crate) fn evaluate(
    builder: &mut MIRBuilder<'_>,
    expression: &THIRExpression,
) -> CXResult<MIRConstant> {
    let value = evaluate_comptime_expr(builder, expression)?;
    match value {
        MIRComptimeValue::Constant(value) => Ok(value),
        MIRComptimeValue::Staged(_) => Err(constant_error(expression, "staged expression")),
    }
}
