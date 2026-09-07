use cx_log::{
    CXResult,
    error::{CXError, context::CXInternalContext, message::CXStdErrMessage},
};
use cx_mir::{MIRConstant, MIRFunction};
use cx_mir_comptime::{ComptimeResolver, InterpretedFunction, MIRComptimeEngine, MIRComptimeValue};
use cx_thir::thir::expression::THIRExpression;

use crate::builder::MIRBuilder;

pub trait MIRContext {
    fn comptime_resolver(&self) -> &dyn ComptimeResolver;
    fn capture_expression(&mut self, expression: &THIRExpression) -> CXResult<MIRFunction>;
}

pub(crate) fn evaluate_comptime_expr<T: MIRContext>(
    context: &mut T,
    expr: &THIRExpression,
) -> CXResult<MIRComptimeValue> {
    let function = context.capture_expression(expr)?;
    let mut engine = MIRComptimeEngine::new(context.comptime_resolver());
    let entry = InterpretedFunction::new(&function)
        .expect("captured comptime functions have definitions");
    Ok(MIRComptimeValue::Constant(engine.run(entry, &[])?))
}

fn constant_error(context: &str) -> CXError {
    CXError::new(
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
    let value = evaluate_comptime_expr(builder, expression)?;
    match value {
        MIRComptimeValue::Constant(value) => Ok(value),
        MIRComptimeValue::Staged(_) => Err(constant_error("staged expression")),
    }
}
