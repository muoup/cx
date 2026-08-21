use cx_log::{
    CXResult,
    error::{CXErr, context::CXInternalContext, message::CXStdErrMessage},
};
use cx_mir::{MIRConstant, MIRInstrKind};
use cx_mir_comptime::MIRComptimeEngine;
use cx_thir::thir::expression::THIRExpression;

use crate::MIRBuilder;

pub(super) fn evaluate(
    builder: &mut MIRBuilder<'_>,
    expression: &THIRExpression,
) -> CXResult<MIRConstant> {
    let function = builder.start_comptime_function(expression)?;
    let lowered = (|| {
        let value = super::lower_expression(builder, expression)?;
        if !builder.current_block_terminated() {
            builder.emit(MIRInstrKind::Return { value: Some(value) });
        }
        Ok::<(), CXErr>(())
    })();
    let function_definition = builder.finish_comptime_function();
    lowered?;

    let unit = builder.comptime_unit(function_definition);
    MIRComptimeEngine::new(&unit)
        .evaluate(function, &[])
        .map_err(comptime_error)
}

pub(super) fn evaluate_integer(
    builder: &mut MIRBuilder<'_>,
    expression: &THIRExpression,
    context: &str,
) -> CXResult<usize> {
    let value = evaluate(builder, expression)?;
    let MIRConstant::Integer { value, .. } = value else {
        return Err(CXErr::new(
            CXStdErrMessage::error(
                "COMPTIME ERROR",
                format!("{context} must evaluate to an integer"),
            ),
            CXInternalContext::error("MIR comptime expression returned a non-integer value"),
        ));
    };

    usize::try_from(value).map_err(|_| {
        CXErr::new(
            CXStdErrMessage::error(
                "COMPTIME ERROR",
                format!("{context} is outside the supported array length range"),
            ),
            CXInternalContext::error("MIR comptime integer could not be represented as usize"),
        )
    })
}

fn comptime_error(error: cx_mir_comptime::MIRComptimeError) -> CXErr {
    let diagnostic = error.diagnostic();
    let mut message = diagnostic.message().to_owned();
    for note in diagnostic.notes() {
        message.push_str("\nnote: ");
        message.push_str(note);
    }
    CXErr::new(
        CXStdErrMessage::error(diagnostic.code(), message),
        CXInternalContext::error(format!(
            "MIR comptime evaluation failed at {:?}",
            diagnostic.location()
        )),
    )
}
