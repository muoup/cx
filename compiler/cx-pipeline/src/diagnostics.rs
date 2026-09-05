use cx_log::error::{CXError, context::CXInternalContext, message::CXStdErrMessage};
use cx_mir::{MIRDiagnostic, MIRDiagnosticLocation, MIRUnit};
use cx_pipeline_data::db::ModuleData;

pub(crate) fn mir_diagnostic_error(
    module_data: &ModuleData,
    mir: Option<&MIRUnit>,
    diagnostic: MIRDiagnostic,
) -> CXError {
    let context = match diagnostic.location() {
        MIRDiagnosticLocation::Instruction {
            function,
            block,
            instruction,
        } => mir
            .and_then(|mir| mir.instruction_range(*function, *block, *instruction))
            .map(|range| module_data.convert_token_range(range))
            .unwrap_or_else(|| {
                CXInternalContext::error("MIR diagnostic instruction has no source context")
            }),
        MIRDiagnosticLocation::Scope { function, scope } => mir
            .and_then(|mir| mir.scope_range(*function, *scope))
            .map(|range| module_data.convert_token_range(range))
            .unwrap_or_else(|| {
                CXInternalContext::error("MIR diagnostic scope has no source context")
            }),
        MIRDiagnosticLocation::TokenRange(range) => module_data.convert_token_range(range),
        MIRDiagnosticLocation::Internal(message) => CXInternalContext::error(message.clone()),
    };

    let mut message = diagnostic.message().to_owned();
    for note in diagnostic.notes() {
        message.push_str("\nnote: ");
        message.push_str(note);
    }

    CXError::new(CXStdErrMessage::error(diagnostic.code(), message), context)
}
