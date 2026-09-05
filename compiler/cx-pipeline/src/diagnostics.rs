use cx_log::error::{
    CXError,
    context::{CXInternalContext, from_token_range},
    message::CXStdErrMessage,
};
use cx_mir::{MIRDiagnostic, MIRDiagnosticLocation, MIRUnit};

pub(crate) fn mir_diagnostic_error(
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
            .map(from_token_range)
            .unwrap_or_else(|| {
                CXInternalContext::error("MIR diagnostic instruction has no source context")
            }),
        MIRDiagnosticLocation::Scope { function, scope } => mir
            .and_then(|mir| mir.scope_range(*function, *scope))
            .map(from_token_range)
            .unwrap_or_else(|| {
                CXInternalContext::error("MIR diagnostic scope has no source context")
            }),
        MIRDiagnosticLocation::TokenRange(range) => from_token_range(range),
        MIRDiagnosticLocation::Internal(message) => CXInternalContext::error(message.clone()),
    };

    let mut message = diagnostic.message().to_owned();
    for note in diagnostic.notes() {
        message.push_str("\nnote: ");
        message.push_str(note);
    }

    CXError::new(CXStdErrMessage::error(diagnostic.code(), message), context)
}
