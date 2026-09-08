use crate::types::MIRAnalysisError;
use cx_log::catalogue::ErrorDefinition;
use cx_mir::{MIRDiagnostic, MIRDiagnosticLocation, MIRFunction, MIRPlace};

pub(crate) fn error<T>(
    definition: &ErrorDefinition<T>,
    args: T,
    location: MIRDiagnosticLocation,
) -> MIRDiagnostic {
    MIRDiagnostic::new(definition, args, location)
}

pub(crate) fn ownership_error(
    function: &MIRFunction,
    block: cx_mir::MIRBasicBlockID,
    instruction: usize,
    scope: Option<cx_mir::MIRScopeID>,
    place: MIRPlace,
    definition: &ErrorDefinition<(String, String, bool)>,
    name: String,
) -> MIRAnalysisError {
    let discarded = match place {
        MIRPlace::FunctionLocal(id) => function
            .definition()
            .and_then(|definition| definition.place(id))
            .and_then(|declaration| declaration.debug_name.as_ref())
            .is_some_and(|name| name.as_str() == "_"),
        _ => false,
    };
    let location = scope
        .map(|scope| MIRDiagnosticLocation::Scope {
            function: function.id(),
            scope,
        })
        .unwrap_or(MIRDiagnosticLocation::Instruction {
            function: function.id(),
            block,
            instruction,
        });
    MIRAnalysisError::OwnershipViolation {
        place,
        diagnostic: error(
            definition,
            (
                function.prototype().signature.display_name().to_string(),
                name,
                discarded,
            ),
            location,
        ),
    }
}
