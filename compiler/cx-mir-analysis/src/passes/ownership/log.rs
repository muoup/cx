use crate::{framework::environment::Location, log::analysis_error};
use cx_log::{catalogue::ErrorDefinition, error::CXError};
use cx_mir::{MIRFunction, MIRPlace};

pub(crate) fn ownership_error<T, F>(
    function: &MIRFunction,
    block: cx_mir::MIRBasicBlockID,
    instruction: usize,
    scope: Option<cx_mir::MIRScopeID>,
    place: MIRPlace,
    definition: &ErrorDefinition<T>,
    name: String,
    make_args: F,
) -> CXError
where
    F: FnOnce(String, String, bool) -> T,
{
    let discarded = match place {
        MIRPlace::FunctionLocal(id) => function
            .body()
            .and_then(|definition| definition.place(id))
            .and_then(|declaration| declaration.debug_name.as_ref())
            .is_some_and(|name| name.as_str() == "_"),
        _ => false,
    };
    analysis_error(
        function,
        Location { block, instruction },
        scope,
        (
            definition,
            make_args(
                function.prototype().signature.display_name().to_string(),
                name,
                discarded,
            ),
        ),
    )
}
