use crate::log::analysis_error;
use cx_log::{catalogue::ErrorDefinition, error::CXError};
use cx_mir::{MIRFunction, MIRPlaceID};

pub(crate) fn ownership_error<T, F>(
    function: &MIRFunction,
    block: cx_mir::MIRBasicBlockID,
    instruction: usize,
    scope: Option<cx_mir::MIRScopeID>,
    place: MIRPlaceID,
    definition: &ErrorDefinition<T>,
    name: String,
    make_args: F,
) -> CXError
where
    F: FnOnce(String, String, bool) -> T,
{
    let discarded = function
        .body()
        .and_then(|definition| definition.place(place))
        .and_then(|declaration| declaration.debug_name.as_ref())
        .is_some_and(|name| name.as_str() == "_");
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
