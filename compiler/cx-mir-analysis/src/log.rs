use crate::framework::environment::Location;
use cx_log::{
    catalogue::ErrorDefinition,
    error::{
        CXError,
        context::{CXInternalContext, from_token_range},
    },
};
use cx_mir::{MIRFunction, MIRScopeID};

pub(crate) fn analysis_error<A>(
    function: &MIRFunction,
    location: Location,
    scope: Option<MIRScopeID>,
    diagnostic: (&ErrorDefinition<A>, A),
) -> CXError {
    let range = function.body().and_then(|body| match scope {
        Some(scope) => body.scope(scope).map(|scope| &scope.token_range),
        None => body.instruction_range(location.block, location.instruction),
    });
    let context = range
        .map(from_token_range)
        .unwrap_or_else(|| CXInternalContext::error("MIR analysis has no source context"));
    CXError::new(diagnostic.0.bind(diagnostic.1), context)
}
