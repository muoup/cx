use cx_log::{
    CXResult,
    catalogue::ErrorDefinition,
    error::{
        CXError,
        context::{CXInternalContext, from_token_range},
    },
};
use cx_tokens::TokenRange;

pub fn internal_error<A>(definition: &ErrorDefinition<A>, args: A, context: &str) -> CXError {
    CXError::new(definition.bind(args), CXInternalContext::error(context))
}

pub fn comptime_error<T, A>(
    token_range: TokenRange,
    diagnostic: (&ErrorDefinition<A>, A),
) -> CXResult<T> {
    Err(CXError::new(
        diagnostic.0.bind(diagnostic.1),
        from_token_range(&token_range),
    ))
}
