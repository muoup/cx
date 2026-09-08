use cx_log::{
    CXResult,
    catalogue::ErrorDefinition,
    error::{CXError, context::from_token_range},
};
use cx_tokens::TokenRange;

pub fn mir_error<A>(range: &TokenRange, diagnostic: (&ErrorDefinition<A>, A)) -> CXError {
    CXError::new(diagnostic.0.bind(diagnostic.1), from_token_range(range))
}

pub fn log_mir_error<T, A>(
    range: &TokenRange,
    diagnostic: (&ErrorDefinition<A>, A),
) -> CXResult<T> {
    Err(mir_error(range, diagnostic))
}
