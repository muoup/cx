use cx_log::{
    CXResult,
    catalogue::ErrorDefinition,
    error::{CXError, CXErrorMaybeRaw, context::from_token_range},
};
use cx_tokens::TokenRange;

pub fn analysis_error<A>(range: &TokenRange, diagnostic: (&ErrorDefinition<A>, A)) -> CXError {
    CXError::new(diagnostic.0.bind(diagnostic.1), from_token_range(range))
}

pub fn complete_analysis_error(range: &TokenRange, error: CXErrorMaybeRaw) -> CXError {
    match error {
        CXErrorMaybeRaw::Raw(error) => CXError::new(error, from_token_range(range)),
        CXErrorMaybeRaw::Complete(error) => error,
    }
}

pub fn log_analysis_error<T, A>(
    range: &TokenRange,
    diagnostic: (&ErrorDefinition<A>, A),
) -> CXResult<T> {
    Err(analysis_error(range, diagnostic))
}
