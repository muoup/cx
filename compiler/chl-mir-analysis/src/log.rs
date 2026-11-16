use chl_log::{
    CHLResult,
    catalogue::ErrorDefinition,
    error::{CHLError, CHLErrorMaybeRaw, context::from_token_range},
};
use chl_tokens::TokenRange;

pub fn analysis_error<A>(range: &TokenRange, diagnostic: (&ErrorDefinition<A>, A)) -> CHLError {
    CHLError::new(diagnostic.0.bind(diagnostic.1), from_token_range(range))
}

pub fn complete_analysis_error(range: &TokenRange, error: CHLErrorMaybeRaw) -> CHLError {
    match error {
        CHLErrorMaybeRaw::Raw(error) => CHLError::new(error, from_token_range(range)),
        CHLErrorMaybeRaw::Complete(error) => error,
    }
}

pub fn log_analysis_error<T, A>(
    range: &TokenRange,
    diagnostic: (&ErrorDefinition<A>, A),
) -> CHLResult<T> {
    Err(analysis_error(range, diagnostic))
}
