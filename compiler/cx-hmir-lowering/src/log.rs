use cx_log::{
    CXResult,
    catalogue::ErrorDefinition,
    error::{CXError, context::from_token_range},
};
use cx_tokens::TokenRange;

pub(crate) fn log_hmir_error<A, T>(
    span: &TokenRange,
    definition: &ErrorDefinition<A>,
    args: A,
) -> CXResult<T> {
    Err(hmir_error(span, definition, args))
}

pub(crate) fn hmir_error<A>(
    span: &TokenRange,
    definition: &ErrorDefinition<A>,
    args: A,
) -> CXError {
    CXError::new(definition.bind(args), from_token_range(span))
}
