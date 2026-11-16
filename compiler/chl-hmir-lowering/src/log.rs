use chl_log::{
    CHLResult,
    catalogue::ErrorDefinition,
    error::{CHLError, context::from_token_range},
};
use chl_tokens::TokenRange;

pub(crate) fn log_hmir_error<A, T>(
    span: &TokenRange,
    definition: &ErrorDefinition<A>,
    args: A,
) -> CHLResult<T> {
    Err(hmir_error(span, definition, args))
}

pub(crate) fn hmir_error<A>(
    span: &TokenRange,
    definition: &ErrorDefinition<A>,
    args: A,
) -> CHLError {
    CHLError::new(definition.bind(args), from_token_range(span))
}
