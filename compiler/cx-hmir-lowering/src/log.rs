use cx_log::{
    catalogue::ErrorDefinition,
    error::{CXError, context::from_token_range},
};
use cx_tokens::TokenRange;

pub(crate) fn hmir_error<A>(
    span: &TokenRange,
    definition: &ErrorDefinition<A>,
    args: A,
) -> CXError {
    CXError::new(definition.bind(args), from_token_range(span))
}
