use cx_log::{
    CXResult,
    catalogue::ErrorDefinition,
    error::{CXError, context::from_token_range},
};
use cx_tokens::TokenRange;

pub(crate) fn log_hir_error<A, T>(
    span: &TokenRange,
    definition: &ErrorDefinition<A>,
    args: A,
) -> CXResult<T> {
    Err(CXError::new(definition.bind(args), from_token_range(span)))
}
