use cx_log::{
    CXResult,
    catalogue::ErrorDefinition,
    error::{
        CXError, CXRawError,
        context::{CXInternalContext, from_token_range},
    },
};
use cx_tokens::TokenRange;

pub fn generate_type_error<A>(
    range: &TokenRange,
    definition: &ErrorDefinition<A>,
    args: A,
    notes: Vec<String>,
) -> CXError {
    CXError::new(
        definition.bind(args).with_notes(notes),
        from_token_range(range),
    )
}

pub fn generate_raw_error<A>(definition: &ErrorDefinition<A>, args: A) -> CXRawError {
    definition.bind(args)
}

pub fn internal_type_error<T, A>(definition: &ErrorDefinition<A>, args: A) -> CXResult<T> {
    Err(CXError::new(
        generate_raw_error(definition, args),
        CXInternalContext::error("typechecker diagnostic has no source range"),
    ))
}
