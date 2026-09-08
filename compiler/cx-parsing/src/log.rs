pub(crate) use cx_log::catalogue::parse::{EXPECTED_TOKEN, UNEXPECTED_END_TOKENS};
use cx_log::{
    catalogue::ErrorDefinition,
    error::{
        context::{from_token_range, CXInternalContext, CXPointingContext},
        CXError,
    },
    CXResult,
};
use cx_tokens::{TokenIter, TokenRange};

fn pointing_context(tokens: &TokenIter<'_>) -> cx_log::error::CXErrorContext {
    if let Some(token) = tokens.peek().or_else(|| tokens.prev()) {
        CXPointingContext::error(
            token.file_origin.as_ref().to_path_buf(),
            token.byte_start_index,
        )
    } else {
        CXPointingContext::error(tokens.file.clone(), 0)
    }
}

fn range_context(range: &TokenRange) -> cx_log::error::CXErrorContext {
    from_token_range(range)
}

fn parse_error<A>(
    definition: &ErrorDefinition<A>,
    args: A,
    context: cx_log::error::CXErrorContext,
) -> CXError {
    CXError::new(definition.bind(args), context)
}

pub fn parse_point_error<T, A>(
    tokens: &TokenIter<'_>,
    definition: &ErrorDefinition<A>,
    args: A,
) -> CXResult<T> {
    Err(parse_error(definition, args, pointing_context(tokens)))
}

pub fn parse_underline_error<T, A>(
    definition: &ErrorDefinition<A>,
    args: A,
    range: &TokenRange,
) -> CXResult<T> {
    Err(parse_error(definition, args, range_context(range)))
}

pub fn internal_error<A>(definition: &ErrorDefinition<A>, args: A, context: &str) -> CXError {
    parse_error(definition, args, CXInternalContext::error(context))
}
