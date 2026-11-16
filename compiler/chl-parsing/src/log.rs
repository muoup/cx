pub(crate) use chl_log::catalogue::parse::{EXPECTED_SYNTAX, UNEXPECTED_END};
use chl_log::{
    catalogue::ErrorDefinition,
    error::{
        context::{from_token_range, CHLPointingContext},
        CHLError, CHLErrorContext,
    },
    CHLResult,
};
use chl_tokens::{TokenIter, TokenRange};

fn pointing_context(tokens: &TokenIter<'_>) -> CHLErrorContext {
    if let Some(token) = tokens.peek().or_else(|| tokens.prev()) {
        CHLPointingContext::error(
            token.file_origin.as_ref().to_path_buf(),
            token.byte_start_index,
        )
    } else {
        CHLPointingContext::error(tokens.file.clone(), 0)
    }
}

fn range_context(range: &TokenRange) -> CHLErrorContext {
    from_token_range(range)
}

fn parse_error<A>(
    definition: &ErrorDefinition<A>,
    args: A,
    context: CHLErrorContext,
) -> CHLError {
    CHLError::new(definition.bind(args), context)
}

pub fn parse_point_error<T, A>(
    tokens: &TokenIter<'_>,
    definition: &ErrorDefinition<A>,
    args: A,
) -> CHLResult<T> {
    Err(parse_error(definition, args, pointing_context(tokens)))
}

pub fn parse_underline_error<T, A>(
    definition: &ErrorDefinition<A>,
    args: A,
    range: &TokenRange,
) -> CHLResult<T> {
    Err(parse_error(definition, args, range_context(range)))
}
