use cx_log::{
    error::{
        context::{from_token_range, CXPointingContext},
        message::CXStdErrMessage,
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

fn parse_error(message: impl Into<String>, context: cx_log::error::CXErrorContext) -> CXError {
    CXError::new(
        CXStdErrMessage::error("PARSER ERROR", message.into()),
        context,
    )
}

pub fn parse_point_error<T>(tokens: &TokenIter<'_>, message: impl Into<String>) -> CXResult<T> {
    CXResult::Err(parse_error(message, pointing_context(tokens)))
}

pub fn parse_underline_error<T>(
    message: impl Into<String>,
    range: &TokenRange,
) -> CXResult<T> {
    CXResult::Err(parse_error(message, range_context(range)))
}
