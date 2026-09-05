use cx_log::{
    error::{
        context::{CXInternalContext, CXPointingContext, CXUnderlineContext},
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

fn range_context(tokens: &TokenIter<'_>, range: &TokenRange) -> cx_log::error::CXErrorContext {
    let TokenRange::Source {
        start_token,
        end_token,
        ..
    } = range
    else {
        return CXInternalContext::error(format!(
            "parser diagnostic has non-source range: {range:?}"
        ));
    };

    let Some(start) = tokens.slice.get(*start_token) else {
        return CXInternalContext::error(format!(
            "parser diagnostic start token {start_token} is out of bounds"
        ));
    };
    let Some(end) = tokens.slice.get(end_token.saturating_sub(1)) else {
        return CXInternalContext::error(format!(
            "parser diagnostic end token {end_token} is out of bounds"
        ));
    };

    CXUnderlineContext::error(
        start.file_origin.as_ref().to_path_buf(),
        start.byte_start_index,
        end.byte_end_index,
    )
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
    tokens: &TokenIter<'_>,
    message: impl Into<String>,
    range: &TokenRange,
) -> CXResult<T> {
    CXResult::Err(parse_error(message, range_context(tokens, range)))
}
