use std::path::PathBuf;

use cx_log::{
    error::{
        context::{CXInternalContext, CXPointingContext, CXUnderlineContext},
        message::CXStdErrMessage,
        CXErr,
    },
    CXResult,
};
use cx_tokens::{token::Token, TokenIter, TokenRange};

use crate::parse::parser::ParserData;

fn token_file(tokens: &TokenIter<'_>, token: &Token) -> PathBuf {
    if token.file_origin.as_os_str().is_empty() {
        tokens.file.clone()
    } else {
        token.file_origin.as_ref().to_path_buf()
    }
}

fn pointing_context(tokens: &TokenIter<'_>) -> cx_log::error::CXErrContext {
    if let Some(token) = tokens.peek().or_else(|| tokens.prev()) {
        CXPointingContext::error(token_file(tokens, token), token.byte_start_index)
    } else {
        CXPointingContext::error(tokens.file.clone(), 0)
    }
}

fn range_context(tokens: &TokenIter<'_>, range: &TokenRange) -> cx_log::error::CXErrContext {
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
    let Some(end) = tokens.slice.get(*end_token) else {
        return CXInternalContext::error(format!(
            "parser diagnostic end token {end_token} is out of bounds"
        ));
    };

    CXUnderlineContext::error(
        token_file(tokens, start),
        start.byte_start_index,
        end.byte_end_index,
    )
}

pub(crate) fn parse_error(
    message: impl Into<String>,
    context: cx_log::error::CXErrContext,
) -> CXErr {
    CXErr::new(
        CXStdErrMessage::error("PARSER ERROR", message.into()),
        context,
    )
}

pub(crate) fn token_iter_log_error<T>(
    tokens: &TokenIter<'_>,
    message: impl Into<String>,
) -> CXResult<T> {
    Err(parse_error(message, pointing_context(tokens)))
}

pub(crate) trait TokenIterLogExt {
    fn log_error<T>(&self, message: impl Into<String>) -> CXResult<T>;
}

impl TokenIterLogExt for TokenIter<'_> {
    fn log_error<T>(&self, message: impl Into<String>) -> CXResult<T> {
        token_iter_log_error(self, message)
    }
}

pub(crate) trait ParserLogExt {
    fn log_error<T>(&self, message: impl Into<String>) -> CXResult<T>;
    fn log_range_error<T>(&self, range: &TokenRange, message: impl Into<String>) -> CXResult<T>;
}

impl ParserLogExt for ParserData<'_> {
    fn log_error<T>(&self, message: impl Into<String>) -> CXResult<T> {
        Err(parse_error(message, pointing_context(&self.tokens)))
    }

    fn log_range_error<T>(&self, range: &TokenRange, message: impl Into<String>) -> CXResult<T> {
        Err(parse_error(message, range_context(&self.tokens, range)))
    }
}
