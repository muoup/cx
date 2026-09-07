use cx_log::{
    CXResult,
    error::{CXError, context::from_token_range, message::CXStdErrMessage},
};
use cx_tokens::TokenRange;

pub fn mir_error(token_range: &TokenRange, message: impl Into<String>) -> CXError {
    CXError::new(
        CXStdErrMessage::error("MIR ERROR", message.into()),
        from_token_range(token_range),
    )
}

pub fn log_mir_error<T>(token_range: &TokenRange, message: impl Into<String>) -> CXResult<T> {
    Err(mir_error(token_range, message))
}