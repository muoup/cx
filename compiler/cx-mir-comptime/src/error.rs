use cx_log::{
    CXResult,
    error::{CXError, context::CXInternalContext, message::CXStdErrMessage},
};
use cx_tokens::TokenRange;

pub fn comptime_error<T>(token_range: TokenRange, message: impl Into<String>) -> CXResult<T> {
    Err(CXError::new(
        CXStdErrMessage::error("COMPTIME ERROR", message.into()),
        CXInternalContext::error(format!("at {:?}", token_range)),
    ))
}
