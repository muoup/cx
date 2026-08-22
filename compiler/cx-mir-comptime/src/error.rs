use cx_log::{
    CXResult,
    error::{CXErr, context::CXInternalContext, message::CXStdErrMessage},
};
use cx_tokens::TokenRange;

use crate::context::MIRContext;

pub fn log_comptime_error<T, C: MIRContext + ?Sized>(
    context: &C,
    token_range: TokenRange,
    message: impl Into<String>,
) -> CXResult<T> {
    let _ = context;
    comptime_error(token_range, message)
}

pub fn comptime_error<T>(token_range: TokenRange, message: impl Into<String>) -> CXResult<T> {
    Err(CXErr::new(
        CXStdErrMessage::error("COMPTIME ERROR", message.into()),
        CXInternalContext::error(format!("at {:?}", token_range)),
    ))
}
