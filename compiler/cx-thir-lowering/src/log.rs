use cx_log::{
    CXResult,
    error::{CXError, context::{CXInternalContext, CXUnderlineContext}, message::CXStdErrMessage},
};
use cx_tokens::TokenRange;

pub fn log_mir_error<T>(token_range: &TokenRange, message: impl Into<String>) -> CXResult<T> {
    Err(CXError::new(
        CXStdErrMessage::error("MIR ERROR", message.into()),
        token_range.source_bounds()
            .map(|(file, start, end)| CXUnderlineContext::error(file, start, end))
            .unwrap_or_else(|| CXInternalContext::error("Failed to retrieve bounds"))
    ))
}
