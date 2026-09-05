use cx_log::{
    CXResult,
    error::{
        CXError,
        CXRawError,
        context::{CXInternalContext, from_token_range},
        message::CXStdErrMessage,
    },
};
use cx_tokens::TokenRange;

fn append_notes(mut message: String, notes: Vec<String>) -> String {
    for note in notes {
        message.push_str("\nnote: ");
        message.push_str(&note);
    }
    message
}

pub fn generate_type_error(
    range: &TokenRange,
    message: impl Into<String>,
    notes: Vec<String>,
) -> CXError {
    CXError::new(
        CXStdErrMessage::error("TYPE ERROR", append_notes(message.into(), notes)),
        from_token_range(range),
    )
}

pub fn generate_raw_error(message: impl Into<String>) -> CXRawError {
    CXStdErrMessage::error("TYPE ERROR", message)
}

pub fn internal_type_error<T>(message: impl Into<String>) -> CXResult<T> {
    Err(CXError::new(
        generate_raw_error(message),
        CXInternalContext::error("typechecker diagnostic has no source range"),
    ))
}
