use std::cell::LazyCell;

use cx_tokens::TokenRange;

use crate::error::{
    context::from_token_range,
    message::CXStdErrMessage,
    CXError,
};

pub type StandardTypeError = Box<dyn FnOnce(TokenRange) -> CXError>;

pub fn standard_type_error(code: usize, message: impl Into<String>) -> StandardTypeError {
    let message = message.into();
    Box::new(move |token_range| {
        CXError::new(
            CXStdErrMessage::error(format!("T{code}"), message),
            from_token_range(&token_range),
        )
    })
}

thread_local! {
    pub static ASSIGN_TO_CONST: LazyCell<StandardTypeError> = LazyCell::new(|| standard_type_error(0001, "Identifier not found"));
}
