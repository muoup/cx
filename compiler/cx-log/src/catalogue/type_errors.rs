use std::cell::LazyCell;

use cx_tokens::TokenRange;

use crate::error::{CXError, context::{CXInternalContext, CXUnderlineContext}, message::CXStdErrMessage};

pub type StandardTypeError = Box<dyn FnOnce(TokenRange) -> CXError>; 

pub const fn standard_type_error(code: usize, message: impl Into<String>) -> LazyCell<StandardTypeError> {
    LazyCell::new(move || {
        let message = message.into();
        
        Box::new(move |token_range: TokenRange| {
            CXError::new(
                CXStdErrMessage::error(format!("T{code}"), message.clone()),
                CXUnderlineContext::new(token_range.namespace()
            )
        })
    })
}

thread_local! {
    pub static ASSIGN_TO_CONST: LazyCell<StandardTypeError> = standard_type_error(0001, "Identifier not found");
    
}