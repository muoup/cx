use cx_log::error::{CXError, context::CXInternalContext};
use cx_log::error::message::CXStdErrMessage;
use std::fmt::{Display, Formatter};

#[derive(Debug)]
pub(crate) struct LLVMError {
    message: String,
}

impl LLVMError {
    pub(crate) fn new(message: impl Into<String>) -> Self {
        Self {
            message: message.into(),
        }
    }

    pub(crate) fn from_error(error: impl Display) -> Self {
        Self::new(error.to_string())
    }
}

impl Display for LLVMError {
    fn fmt(&self, formatter: &mut Formatter<'_>) -> std::fmt::Result {
        formatter.write_str(&self.message)
    }
}

impl From<LLVMError> for CXError {
    fn from(error: LLVMError) -> Self {
        CXError::new(
            CXStdErrMessage::error("INTERNAL LLVM ERROR", error.to_string()),
            CXInternalContext::error(
                "Internal error while generating LLVM; please report this issue.",
            ),
        )
    }
}

pub(crate) type LLVMResult<T> = Result<T, LLVMError>;
