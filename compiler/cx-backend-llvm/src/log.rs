use cx_log::{
    catalogue::{ErrorDefinition, backend},
    error::{CXError, context::CXNoSourceContext, message::CXStdErrMessage},
};
use std::fmt::{Display, Formatter};

#[derive(Debug)]
pub(crate) struct LLVMError {
    code: &'static str,
    message: String,
}

impl LLVMError {
    pub(crate) fn new<T>(definition: &ErrorDefinition<T>, args: T) -> Self {
        Self {
            code: definition.code,
            message: (definition.message)(args),
        }
    }

    pub(crate) fn from_error(error: impl Display) -> Self {
        Self::new(&backend::BUILDER_FAILED, ("LLVM".into(), error.to_string()))
    }
}

impl Display for LLVMError {
    fn fmt(&self, formatter: &mut Formatter<'_>) -> std::fmt::Result {
        formatter.write_str(&self.message)
    }
}

impl LLVMError {
    pub(crate) fn complete(self, context: &str) -> CXError {
        CXError::new(
            CXStdErrMessage::error(self.code, self.message),
            CXNoSourceContext::error(context),
        )
    }
}

impl From<LLVMError> for CXError {
    fn from(error: LLVMError) -> Self {
        error.complete("while generating LLVM IR")
    }
}

pub(crate) type LLVMResult<T> = Result<T, LLVMError>;
