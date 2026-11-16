use crate::{
    catalogue::is_internal,
    error::{CHLRawError, CHLRawResult},
};

pub trait CHLErrorMessage: Send {
    fn code(&self) -> String;

    fn message(&self) -> String;

    fn dump(&self, f: &mut dyn std::io::Write) -> std::io::Result<()> {
        let code = self.code();
        if code.is_empty() {
            write!(f, "error: {}", self.message())
        } else if is_internal(&code) {
            write!(f, "error[{code}]: internal compiler error: {}", self.message())
        } else {
            write!(f, "error[{code}]: {}", self.message())
        }
    }
}

pub struct CHLStdErrMessage {
    code: String,
    message: String,
}

impl CHLRawError {
    pub fn code(&self) -> String {
        self.0.code()
    }

    pub fn message(&self) -> String {
        self.0.message()
    }
}

impl CHLStdErrMessage {
    pub fn new(code: impl Into<String>, message: impl Into<String>) -> Self {
        Self {
            code: code.into(),
            message: message.into(),
        }
    }

    pub fn error(code: impl Into<String>, message: impl Into<String>) -> CHLRawError {
        CHLRawError(Box::new(Self::new(code, message)))
    }

    pub fn result<T>(code: impl Into<String>, message: impl Into<String>) -> CHLRawResult<T> {
        Err(CHLRawError(Box::new(Self::new(code, message))))
    }
}

impl CHLErrorMessage for CHLStdErrMessage {
    fn code(&self) -> String {
        self.code.clone()
    }

    fn message(&self) -> String {
        self.message.clone()
    }
}
