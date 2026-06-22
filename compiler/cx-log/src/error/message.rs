use crate::error::{CXErrMsg, CXRawResult};

pub trait CXErrorMessage {
    fn code(&self) -> String;

    fn message(&self) -> String;

    fn dump(&self, f: &mut dyn std::io::Write) -> std::io::Result<()> {
        write!(f, "{}: {}", self.code(), self.message())
    }
}

pub struct CXStdErrMessage {
    code: String,
    message: String,
}

impl CXErrMsg {
    pub fn code(&self) -> String {
        self.0.code()
    }

    pub fn message(&self) -> String {
        self.0.message()
    }
}

impl CXStdErrMessage {
    pub fn new(code: impl Into<String>, message: impl Into<String>) -> Self {
        Self {
            code: code.into(),
            message: message.into(),
        }
    }

    pub fn error(code: impl Into<String>, message: impl Into<String>) -> CXErrMsg {
        CXErrMsg(Box::new(Self::new(code, message)))
    }

    pub fn result<T>(code: impl Into<String>, message: impl Into<String>) -> CXRawResult<T> {
        Err(CXErrMsg(Box::new(Self::new(code, message))))
    }
}

impl CXErrorMessage for CXStdErrMessage {
    fn code(&self) -> String {
        self.code.clone()
    }

    fn message(&self) -> String {
        self.message.clone()
    }
}
