use std::path::PathBuf;

use crate::span::{DiagnosticPointer, DiagnosticSpan};

pub type CXResult<T> = Result<T, Box<dyn CXError>>;
pub struct CXRawResult<T>(Result<T, Box<dyn CXErrorMessage>>);

impl<T> CXRawResult<T> {
    pub fn new(result: Result<T, Box<dyn CXErrorMessage>>) -> Self {
        CXRawResult(result)
    }

    pub fn complete(self, context: Box<dyn CXErrorContext>) -> CXResult<T> {
        self.0.map_err(|msg| {
            Box::new(CXComposedError {
                error: msg,
                context,
            }) as Box<dyn CXError>
        })
    }
}

pub trait CXErrorMessage {
    fn error_content(&self) -> String;
}

pub trait CXErrorContext {
    fn error_prefix(&self) -> String;

    /// Get the compilation unit for this error, if applicable
    fn compilation_unit(&self) -> Option<PathBuf> {
        None
    }

    /// Get the byte start for this error, if applicable.
    fn byte_start(&self) -> Option<usize> {
        None
    }

    /// Get the byte end for this error, if applicable.
    fn byte_end(&self) -> Option<usize> {
        None
    }
}

pub trait CXError: CXErrorMessage + CXErrorContext {}

pub struct CXErrorBase {
    pub message: String,
}

impl CXErrorMessage for CXErrorBase {
    fn error_content(&self) -> String {
        self.message.clone()
    }
}

impl CXErrorBase {
    pub fn new<T: Into<String>>(msg: T) -> Self {
        CXErrorBase {
            message: msg.into(),
        }
    }

    pub fn create_result<T, U: Into<String>>(msg: U) -> CXRawResult<T> {
        CXRawResult::new(Err(Box::new(CXErrorBase::new(msg))))
    }

    pub fn create_boxed<U: Into<String>>(msg: U) -> Box<dyn CXErrorMessage> {
        Box::new(CXErrorBase::new(msg))
    }
}

#[derive(Clone, Debug)]
pub struct CXComposedError {
    pub error: Box<dyn CXErrorMessage>,
    pub context: Box<dyn CXErrorContext>,
}

impl CXErrorMessage for CXComposedError {
    fn error_content(&self) -> String {
        self.error.error_content()
    }
}

impl CXErrorContext for CXComposedError {
    fn error_prefix(&self) -> String {
        self.context.error_prefix()
    }

    fn compilation_unit(&self) -> Option<PathBuf> {
        self.context.compilation_unit()
    }

    fn byte_start(&self) -> Option<usize> {
        self.context.byte_start()
    }

    fn byte_end(&self) -> Option<usize> {
        self.context.byte_end()
    }
}

#[derive(Clone, Debug)]
pub struct PointingError {
    pub prefix: String,
    pub message: String,
    pub pointer: DiagnosticPointer,
    pub notes: Vec<String>,
}

impl PointingError {
    pub fn new(
        prefix: impl Into<String>,
        message: impl Into<String>,
        pointer: DiagnosticPointer,
    ) -> Self {
        Self {
            prefix: prefix.into(),
            message: message.into(),
            pointer,
            notes: Vec::new(),
        }
    }

    pub fn legacy(
        prefix: impl Into<String>,
        message: impl Into<String>,
        file: PathBuf,
        point: usize,
    ) -> Self {
        Self::new(prefix, message, DiagnosticPointer::new(file, point))
    }

    pub fn with_diagnostic_range(mut self, start: usize, end: usize) -> Self {
        self.pointer = self.pointer.with_diagnostic_range(start, end);
        self
    }
}

impl CXErrorMessage for PointingError {
    fn error_content(&self) -> String {
        self.message.clone()
    }
}

impl CXErrorContext for PointingError {
    fn error_prefix(&self) -> String {
        self.prefix.clone()
    }

    fn compilation_unit(&self) -> Option<PathBuf> {
        Some(self.pointer.compilation_unit.clone())
    }

    fn byte_start(&self) -> Option<usize> {
        Some(self.pointer.diagnostic_start)
    }

    fn byte_end(&self) -> Option<usize> {
        Some(self.pointer.diagnostic_end)
    }
}

#[derive(Clone, Debug)]
pub struct UnderlineError {
    pub prefix: String,
    pub message: String,
    pub span: DiagnosticSpan,
    pub notes: Vec<String>,
}

impl UnderlineError {
    pub fn new(
        prefix: impl Into<String>,
        message: impl Into<String>,
        span: DiagnosticSpan,
    ) -> Self {
        Self {
            prefix: prefix.into(),
            message: message.into(),
            span,
            notes: Vec::new(),
        }
    }

    pub fn with_notes(mut self, notes: Vec<String>) -> Self {
        self.notes = notes;
        self
    }
}

impl CXErrorMessage for UnderlineError {
    fn error_content(&self) -> String {
        self.message.clone()
    }
}

impl CXErrorContext for UnderlineError {
    fn error_prefix(&self) -> String {
        self.prefix.clone()
    }

    fn compilation_unit(&self) -> Option<PathBuf> {
        Some(self.span.compilation_unit.clone())
    }

    fn byte_start(&self) -> Option<usize> {
        Some(self.span.byte_start)
    }

    fn byte_end(&self) -> Option<usize> {
        Some(self.span.byte_end)
    }
}
