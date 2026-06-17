use std::io::Write;
use std::{io, path::PathBuf};

use crate::span::{DiagnosticPointer, DiagnosticSpan};
use crate::{point_error, pretty_underline_error, write_unspanned};

pub type CXResult<T> = Result<T, Box<dyn CXError>>;
pub type CXRawResult<T> = Result<T, Box<dyn CXErrorMessage>>;

pub fn complete_raw_result<T>(
    result: CXRawResult<T>,
    context: Box<dyn CXErrorContext>,
) -> CXResult<T> {
    result.map_err(|msg| {
        Box::new(CXComposedError {
            error: msg,
            context,
        }) as Box<dyn CXError>
    })
}

pub trait CXErrorMessage {
    fn error_prefix(&self) -> String;

    fn error_content(&self) -> String;

    fn error_message(&self) -> String {
        format!("{}: {}", self.error_prefix(), self.error_content())
    }
}

pub trait CXErrorContext {
    fn compilation_unit(&self) -> Option<PathBuf> {
        None
    }

    fn byte_start(&self) -> Option<usize> {
        None
    }

    fn byte_end(&self) -> Option<usize> {
        None
    }

    fn notes(&self) -> &[String] {
        &[]
    }
}

pub trait CXError: CXErrorMessage + CXErrorContext {
    fn print(&self, f: &mut dyn Write) -> io::Result<()> {
        if let (Some(compilation_unit), Some(byte_start), Some(byte_end)) =
            (self.compilation_unit(), self.byte_start(), self.byte_end())
        {
            pretty_underline_error(
                f,
                &self.error_message(),
                self.notes(),
                compilation_unit.as_path(),
                byte_start,
                byte_end,
            )
        } else {
            write_unspanned(f, &self.error_message(), self.notes())
        }
    }
}

pub struct CXErrorBase {
    pub message: String,
}

impl CXErrorMessage for CXErrorBase {
    fn error_prefix(&self) -> String {
        "ERROR".to_string()
    }

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

    pub fn raw_result<T, U: Into<String>>(msg: U) -> CXRawResult<T> {
        Err(Box::new(CXErrorBase::new(msg)))
    }

    pub fn raw_boxed<U: Into<String>>(msg: U) -> Box<dyn CXErrorMessage> {
        Box::new(CXErrorBase::new(msg))
    }
}

#[derive(Clone, Debug)]
pub struct CXUnspannedError {
    pub prefix: String,
    pub message: String,
    pub notes: Vec<String>,
}

impl CXUnspannedError {
    pub fn new(prefix: impl Into<String>, message: impl Into<String>) -> Self {
        Self {
            prefix: prefix.into(),
            message: message.into(),
            notes: Vec::new(),
        }
    }

    pub fn boxed(prefix: impl Into<String>, message: impl Into<String>) -> Box<dyn CXError> {
        Box::new(Self::new(prefix, message))
    }

    pub fn result<T>(prefix: impl Into<String>, message: impl Into<String>) -> CXResult<T> {
        Err(Self::boxed(prefix, message))
    }

    pub fn with_notes(mut self, notes: Vec<String>) -> Self {
        self.notes = notes;
        self
    }
}

impl CXError for CXUnspannedError {}

impl CXErrorMessage for CXUnspannedError {
    fn error_prefix(&self) -> String {
        self.prefix.clone()
    }

    fn error_content(&self) -> String {
        self.message.clone()
    }
}

impl CXErrorContext for CXUnspannedError {
    fn notes(&self) -> &[String] {
        &self.notes
    }
}

pub struct CXComposedError {
    pub error: Box<dyn CXErrorMessage>,
    pub context: Box<dyn CXErrorContext>,
}

impl CXError for CXComposedError {}

impl CXErrorMessage for CXComposedError {
    fn error_prefix(&self) -> String {
        self.error.error_prefix()
    }

    fn error_content(&self) -> String {
        self.error.error_content()
    }
}

impl CXErrorContext for CXComposedError {
    fn compilation_unit(&self) -> Option<PathBuf> {
        self.context.compilation_unit()
    }

    fn byte_start(&self) -> Option<usize> {
        self.context.byte_start()
    }

    fn byte_end(&self) -> Option<usize> {
        self.context.byte_end()
    }

    fn notes(&self) -> &[String] {
        self.context.notes()
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

    pub fn with_diagnostic_range(mut self, start: usize, end: usize) -> Self {
        self.pointer = self.pointer.with_diagnostic_range(start, end);
        self
    }
}

impl CXError for PointingError {
    fn print(&self, f: &mut dyn Write) -> io::Result<()> {
        point_error(
            f,
            &self.error_message(),
            self.notes(),
            self.pointer.compilation_unit.as_path(),
            self.pointer.point,
        )
    }
}

impl CXErrorMessage for PointingError {
    fn error_prefix(&self) -> String {
        self.prefix.clone()
    }

    fn error_content(&self) -> String {
        self.message.clone()
    }
}

impl CXErrorContext for PointingError {
    fn compilation_unit(&self) -> Option<PathBuf> {
        Some(self.pointer.compilation_unit.clone())
    }

    fn byte_start(&self) -> Option<usize> {
        Some(self.pointer.diagnostic_start)
    }

    fn byte_end(&self) -> Option<usize> {
        Some(self.pointer.diagnostic_end)
    }

    fn notes(&self) -> &[String] {
        &self.notes
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

impl CXError for UnderlineError {}

impl CXErrorMessage for UnderlineError {
    fn error_prefix(&self) -> String {
        self.prefix.clone()
    }

    fn error_content(&self) -> String {
        self.message.clone()
    }
}

impl CXErrorContext for UnderlineError {
    fn compilation_unit(&self) -> Option<PathBuf> {
        Some(self.span.compilation_unit.clone())
    }

    fn byte_start(&self) -> Option<usize> {
        Some(self.span.byte_start)
    }

    fn byte_end(&self) -> Option<usize> {
        Some(self.span.byte_end)
    }

    fn notes(&self) -> &[String] {
        &self.notes
    }
}
