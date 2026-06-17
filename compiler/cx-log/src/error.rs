use std::fmt::Formatter;
use std::path::PathBuf;

use crate::pretty::{pretty_point_error, pretty_underline_error_with_notes};
use crate::span::{DiagnosticPointer, DiagnosticSpan};

pub trait CXErrorTrait {
    fn pretty_print(&self);

    /// Attempt to downcast this error to a concrete type.
    /// Returns Some if the error is of the given type, None otherwise.
    fn as_any(&self) -> &dyn std::any::Any {
        &()
    }

    fn error_prefix(&self) -> String;

    fn error_content(&self) -> String;

    /// Get the error as a string for LSP diagnostics
    fn error_message(&self) -> String {
        format!("{}: {}", self.error_prefix(), self.error_content())
    }

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

    /// Get any supplementary notes associated with this error, if applicable.
    fn notes(&self) -> Vec<String> {
        Vec::new()
    }
}

pub struct CXError {
    pub message: String,
}

impl CXErrorTrait for CXError {
    fn pretty_print(&self) {
        println!("CXError: {}", self.message);
    }

    fn error_prefix(&self) -> String {
        "Error".to_string()
    }

    fn error_content(&self) -> String {
        self.message.clone()
    }
}

pub type CXResult<T> = Result<T, Box<dyn CXErrorTrait>>;

impl CXError {
    pub fn new<T: Into<String>>(msg: T) -> Self {
        CXError {
            message: msg.into(),
        }
    }

    pub fn unimplemented<T, U: Into<String>>(msg: U) -> CXResult<T> {
        Err(Box::new(CXError::new(format!(
            "Unimplemented: {}",
            msg.into()
        ))))
    }

    pub fn create_result<T, U: Into<String>>(msg: U) -> CXResult<T> {
        Err(Box::new(CXError::new(msg)))
    }

    pub fn create_boxed<U: Into<String>>(msg: U) -> Box<dyn CXErrorTrait> {
        Box::new(CXError::new(msg))
    }
}

impl std::fmt::Debug for CXError {
    fn fmt(&self, f: &mut Formatter<'_>) -> std::fmt::Result {
        write!(f, "CXError: {}", self.message)
    }
}

impl std::fmt::Display for CXError {
    fn fmt(&self, f: &mut Formatter<'_>) -> std::fmt::Result {
        write!(f, "CXError: {}", self.message)
    }
}

#[derive(Clone, Debug)]
pub struct UnspannedError {
    pub prefix: String,
    pub message: String,
    pub notes: Vec<String>,
}

impl UnspannedError {
    pub fn new(prefix: impl Into<String>, message: impl Into<String>) -> Self {
        Self {
            prefix: prefix.into(),
            message: message.into(),
            notes: Vec::new(),
        }
    }

    pub fn with_notes(mut self, notes: Vec<String>) -> Self {
        self.notes = notes;
        self
    }
}

impl CXErrorTrait for UnspannedError {
    fn pretty_print(&self) {
        println!("{}", self.error_message());
        for note in &self.notes {
            println!("note: {note}");
        }
    }

    fn error_prefix(&self) -> String {
        self.prefix.clone()
    }

    fn error_content(&self) -> String {
        self.message.clone()
    }

    fn notes(&self) -> Vec<String> {
        self.notes.clone()
    }

    fn as_any(&self) -> &dyn std::any::Any {
        self
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

impl CXErrorTrait for PointingError {
    fn pretty_print(&self) {
        pretty_point_error(
            &self.message,
            self.pointer.compilation_unit.as_path(),
            self.pointer.point,
        );
    }

    fn error_prefix(&self) -> String {
        self.prefix.clone()
    }

    fn error_content(&self) -> String {
        self.message.clone()
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

    fn notes(&self) -> Vec<String> {
        self.notes.clone()
    }

    fn as_any(&self) -> &dyn std::any::Any {
        self
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

    pub fn legacy(
        prefix: impl Into<String>,
        message: impl Into<String>,
        file: PathBuf,
        byte_start: usize,
        byte_end: usize,
    ) -> Self {
        Self::new(
            prefix,
            message,
            DiagnosticSpan::new(file, byte_start, byte_end),
        )
    }

    pub fn with_notes(mut self, notes: Vec<String>) -> Self {
        self.notes = notes;
        self
    }
}

impl CXErrorTrait for UnderlineError {
    fn pretty_print(&self) {
        pretty_underline_error_with_notes(
            &self.error_message(),
            &self.notes,
            self.span.compilation_unit.as_path(),
            self.span.byte_start,
            self.span.byte_end,
        );
    }

    fn error_prefix(&self) -> String {
        self.prefix.clone()
    }

    fn error_content(&self) -> String {
        self.message.clone()
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

    fn notes(&self) -> Vec<String> {
        self.notes.clone()
    }

    fn as_any(&self) -> &dyn std::any::Any {
        self
    }
}
