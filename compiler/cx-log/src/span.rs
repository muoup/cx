use std::path::PathBuf;

use crate::error::{CXErrorTrait, UnderlineError, UnspannedError};

#[derive(Clone, Debug, PartialEq, Eq)]
pub struct DiagnosticSpan {
    pub compilation_unit: PathBuf,
    pub byte_start: usize,
    pub byte_end: usize,
}

impl DiagnosticSpan {
    pub fn new(compilation_unit: impl Into<PathBuf>, byte_start: usize, byte_end: usize) -> Self {
        Self {
            compilation_unit: compilation_unit.into(),
            byte_start,
            byte_end: byte_end.max(byte_start.saturating_add(1)),
        }
    }
}

#[derive(Clone, Debug, PartialEq, Eq)]
pub struct DiagnosticPointer {
    pub compilation_unit: PathBuf,
    pub point: usize,
    pub diagnostic_start: usize,
    pub diagnostic_end: usize,
}

impl DiagnosticPointer {
    pub fn new(compilation_unit: impl Into<PathBuf>, point: usize) -> Self {
        Self {
            compilation_unit: compilation_unit.into(),
            point,
            diagnostic_start: point,
            diagnostic_end: point.saturating_add(1),
        }
    }

    pub fn with_diagnostic_range(mut self, start: usize, end: usize) -> Self {
        self.diagnostic_start = start;
        self.diagnostic_end = end.max(start.saturating_add(1));
        self
    }
}

pub fn produce_diagnostic_error(
    prefix: impl Into<String>,
    message: String,
    notes: Vec<String>,
    span: Option<DiagnosticSpan>,
) -> Box<dyn CXErrorTrait> {
    let prefix = prefix.into();
    let Some(span) = span else {
        return Box::new(UnspannedError::new(prefix, message).with_notes(notes));
    };

    Box::new(UnderlineError::new(prefix, message, span).with_notes(notes))
}
