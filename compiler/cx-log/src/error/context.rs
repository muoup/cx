use std::path::PathBuf;

use crate::{
    error::CXErrorContext,
    format::{pointing::point_error, underline::pretty_underline_error},
};

#[derive(Debug, Clone, PartialEq, Eq)]
pub struct CXSourceSpan {
    pub file: PathBuf,
    pub byte_start: usize,
    pub byte_end: usize,
}

pub trait CXErrorContextTrait {
    fn dump(&self, f: &mut dyn std::io::Write) -> std::io::Result<()>;

    fn source_span(&self) -> Option<CXSourceSpan> {
        None
    }
}

pub struct CXPointingContext {
    file: PathBuf,
    str_index: usize,
}

impl CXPointingContext {
    pub fn new(file: PathBuf, str_index: usize) -> Self {
        Self { file, str_index }
    }

    pub fn error(file: impl Into<PathBuf>, str_index: usize) -> CXErrorContext {
        Box::new(Self::new(file.into(), str_index))
    }
}

impl CXErrorContextTrait for CXPointingContext {
    fn dump(&self, f: &mut dyn std::io::Write) -> std::io::Result<()> {
        point_error(f, self.file.as_path(), self.str_index)
    }

    fn source_span(&self) -> Option<CXSourceSpan> {
        Some(CXSourceSpan {
            file: self.file.clone(),
            byte_start: self.str_index,
            byte_end: self.str_index.saturating_add(1),
        })
    }
}

pub struct CXUnderlineContext {
    file: PathBuf,
    str_start: usize,
    str_end: usize,
}

impl CXUnderlineContext {
    pub fn new(file: PathBuf, str_start: usize, str_end: usize) -> Self {
        Self {
            file,
            str_start,
            str_end,
        }
    }

    pub fn error(file: impl Into<PathBuf>, str_start: usize, str_end: usize) -> CXErrorContext {
        Box::new(Self::new(file.into(), str_start, str_end))
    }
}

impl CXErrorContextTrait for CXUnderlineContext {
    fn dump(&self, f: &mut dyn std::io::Write) -> std::io::Result<()> {
        pretty_underline_error(f, self.file.as_path(), self.str_start, self.str_end)
    }

    fn source_span(&self) -> Option<CXSourceSpan> {
        Some(CXSourceSpan {
            file: self.file.clone(),
            byte_start: self.str_start,
            byte_end: self.str_end,
        })
    }
}

pub struct CXInternalContext {
    message: String,
}

impl CXInternalContext {
    pub fn new(message: impl Into<String>) -> Self {
        Self {
            message: message.into(),
        }
    }

    pub fn error(message: impl Into<String>) -> CXErrorContext {
        Box::new(Self::new(message))
    }
}

impl CXErrorContextTrait for CXInternalContext {
    fn dump(&self, f: &mut dyn std::io::Write) -> std::io::Result<()> {
        writeln!(f, "{}", self.message)
    }
}
