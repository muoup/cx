use std::path::PathBuf;

use crate::{
    error::CXErrContext,
    format::{pointing::point_error, underline::pretty_underline_error},
};

pub trait CXErrorContext {
    fn dump(&self, f: &mut dyn std::io::Write) -> std::io::Result<()>;
}

pub struct CXPointingContext {
    file: PathBuf,
    str_index: usize,
}

impl CXPointingContext {
    pub fn new(file: PathBuf, str_index: usize) -> Self {
        Self { file, str_index }
    }

    pub fn error(file: impl Into<PathBuf>, str_index: usize) -> CXErrContext {
        Box::new(Self::new(file.into(), str_index))
    }
}

impl CXErrorContext for CXPointingContext {
    fn dump(&self, f: &mut dyn std::io::Write) -> std::io::Result<()> {
        point_error(f, self.file.as_path(), self.str_index)
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

    pub fn error(file: impl Into<PathBuf>, str_start: usize, str_end: usize) -> CXErrContext {
        Box::new(Self::new(file.into(), str_start, str_end))
    }
}

impl CXErrorContext for CXUnderlineContext {
    fn dump(&self, f: &mut dyn std::io::Write) -> std::io::Result<()> {
        pretty_underline_error(f, self.file.as_path(), self.str_start, self.str_end)
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

    pub fn error(message: impl Into<String>) -> CXErrContext {
        Box::new(Self::new(message))
    }
}

impl CXErrorContext for CXInternalContext {
    fn dump(&self, f: &mut dyn std::io::Write) -> std::io::Result<()> {
        writeln!(f, "{}", self.message)
    }
}
