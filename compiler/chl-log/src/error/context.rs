use std::path::PathBuf;

use chl_tokens::TokenRange;

use crate::{
    error::CHLErrorContext,
    format::{pointing::point_error, underline::pretty_underline_error},
};

pub fn from_token_range(range: &TokenRange) -> CHLErrorContext {
    match range {
        TokenRange::Source {
            file,
            byte_start,
            byte_end,
        } => CHLUnderlineContext::error(file.clone(), *byte_start, *byte_end),
        TokenRange::Internal => {
            CHLNoSourceContext::error("diagnostic originated in compiler-generated code")
        }
        TokenRange::Error(message) => {
            CHLNoSourceContext::error(format!("failed to determine source range: {message}"))
        }
    }
}

#[derive(Debug, Clone, PartialEq, Eq)]
pub struct CHLSourceSpan {
    pub file: PathBuf,
    pub byte_start: usize,
    pub byte_end: usize,
}

pub trait CHLErrorContextTrait: Send {
    fn dump(&self, f: &mut dyn std::io::Write) -> std::io::Result<()>;

    fn source_span(&self) -> Option<CHLSourceSpan> {
        None
    }
}

pub struct CHLPointingContext {
    file: PathBuf,
    str_index: usize,
}

impl CHLPointingContext {
    pub fn new(file: PathBuf, str_index: usize) -> Self {
        Self { file, str_index }
    }

    pub fn error(file: impl Into<PathBuf>, str_index: usize) -> CHLErrorContext {
        Box::new(Self::new(file.into(), str_index))
    }
}

impl CHLErrorContextTrait for CHLPointingContext {
    fn dump(&self, f: &mut dyn std::io::Write) -> std::io::Result<()> {
        point_error(f, self.file.as_path(), self.str_index)
    }

    fn source_span(&self) -> Option<CHLSourceSpan> {
        Some(CHLSourceSpan {
            file: self.file.clone(),
            byte_start: self.str_index,
            byte_end: self.str_index.saturating_add(1),
        })
    }
}

pub struct CHLUnderlineContext {
    file: PathBuf,
    str_start: usize,
    str_end: usize,
}

impl CHLUnderlineContext {
    pub fn new(file: PathBuf, str_start: usize, str_end: usize) -> Self {
        Self {
            file,
            str_start,
            str_end,
        }
    }

    pub fn error(file: impl Into<PathBuf>, str_start: usize, str_end: usize) -> CHLErrorContext {
        Box::new(Self::new(file.into(), str_start, str_end))
    }
}

impl CHLErrorContextTrait for CHLUnderlineContext {
    fn dump(&self, f: &mut dyn std::io::Write) -> std::io::Result<()> {
        pretty_underline_error(f, self.file.as_path(), self.str_start, self.str_end)
    }

    fn source_span(&self) -> Option<CHLSourceSpan> {
        Some(CHLSourceSpan {
            file: self.file.clone(),
            byte_start: self.str_start,
            byte_end: self.str_end,
        })
    }
}

pub struct CHLNoSourceContext {
    message: String,
}

impl CHLNoSourceContext {
    pub fn new(message: impl Into<String>) -> Self {
        Self {
            message: message.into(),
        }
    }

    pub fn error(message: impl Into<String>) -> CHLErrorContext {
        Box::new(Self::new(message))
    }
}

impl CHLErrorContextTrait for CHLNoSourceContext {
    fn dump(&self, f: &mut dyn std::io::Write) -> std::io::Result<()> {
        writeln!(f, "note: {}", self.message)
    }
}
