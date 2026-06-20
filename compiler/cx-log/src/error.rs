use std::io::Write;
use std::{io, path::PathBuf};

use crate::span::{DiagnosticPointer, DiagnosticSpan};
use crate::{point_error, pretty_underline_error, write_unspanned};

pub struct CXErrMsg(pub Box<dyn CXErrorMessage>);
pub enum CXMaybeRawErr {
    Raw(CXErrMsg),
    Complete(CXErr)
}

pub type CXResult<T> = Result<T, CXErr>;
pub type CXRawResult<T> = Result<T, CXErrMsg>;
pub type CXMaybeRawResult<T> = Result<T, CXMaybeRawErr>;

pub mod context;

pub struct CXErr {
    error: CXErrMsg,
    context: Box<dyn CXErrorContext>,
}

impl CXErr {
    pub fn new(error: CXErrMsg, context: Box<dyn CXErrorContext>) -> Self {
        CXErr { error, context }
    }

    pub fn output<F>(&self, mut f: F) -> io::Result<()> 
        where F: Write
    {
        write_unspanned(&mut f, &self.error.0)?;
        self.context.output(&mut f)
    }
}