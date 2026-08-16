use crate::error::context::{CXErrorContext, CXSourceSpan};
use crate::error::message::CXErrorMessage;

pub struct CXErrMsg(pub Box<dyn CXErrorMessage>);
pub type CXErrContext = Box<dyn CXErrorContext>;

pub enum CXMaybeRawErr {
    Raw(CXErrMsg),
    Complete(CXErr),
}

impl From<CXErrMsg> for CXMaybeRawErr {
    fn from(value: CXErrMsg) -> Self {
        Self::Raw(value)
    }
}

impl From<CXErr> for CXMaybeRawErr {
    fn from(value: CXErr) -> Self {
        Self::Complete(value)
    }
}

pub type CXResult<T> = Result<T, CXErr>;
pub type CXRawResult<T> = Result<T, CXErrMsg>;
pub type CXMaybeRawResult<T> = Result<T, CXMaybeRawErr>;

pub mod context;
pub mod message;

pub struct CXErr {
    error: CXErrMsg,
    context: CXErrContext,
}

impl CXErr {
    pub fn new(error: CXErrMsg, context: CXErrContext) -> Self {
        CXErr { error, context }
    }

    pub fn message(&self) -> String {
        self.error.message()
    }

    pub fn code(&self) -> String {
        self.error.code()
    }

    pub fn source_span(&self) -> Option<CXSourceSpan> {
        self.context.source_span()
    }

    pub fn output<F>(&self, f: &mut F) -> std::io::Result<()>
    where
        F: std::io::Write,
    {
        self.error.0.dump(f)?;
        writeln!(f)?;
        self.context.dump(f)?;

        Ok(())
    }

    pub fn print(&self) -> std::io::Result<()> {
        self.output(&mut std::io::stdout())
    }
}
