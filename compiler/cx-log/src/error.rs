use crate::error::context::CXErrorContext;
use crate::error::message::CXErrorMessage;

pub struct CXErrMsg(pub Box<dyn CXErrorMessage>);
pub type CXErrContext = Box<dyn CXErrorContext>;

pub enum CXMaybeRawErr {
    Raw(CXErrMsg),
    Complete(CXErr)
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

    pub fn output<F>(&self, f: &mut F) -> std::io::Result<()> 
        where F: std::io::Write
    {
        self.error.0.dump(f)?;
        self.context.dump(f)?;

        Ok(())
    }
}