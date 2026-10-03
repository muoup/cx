use crate::catalogue::{ISSUE_TRACKER, is_internal};
use crate::error::context::{CXErrorContextTrait, CXSourceSpan};
use crate::error::message::CXErrorMessage;

pub fn write_bug_report_notes(f: &mut dyn std::io::Write) -> std::io::Result<()> {
    writeln!(f, "note: this is a bug in the cx compiler, not in your code")?;
    writeln!(
        f,
        "note: please report it at {ISSUE_TRACKER}, including the input that triggered it"
    )
}

pub struct CXRawError(pub Box<dyn CXErrorMessage>, pub(crate) Vec<String>);
pub type CXErrorContext = Box<dyn CXErrorContextTrait>;

pub enum CXErrorMaybeRaw {
    Raw(CXRawError),
    Complete(CXError),
}

impl From<CXRawError> for CXErrorMaybeRaw {
    fn from(value: CXRawError) -> Self {
        Self::Raw(value)
    }
}

impl From<CXError> for CXErrorMaybeRaw {
    fn from(value: CXError) -> Self {
        Self::Complete(value)
    }
}

pub type CXResult<T> = Result<T, CXError>;
pub type CXRawResult<T> = Result<T, CXRawError>;
pub type CXMaybeRawResult<T> = Result<T, CXErrorMaybeRaw>;

pub mod context;
pub mod message;

pub struct CXError {
    error: CXRawError,
    context: CXErrorContext,
}

impl CXError {
    pub fn new(error: CXRawError, context: CXErrorContext) -> Self {
        CXError { error, context }
    }

    pub fn message(&self) -> String {
        self.error.message()
    }

    pub fn code(&self) -> String {
        self.error.code()
    }

    pub fn notes(&self) -> &[String] {
        &self.error.1
    }

    pub fn is_internal(&self) -> bool {
        is_internal(&self.code())
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
        for note in self.notes() {
            writeln!(f, "note: {note}")?;
        }
        if self.is_internal() {
            write_bug_report_notes(f)?;
        }

        Ok(())
    }

    pub fn print(&self) -> std::io::Result<()> {
        self.output(&mut std::io::stderr().lock())
    }
}
