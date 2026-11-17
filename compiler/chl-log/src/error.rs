use std::any::Any;

use crate::catalogue::{ISSUE_TRACKER, is_internal};
use crate::error::context::{CHLErrorContextTrait, CHLSourceSpan};
use crate::error::message::CHLErrorMessage;
use crate::error::note::CHLNote;
use chl_tokens::TokenRange;

pub fn write_bug_report_notes(f: &mut dyn std::io::Write) -> std::io::Result<()> {
    writeln!(f, "note: this is a bug in the chl compiler, not in your code")?;
    writeln!(
        f,
        "note: please report it at {ISSUE_TRACKER}, including the input that triggered it"
    )
}

pub struct CHLRawError(pub Box<dyn CHLErrorMessage>);
pub type CHLErrorContext = Box<dyn CHLErrorContextTrait>;

pub enum CHLErrorMaybeRaw {
    Raw(CHLRawError),
    Complete(CHLError),
}

impl From<CHLRawError> for CHLErrorMaybeRaw {
    fn from(value: CHLRawError) -> Self {
        Self::Raw(value)
    }
}

impl From<CHLError> for CHLErrorMaybeRaw {
    fn from(value: CHLError) -> Self {
        Self::Complete(value)
    }
}

pub type CHLResult<T> = Result<T, CHLError>;
pub type CHLRawResult<T> = Result<T, CHLRawError>;
pub type CHLMaybeRawResult<T> = Result<T, CHLErrorMaybeRaw>;

pub mod context;
pub mod message;
pub mod note;

pub struct CHLError {
    error: CHLRawError,
    context: CHLErrorContext,
    notes: Vec<CHLNote>,
}

impl CHLError {
    pub fn new(error: CHLRawError, context: CHLErrorContext) -> Self {
        CHLError {
            error,
            context,
            notes: Vec::new(),
        }
    }

    pub fn message(&self) -> String {
        self.error.message()
    }

    pub fn message_as<M: CHLErrorMessage>(&self) -> Option<&M> {
        let message: &dyn Any = self.error.0.as_ref();
        message.downcast_ref()
    }

    pub fn code(&self) -> String {
        self.error.code()
    }

    pub fn with_note(mut self, message: impl Into<String>, span: Option<&TokenRange>) -> Self {
        self.notes.push(CHLNote::new(message, span));
        self
    }

    pub fn notes(&self) -> &[CHLNote] {
        &self.notes
    }

    pub fn is_internal(&self) -> bool {
        is_internal(&self.code())
    }

    pub fn source_span(&self) -> Option<CHLSourceSpan> {
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
            note.dump(f)?;
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
