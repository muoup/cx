use cx_tokens::TokenRange;

use crate::error::{
    CXErrorContext,
    context::{CXSourceSpan, from_token_range},
};

pub struct CXNote {
    message: String,
    context: Option<CXErrorContext>,
}

impl CXNote {
    pub fn new(message: impl Into<String>, span: Option<&TokenRange>) -> Self {
        Self {
            message: message.into(),
            context: span.map(from_token_range),
        }
    }

    pub fn message(&self) -> &str {
        &self.message
    }

    pub fn source_span(&self) -> Option<CXSourceSpan> {
        self.context.as_ref()?.source_span()
    }

    pub(crate) fn dump(&self, f: &mut dyn std::io::Write) -> std::io::Result<()> {
        writeln!(f, "note: {}", self.message)?;
        match &self.context {
            Some(context) => context.dump(f),
            None => Ok(()),
        }
    }
}
