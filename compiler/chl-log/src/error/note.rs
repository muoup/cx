use chl_tokens::TokenRange;

use crate::error::{
    CHLErrorContext,
    context::{CHLSourceSpan, from_token_range},
};

pub struct CHLNote {
    message: String,
    context: Option<CHLErrorContext>,
}

impl CHLNote {
    pub fn new(message: impl Into<String>, span: Option<&TokenRange>) -> Self {
        Self {
            message: message.into(),
            context: span.map(from_token_range),
        }
    }

    pub fn message(&self) -> &str {
        &self.message
    }

    pub fn source_span(&self) -> Option<CHLSourceSpan> {
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
