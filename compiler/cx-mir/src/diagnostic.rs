use cx_log::catalogue::ErrorDefinition;
use cx_tokens::TokenRange;

use crate::{MIRBasicBlockID, MIRFunctionID, MIRScopeID};

#[derive(Debug, Clone, PartialEq, Eq)]
pub enum MIRDiagnosticLocation {
    Instruction {
        function: MIRFunctionID,
        block: MIRBasicBlockID,
        instruction: usize,
    },
    Scope {
        function: MIRFunctionID,
        scope: MIRScopeID,
    },
    TokenRange(TokenRange),
    Internal(String),
}

#[derive(Debug, Clone, PartialEq, Eq)]
pub struct MIRDiagnostic {
    code: String,
    message: String,
    location: MIRDiagnosticLocation,
    notes: Vec<String>,
}

impl MIRDiagnostic {
    pub fn new<T>(
        definition: &ErrorDefinition<T>,
        args: T,
        location: MIRDiagnosticLocation,
    ) -> Self {
        Self {
            code: definition.code.to_owned(),
            message: (definition.message)(args),
            location,
            notes: Vec::new(),
        }
    }

    pub fn code(&self) -> &str {
        &self.code
    }

    pub fn message(&self) -> &str {
        &self.message
    }

    pub fn location(&self) -> &MIRDiagnosticLocation {
        &self.location
    }

    pub fn notes(&self) -> &[String] {
        &self.notes
    }

    pub fn with_note(mut self, note: impl Into<String>) -> Self {
        self.notes.push(note.into());
        self
    }

    pub fn add_note(&mut self, note: impl Into<String>) {
        self.notes.push(note.into());
    }
}
