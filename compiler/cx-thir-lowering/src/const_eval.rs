use std::fmt::{self, Display, Formatter};

use cx_mir::{MIRDiagnostic, MIRDiagnosticLocation};

#[derive(Debug, Clone, PartialEq, Eq)]
pub struct MIRConstEvalError {
    diagnostic: MIRDiagnostic,
    trace: Vec<MIRDiagnosticLocation>,
}

impl MIRConstEvalError {
    pub fn new(message: impl Into<String>, location: MIRDiagnosticLocation) -> Self {
        Self {
            diagnostic: MIRDiagnostic::new("CONST EVAL ERROR", message, location),
            trace: Vec::new(),
        }
    }

    pub fn diagnostic(&self) -> MIRDiagnostic {
        let mut diagnostic = self.diagnostic.clone();
        for (index, location) in self.trace.iter().enumerate() {
            diagnostic.add_note(format!("evaluation frame {index}: {location:?}"));
        }
        diagnostic
    }

    pub fn trace(&self) -> &[MIRDiagnosticLocation] {
        &self.trace
    }

    pub fn push_frame(&mut self, location: MIRDiagnosticLocation) {
        self.trace.push(location);
    }

    pub fn with_frame(mut self, location: MIRDiagnosticLocation) -> Self {
        self.push_frame(location);
        self
    }
}

impl Display for MIRConstEvalError {
    fn fmt(&self, f: &mut Formatter<'_>) -> fmt::Result {
        Display::fmt(self.diagnostic.message(), f)
    }
}

impl std::error::Error for MIRConstEvalError {}
