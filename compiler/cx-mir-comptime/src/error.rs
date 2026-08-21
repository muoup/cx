use std::fmt::{self, Display, Formatter};

use cx_mir::{MIRDiagnostic, MIRDiagnosticLocation};

#[derive(Debug, Clone, PartialEq, Eq)]
pub struct MIRComptimeError {
    diagnostic: MIRDiagnostic,
    trace: Vec<MIRDiagnosticLocation>,
}

impl MIRComptimeError {
    pub(crate) fn new(message: impl Into<String>, location: MIRDiagnosticLocation) -> Self {
        Self {
            diagnostic: MIRDiagnostic::new("COMPTIME ERROR", message, location),
            trace: Vec::new(),
        }
    }

    pub fn diagnostic(&self) -> MIRDiagnostic {
        let mut diagnostic = self.diagnostic.clone();
        for (index, location) in self.trace.iter().enumerate() {
            diagnostic.add_note(format!("comptime frame {index}: {location:?}"));
        }
        diagnostic
    }

    pub fn trace(&self) -> &[MIRDiagnosticLocation] {
        &self.trace
    }

    fn with_frame(mut self, location: MIRDiagnosticLocation) -> Self {
        self.trace.push(location);
        self
    }
}

impl Display for MIRComptimeError {
    fn fmt(&self, f: &mut Formatter<'_>) -> fmt::Result {
        Display::fmt(self.diagnostic.message(), f)
    }
}

impl std::error::Error for MIRComptimeError {}

pub(crate) fn with_frame(
    error: MIRComptimeError,
    location: MIRDiagnosticLocation,
) -> MIRComptimeError {
    error.with_frame(location)
}
