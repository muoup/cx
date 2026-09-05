use std::{
    error::Error,
    fmt::{self, Display, Formatter},
};

use cx_mir::{
    MIRBasicBlockID, MIRDiagnostic, MIRDiagnosticLocation, MIRFunctionID, MIRPlace, MIRScopeID,
};

#[derive(Debug, Clone, Copy, PartialEq, Eq)]
pub struct MIRAnalysisOptions {
    /// Reject assertions that can be proven false from MIR constants.
    pub check_assertions: bool,
}

impl Default for MIRAnalysisOptions {
    fn default() -> Self {
        Self {
            check_assertions: true,
        }
    }
}

/// Failures that can prevent MIR analysis.
#[derive(Debug, Clone, PartialEq, Eq)]
pub enum MIRAnalysisError {
    ProvenFalseAssertion {
        function: MIRFunctionID,
        block: MIRBasicBlockID,
        instruction: usize,
        message: Option<String>,
    },
    OwnershipViolation {
        function: MIRFunctionID,
        block: MIRBasicBlockID,
        instruction: usize,
        scope: Option<MIRScopeID>,
        place: MIRPlace,
        function_name: String,
        message: String,
    },
}

impl Display for MIRAnalysisError {
    fn fmt(&self, f: &mut Formatter<'_>) -> fmt::Result {
        match self {
            Self::ProvenFalseAssertion {
                function, message, ..
            } => {
                write!(f, "Assertion in function {function} is provably false")?;
                if let Some(message) = message {
                    write!(f, ": {message}")?;
                }
                Ok(())
            }
            Self::OwnershipViolation {
                function_name,
                message,
                ..
            } => write!(
                f,
                "Ownership error in function '{function_name}': {message}"
            ),
        }
    }
}

impl Error for MIRAnalysisError {
    fn source(&self) -> Option<&(dyn Error + 'static)> {
        match self {
            Self::ProvenFalseAssertion { .. } | Self::OwnershipViolation { .. } => None,
        }
    }
}

impl MIRAnalysisError {
    pub fn diagnostic(&self) -> MIRDiagnostic {
        match self {
            Self::ProvenFalseAssertion {
                function,
                block,
                instruction,
                ..
            } => MIRDiagnostic::new(
                "ANALYSIS ERROR",
                self.to_string(),
                MIRDiagnosticLocation::Instruction {
                    function: *function,
                    block: *block,
                    instruction: *instruction,
                },
            ),
            Self::OwnershipViolation {
                function,
                block,
                instruction,
                scope,
                ..
            } => {
                let location = scope
                    .map(|scope| MIRDiagnosticLocation::Scope {
                        function: *function,
                        scope,
                    })
                    .unwrap_or(MIRDiagnosticLocation::Instruction {
                        function: *function,
                        block: *block,
                        instruction: *instruction,
                    });
                MIRDiagnostic::new("ANALYSIS ERROR", self.to_string(), location)
            }
        }
    }
}
