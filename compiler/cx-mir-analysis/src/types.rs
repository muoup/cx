use std::{
    error::Error,
    fmt::{self, Display, Formatter},
};

use cx_mir::{MIRBasicBlockID, MIRDiagnostic, MIRDiagnosticLocation, MIRFunctionID, MIRPlace};

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
        place: MIRPlace,
        diagnostic: MIRDiagnostic,
    },
}

impl Display for MIRAnalysisError {
    fn fmt(&self, f: &mut Formatter<'_>) -> fmt::Result {
        f.write_str(self.diagnostic().message())
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
                message,
            } => crate::log::error(
                &cx_log::catalogue::analysis::PROVEN_FALSE_ASSERTION,
                (function.to_string(), message.clone()),
                MIRDiagnosticLocation::Instruction {
                    function: *function,
                    block: *block,
                    instruction: *instruction,
                },
            ),
            Self::OwnershipViolation { diagnostic, .. } => diagnostic.clone(),
        }
    }
}
