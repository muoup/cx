use std::{
    collections::{BTreeMap, BTreeSet},
    error::Error,
    fmt::{self, Display, Formatter},
};

use cx_mir::{MIRBasicBlockID, MIRFunctionID, MIRPlace, MIRValidationError};

/// Controls optional checks performed before the independent dataflow analysis.
#[derive(Debug, Clone, Copy, PartialEq, Eq)]
pub struct MIRAnalysisOptions {
    /// Validate all MIR structural invariants before computing liveness.
    pub validate: bool,
    /// Reject assertions that can be proven false from MIR constants.
    pub check_assertions: bool,
}

impl Default for MIRAnalysisOptions {
    fn default() -> Self {
        Self {
            validate: true,
            check_assertions: true,
        }
    }
}

/// Place liveness for every function in a MIR unit.
#[derive(Debug, Clone, Default, PartialEq, Eq)]
pub struct MIRAnalysis {
    pub functions: BTreeMap<MIRFunctionID, MIRFunctionAnalysis>,
}

impl MIRAnalysis {
    pub fn function(&self, id: MIRFunctionID) -> Option<&MIRFunctionAnalysis> {
        self.functions.get(&id)
    }
}

/// Place liveness for each basic block in one function.
#[derive(Debug, Clone, Default, PartialEq, Eq)]
pub struct MIRFunctionAnalysis {
    pub blocks: BTreeMap<MIRBasicBlockID, MIRBlockLiveness>,
}

impl MIRFunctionAnalysis {
    pub fn block(&self, id: MIRBasicBlockID) -> Option<&MIRBlockLiveness> {
        self.blocks.get(&id)
    }
}

/// Fixed-point block boundaries and the corresponding instruction boundaries.
#[derive(Debug, Clone, Default, PartialEq, Eq)]
pub struct MIRBlockLiveness {
    pub live_in: BTreeSet<MIRPlace>,
    pub live_out: BTreeSet<MIRPlace>,
    pub instructions: Vec<MIRInstructionLiveness>,
}

/// Place liveness immediately before and after one MIR instruction.
#[derive(Debug, Clone, Default, PartialEq, Eq)]
pub struct MIRInstructionLiveness {
    pub live_before: BTreeSet<MIRPlace>,
    pub live_after: BTreeSet<MIRPlace>,
}

/// Failures that can prevent MIR analysis.
#[derive(Debug, Clone, PartialEq, Eq)]
pub enum MIRAnalysisError {
    Validation(MIRValidationError),
    ProvenFalseAssertion {
        function: MIRFunctionID,
        block: MIRBasicBlockID,
        instruction: usize,
        message: Option<String>,
    },
}

impl Display for MIRAnalysisError {
    fn fmt(&self, f: &mut Formatter<'_>) -> fmt::Result {
        match self {
            Self::Validation(error) => write!(f, "MIR validation failed: {error}"),
            Self::ProvenFalseAssertion {
                function,
                block,
                instruction,
                message,
            } => {
                write!(
                    f,
                    "MIR assertion in function {function}, block {block}, instruction {instruction} is provably false"
                )?;
                if let Some(message) = message {
                    write!(f, ": {message}")?;
                }
                Ok(())
            }
        }
    }
}

impl Error for MIRAnalysisError {
    fn source(&self) -> Option<&(dyn Error + 'static)> {
        match self {
            Self::Validation(error) => Some(error),
            Self::ProvenFalseAssertion { .. } => None,
        }
    }
}

impl From<MIRValidationError> for MIRAnalysisError {
    fn from(value: MIRValidationError) -> Self {
        Self::Validation(value)
    }
}

impl MIRAnalysisError {
    pub fn instruction_location(&self) -> Option<(MIRFunctionID, MIRBasicBlockID, usize)> {
        match self {
            Self::Validation(error) => error.instruction_location(),
            Self::ProvenFalseAssertion {
                function,
                block,
                instruction,
                ..
            } => Some((*function, *block, *instruction)),
        }
    }
}
