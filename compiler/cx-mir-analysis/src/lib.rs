//! Deterministic analyses over semantic MIR.
//!
//! Liveness v0 deliberately tracks [`cx_mir::MIRPlace`] values only. MIR
//! registers are SSA-like temporaries and are outside the scope of this first
//! analysis.

mod assertions;
mod format;
mod liveness;
mod ownership;
mod types;

pub use types::{
    MIRAnalysis, MIRAnalysisError, MIRAnalysisOptions, MIRBlockLiveness, MIRFunctionAnalysis,
    MIRInstructionLiveness,
};

use cx_mir::{MIRUnit, validate};

/// Optionally validates `unit`, then computes backward place liveness.
pub fn analyze(
    unit: &MIRUnit,
    options: MIRAnalysisOptions,
) -> Result<MIRAnalysis, MIRAnalysisError> {
    if options.validate {
        validate(unit)?;
    }

    let functions = unit
        .functions
        .iter()
        .map(|function| (function.id, liveness::analyze_function(function)))
        .collect();

    ownership::check(unit)?;

    if options.check_assertions {
        assertions::check(unit)?;
    }

    Ok(MIRAnalysis { functions })
}
