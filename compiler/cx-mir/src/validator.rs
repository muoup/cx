pub(crate) mod error;
mod structure;
mod types;

pub use crate::format::MIRValidationErrorDisplay;
pub use error::MIRValidationError;

/// Validate all structural and type invariants of an MIR unit.
pub fn validate(unit: &crate::unit::MIRUnit) -> Result<(), MIRValidationError> {
    unit.validate_internal()
}
