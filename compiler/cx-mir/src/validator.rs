mod error;
mod format;
mod structure;
mod types;

pub use error::MIRValidationError;
pub use format::MIRValidationErrorDisplay;

/// Validate all structural and type invariants of an MIR unit.
pub fn validate(unit: &crate::unit::MIRUnit) -> Result<(), MIRValidationError> {
    unit.validate_internal()
}
