mod error;
mod instruction;
mod structure;
mod targets;
mod types;

pub use error::MIRValidationError;

/// Validate all structural and type invariants of an MIR unit.
pub fn validate(unit: &crate::unit::MIRUnit) -> Result<(), MIRValidationError> {
    unit.validate_internal()
}
