use cx_log::{
    CXRawResult,
    catalogue::{ErrorDefinition, mir},
    error::CXRawError,
};

use crate::MIRLayoutError;

#[allow(dead_code)]
pub fn log_raw_error<T, U>(definition: &ErrorDefinition<T>, args: T) -> CXRawResult<U> {
    Err(definition.bind(args))
}

#[allow(dead_code)]
pub fn raw_error<T>(definition: &ErrorDefinition<T>, args: T) -> CXRawError {
    definition.bind(args)
}

pub fn layout_error(error: MIRLayoutError) -> CXRawError {
    match error {
        MIRLayoutError::InvalidType(id) => mir::MIR_INVALID_TYPE.bind(id.to_string()),
        MIRLayoutError::DuplicateType(id) => mir::MIR_DUPLICATE_TYPE.bind(id.to_string()),
        MIRLayoutError::RecursiveType(id) => mir::MIR_RECURSIVE_TYPE.bind(id.to_string()),
        MIRLayoutError::InvalidBitfieldWidth {
            width,
            storage_bits,
        } => mir::MIR_INVALID_BITFIELD_WIDTH.bind((width, storage_bits)),
        MIRLayoutError::InvalidAlignment(alignment) => mir::MIR_INVALID_ALIGNMENT.bind(alignment),
        MIRLayoutError::InvalidField { ty, field } => {
            mir::MIR_INVALID_FIELD.bind((ty.to_string(), field))
        }
        MIRLayoutError::SizeOverflow => mir::MIR_SIZE_OVERFLOW.bind(()),
    }
}
