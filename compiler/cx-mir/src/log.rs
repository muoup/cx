use cx_log::{
    CXRawResult,
    catalogue::{ErrorDefinition, mir},
    error::CXRawError,
};

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
        MIRLayoutError::InvalidType(id) => mir::ENTITY_REQUIREMENT.bind((
            format!("MIR type {id}"),
            "a valid type".into(),
            None,
        )),
        MIRLayoutError::DuplicateType(id) => mir::DUPLICATE_ENTITY
            .bind((format!("MIR type {id}"), "type registry".into())),
        MIRLayoutError::RecursiveType(id) => mir::RECURSIVE_TYPE.bind(id.to_string()),
        MIRLayoutError::InvalidBitfieldWidth {
            width,
            storage_bits,
        } => mir::INVALID_BITFIELD_WIDTH.bind((width, storage_bits)),
        MIRLayoutError::InvalidAlignment(alignment) => mir::INVALID_LAYOUT
            .bind(("MIR type".into(), "a valid alignment".into(), Some(alignment.to_string()))),
        MIRLayoutError::InvalidField { ty, field } => {
            mir::MISSING_ENTITY.bind((format!("field '{field}'"), format!("MIR type {ty}")))
        }
        MIRLayoutError::SizeOverflow => mir::SIZE_OVERFLOW.bind(()),
    }
}
