use cx_parsing_data::data::CXFunctionContract;
use cx_typechecker_data::mir::{
    name_mangling::base_mangle_deconstructor,
    program::MIRBaseMappings,
    types::{MIRFunctionPrototype, MIRParameter, MIRType, MIRTypeKind},
};
use cx_util::{CXResult, identifier::CXIdent};

use crate::{environment::{MIRFunctionGenRequest, TypeEnvironment}, type_checking::binary_ops::struct_field};

// TODO: Deconstructor generation moved to MIR→LMIR lowering
// These functions are stubbed for now

pub(crate) fn generate_deconstructor(_env: &mut TypeEnvironment, _base_data: &MIRBaseMappings, _type: MIRType) -> CXResult<()> {
    // Deconstructor generation moved to lowering pass
    todo!("Deconstructor generation moved to MIR→LMIR lowering")
}

fn push_deconstructor_request(_env: &mut TypeEnvironment, _type: MIRType) {
    // Deconstructor generation moved to lowering pass
}

pub fn process_new_type(
    _env: &mut TypeEnvironment,
    _base_data: &MIRBaseMappings,
    _type: MIRType,
) {
    // Deconstructor generation moved to lowering pass
}
