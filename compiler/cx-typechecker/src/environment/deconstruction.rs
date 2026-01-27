use cx_typechecker_data::mir::{program::MIRBaseMappings, types::MIRType};
use cx_util::CXResult;

use crate::environment::TypeEnvironment;

// TODO: Deconstructor generation moved to MIR→LMIR lowering
// These functions are stubbed for now

pub(crate) fn generate_deconstructor(
    _env: &mut TypeEnvironment,
    _base_data: &MIRBaseMappings,
    _type: MIRType,
) -> CXResult<()> {
    // Deconstructor generation moved to lowering pass
    // This is now a no-op to defer deconstructor generation
    Ok(())
}

fn push_deconstructor_request(_env: &mut TypeEnvironment, _type: MIRType) {
    // Deconstructor generation moved to lowering pass
}

pub fn process_new_type(_env: &mut TypeEnvironment, _base_data: &MIRBaseMappings, _type: MIRType) {
    // Deconstructor generation moved to lowering pass
}
