use cx_bytecode_data::compilation_unit::BCUnit;
use cx_mir_data::MIRUnit;
use cx_util::{CXError, CXResult};

use crate::{builder::BytecodeBuilder, mir_lowering::lower_mir_function};

mod builder;
mod mir_lowering;

pub fn lower_mir(mir: &MIRUnit) -> CXResult<BCUnit> {
    let mut builder = BytecodeBuilder::new();

    for function in mir.fn_defs.iter() {
        lower_mir_function(&mut builder, function)?;
    }
    
    CXError::unimplemented("lower_mir is not yet implemented")
}
