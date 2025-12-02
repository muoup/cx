use cx_bytecode_data::BCUnit;
use cx_typechecker_data::mir::program::MIRUnit;
use cx_util::CXResult;

use crate::{builder::BCBuilder, mir_lowering::lower_mir};

pub mod builder;

pub(crate) mod mir_lowering;

pub type BytecodeResult<T> = Option<T>;

pub fn generate_bytecode(mir: &MIRUnit) -> CXResult<BCUnit> {
    let mut builder = BCBuilder::new(&mir);
    
    lower_mir(&mut builder, mir)?;
    
    Ok(builder.finish())
}