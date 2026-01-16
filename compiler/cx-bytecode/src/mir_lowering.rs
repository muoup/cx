use cx_typechecker_data::mir::program::{MIRFunction, MIRGlobalVariable, MIRUnit};
use cx_util::CXResult;

use crate::{
    builder::BCBuilder,
    mir_lowering::{expressions::lower_function, instructions::lower_global_value},
};

pub(crate) mod binary_ops;
pub(crate) mod coercion;

pub mod expressions;
pub mod instructions;
pub mod tagged_union;
pub mod types;

pub fn lower_mir(builder: &mut BCBuilder, mir: &MIRUnit) -> CXResult<()> {
    for global_var in mir.global_variables.iter() {
        generate_global_value(builder, global_var)?;
    }

    for function in mir.functions.iter() {
        generate_function(builder, function)?;
    }

    Ok(())
}

pub fn generate_function(builder: &mut BCBuilder, function: &MIRFunction) -> CXResult<()> {
    // Use the new expression-based lowering
    lower_function(builder, function)
}

pub fn generate_global_value(
    builder: &mut BCBuilder,
    global: &MIRGlobalVariable,
) -> CXResult<()> {
    let lowered = lower_global_value(builder, global)?;
    
    builder.add_global_variable(lowered);
    
    Ok(())
}
