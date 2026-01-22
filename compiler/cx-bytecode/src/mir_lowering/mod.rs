//! MIR to Bytecode lowering

use cx_typechecker_data::mir::program::{MIRFunction, MIRGlobalVariable, MIRUnit};
use cx_util::CXResult;

use crate::builder::BCBuilder;

mod binary_ops;
mod coercion;
mod control_flow;
mod tagged_union;

pub mod expressions;
pub mod instructions;
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
    expressions::lower_function(builder, function)
}

pub fn generate_global_value(
    builder: &mut BCBuilder,
    global: &MIRGlobalVariable,
) -> CXResult<()> {
    let lowered = instructions::lower_global_value(builder, global)?;
    builder.add_global_variable(lowered);
    Ok(())
}
