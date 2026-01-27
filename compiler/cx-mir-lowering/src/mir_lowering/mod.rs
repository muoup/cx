//! MIR to LMIR lowering

use cx_mir::mir::program::{MIRFunction, MIRGlobalVariable, MIRUnit};
use cx_util::CXResult;

use crate::builder::LMIRBuilder;

mod binary_ops;
mod coercion;
mod control_flow;
mod tagged_union;

pub mod deconstructors;
pub mod expressions;
pub mod instructions;
pub mod types;

pub fn lower_mir(builder: &mut LMIRBuilder, mir: &MIRUnit) -> CXResult<()> {
    for global_var in mir.global_variables.iter() {
        generate_global_value(builder, global_var)?;
    }

    for function in mir.functions.iter() {
        generate_function(builder, function)?;
    }

    while let Some(deconstructor_type) = builder.pop_deconstructor_request() {
        deconstructors::generate_deconstructor(builder, &deconstructor_type)?;
    }

    Ok(())
}

pub fn generate_function(builder: &mut LMIRBuilder, function: &MIRFunction) -> CXResult<()> {
    expressions::lower_function(builder, function)
}

pub fn generate_global_value(
    builder: &mut LMIRBuilder,
    global: &MIRGlobalVariable,
) -> CXResult<()> {
    let lowered = instructions::lower_global_value(builder, global)?;
    builder.add_global_variable(lowered);
    Ok(())
}
