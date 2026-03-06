use cx_typechecker_data::mir::program::{MIRFunction, MIRGlobalVariable, MIRUnit};
use cx_util::CXResult;

use crate::{
    builder::BCBuilder,
    mir_lowering::instructions::{lower_global_value, lower_instruction},
};

pub(crate) mod binary_ops;
pub(crate) mod coercion;

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
    let bc_prototype = builder.convert_cx_prototype(&function.prototype);
    builder.new_function(bc_prototype);

    for block in function.basic_blocks.iter() {
        builder.create_block(block.id.clone());
        builder.set_current_block(block.id.clone());

        for instruction in block.instructions.iter() {
            lower_instruction(builder, instruction)?;
        }
    }

    builder.finish_function();
    Ok(())
}

pub fn generate_global_value(
    builder: &mut BCBuilder,
    global: &MIRGlobalVariable,
) -> CXResult<()> {
    let lowered = lower_global_value(builder, global)?;
    
    builder.add_global_variable(lowered);
    
    Ok(())
}
