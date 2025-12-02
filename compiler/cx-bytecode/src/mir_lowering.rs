use cx_typechecker_data::mir::program::{MIRFunction, MIRUnit};
use cx_util::CXResult;

use crate::{builder::BCBuilder, mir_lowering::instructions::lower_instruction};

pub mod instructions;
pub mod tagged_union;
pub mod types;

pub fn lower_mir(builder: &mut BCBuilder, mir: &MIRUnit) -> CXResult<()> {
    // for global_var in ast.global_variables.iter() {
    //     generate_global_variable(&mut builder, global_var);
    // }

    for function in mir.functions.iter() {
        lower_function(builder, function)?;
    }

    Ok(())
}

pub fn lower_function(builder: &mut BCBuilder, function: &MIRFunction) -> CXResult<()> {
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
