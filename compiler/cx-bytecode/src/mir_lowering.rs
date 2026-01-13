use cx_typechecker_data::mir::program::{MIRFunction, MIRGlobalVariable, MIRUnit};
use cx_util::CXResult;
use cx_util::identifier::CXIdent;

use crate::{
    builder::BCBuilder,
    mir_lowering::instructions::lower_global_value,
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

    // TODO: Implement expression tree lowering from MIRFunction.body
    // For now, just add an entry block to keep it compiling
    // The expression tree should be lowered to basic blocks here
    let entry_id = CXIdent::new("entry");
    let _entry_block = builder.create_block(entry_id);
    builder.set_current_block(CXIdent::new("entry"));

    // For now, add a no-op to keep it compiling
    // This will be replaced with actual expression tree lowering
    // when typechecker is updated to produce MIRExpression trees

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
