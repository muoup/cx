use cx_mir::mir::program::MIRFunction;
use cx_safe_ir::ast::FMIRFunction;

use crate::mir_conversion::environment::FMIREnvironment;

mod expression;

pub(crate) mod environment;

pub fn convert_mir(env: &mut FMIREnvironment, mir_fn: &MIRFunction) -> FMIRFunction {
    env.set_current_mir_prototype(mir_fn.prototype.clone());
    let fmir_body = expression::convert_expression(env, &mir_fn.body);
    
    println!("Converted FMIR Body:");
    println!("{}", fmir_body);
    
    FMIRFunction {
        prototype: mir_fn.prototype.clone(),
        body: fmir_body,
    }
}