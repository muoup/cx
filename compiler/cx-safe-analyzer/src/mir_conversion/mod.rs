use cx_mir::mir::program::MIRFunction;
use cx_safe_ir::ast::FMIRFunction;

use crate::mir_conversion::environment::FMIREnvironment;

mod expression;

pub(crate) mod environment;

pub fn convert_mir(mir_fn: &MIRFunction) -> FMIRFunction {
    let mut env = FMIREnvironment::new();
    let fmir_body = expression::convert_expression(&mut env, &mir_fn.body);
    
    println!("Converted FMIR Body:");
    println!("{}", fmir_body);
    
    FMIRFunction {
        prototype: mir_fn.prototype.clone(),
        body: fmir_body,
    }
}