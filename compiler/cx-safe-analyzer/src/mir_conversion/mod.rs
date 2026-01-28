use cx_mir::mir::program::MIRFunction;
use cx_safe_ir::ast::FMIRFunction;

mod expression;

pub fn convert_mir(mir_fn: &MIRFunction) -> FMIRFunction {
    let fmir_body = expression::convert_expression(&mir_fn.body);
    
    println!("Converted FMIR Body:");
    println!("{}", fmir_body);
    
    FMIRFunction {
        prototype: mir_fn.prototype.clone(),
        body: fmir_body,
    }
}