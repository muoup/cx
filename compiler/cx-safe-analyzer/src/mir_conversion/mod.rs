use cx_mir::mir::program::MIRFunction;
use cx_safe_ir::ast::FMIRFunction;
use cx_util::CXResult;

use crate::mir_conversion::environment::FMIREnvironment;

pub mod factories;
pub(crate) mod environment;
mod expression;

pub fn convert_mir(env: &mut FMIREnvironment, mir_fn: &MIRFunction) -> CXResult<FMIRFunction> {
    env.begin_function(mir_fn.prototype.clone());
    let fmir_body = expression::convert_expression(env, &mir_fn.body)?;

    Ok(FMIRFunction {
        prototype: mir_fn.prototype.clone(),
        body: fmir_body,
    })
}
