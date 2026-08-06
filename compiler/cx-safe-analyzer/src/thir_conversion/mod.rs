use cx_log::CXResult;
use cx_thir::thir::data::THIRFunction;
use cx_safe_ir::ast::FMIRFunction;

use crate::thir_conversion::{
    environment::FMIREnvironment, types::validate_safe_function_signature,
};

pub mod factories;
pub mod types;

pub(crate) mod environment;
mod expression;

pub fn convert_thir(env: &mut FMIREnvironment, thir_fn: &THIRFunction) -> CXResult<FMIRFunction> {
    validate_safe_function_signature(env, &thir_fn.prototype, &thir_fn.body)?;

    env.begin_function(thir_fn.prototype.clone());
    let fmir_body = expression::convert_expression(env, &thir_fn.body)?;

    Ok(FMIRFunction {
        prototype: thir_fn.prototype.clone(),
        body: fmir_body,
    })
}
