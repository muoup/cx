use cx_fmir_data::ast::FMIRFunction;
use cx_parsing_data::ast::CXExpr;
use cx_typechecker_data::mir::{program::MIRBaseMappings, types::MIRFunctionPrototype};
use cx_util::CXResult;

mod expression;

use crate::environment::TypeEnvironment;

pub fn lower_safe_fn(
    env: &mut TypeEnvironment,
    base_data: &MIRBaseMappings,
    prototype: MIRFunctionPrototype,
    body: &CXExpr,
) -> CXResult<FMIRFunction> {
    let body_expr = expression::lower_expression(env, base_data, &prototype, body)?;
    let as_function = FMIRFunction {
        prototype,
        body: body_expr,
    };
    
    println!("Lowered safe function:\n{}", as_function);
    todo!()
}
