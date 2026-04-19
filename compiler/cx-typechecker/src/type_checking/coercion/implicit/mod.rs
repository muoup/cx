use cx_mir::mir::{expression::MIRExpression, program::MIRBaseMappings, r#type::MIRType};
use cx_util::{CXError, CXResult};

use crate::{environment::TypeEnvironment, type_checking::coercion::CoercionResult};

pub mod array_to_ptr;
pub mod fn_to_ptr;
pub mod lvalue;

pub fn standard_expr_rval_transform(
    env: &mut TypeEnvironment,
    expr: MIRExpression,
) -> CXResult<MIRExpression> {
    match array_to_ptr::try_conversion(env, expr)
        .or_else(|expr| fn_to_ptr::try_conversion(env, expr))
        .or_else(|expr| lvalue::try_conversion(env, expr))
    {
        // If we successfully transformed the value, we should try to apply the same transformation again
        CoercionResult::Success {
            expr: transformed, ..
        } => standard_expr_rval_transform(env, transformed),

        // If no transformation was applied, we can return the original expression as the final result
        CoercionResult::CastNotFound(expr) => Ok(expr),

        // If an error was encountered during transformation, we should log it and return the original expression as the final result to avoid cascading errors
        CoercionResult::Error { message, .. } => CXError::create_result(message),
    }
}

pub fn try_implicit_cast(
    _env: &mut TypeEnvironment,
    _base_data: &MIRBaseMappings,
    _expr: MIRExpression,
    _target_type: &MIRType,
) -> CoercionResult {
    todo!()
    // lvalue::try_coercion(env, base_data, expr, target_type)
    //     .or_else(|expr| array_to_ptr::try_coercion(env, base_data, expr, target_type))
}
