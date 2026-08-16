use cx_log::CXResult;
use cx_thir::thir::expression::THIRExpression;

use crate::{environment::TypeEnvironment, type_checking::coercion::CoercionResult};

pub mod array_to_ptr;
pub mod fn_to_ptr;
pub mod integer;
pub mod lvalue;
pub mod str_to_char_ptr;

pub fn std_rval_promotion_coercion(
    env: &mut TypeEnvironment,
    expr: THIRExpression,
) -> CXResult<CoercionResult> {
    array_to_ptr::try_conversion(env, expr)?
        .or_else(|expr| str_to_char_ptr::try_conversion(env, expr))?
        .or_else(|expr| fn_to_ptr::try_conversion(env, expr))?
        .or_else(|expr| integer::try_promotion(env, expr))?
        .or_else(|expr| lvalue::try_conversion(env, expr))
}

pub fn std_rval_promotion(
    env: &mut TypeEnvironment,
    expr: THIRExpression,
) -> CXResult<THIRExpression> {
    match std_rval_promotion_coercion(env, expr)? {
        // If we successfully transformed the value, we should try to apply the same transformation again
        CoercionResult::Success {
            expr: transformed, ..
        } => std_rval_promotion(env, transformed),

        // If no transformation was applied, we can return the original expression as the final result
        CoercionResult::Unapplied { expr, .. } => Ok(expr),
    }
}
