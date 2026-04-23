use cx_mir::mir::expression::MIRExpression;
use cx_util::CXResult;

use crate::{environment::TypeEnvironment, type_checking::coercion::CoercionResult};

pub mod array_to_ptr;
pub mod fn_to_ptr;
pub mod integer;
pub mod lvalue;

pub fn std_rval_promotion(
    env: &mut TypeEnvironment,
    expr: MIRExpression,
) -> CXResult<MIRExpression> {
    match array_to_ptr::try_conversion(env, expr)?
        .or_else(|expr| fn_to_ptr::try_conversion(env, expr))?
        .or_else(|expr| lvalue::try_conversion(env, expr))?
    {
        // If we successfully transformed the value, we should try to apply the same transformation again
        CoercionResult::Success {
            expr: transformed, ..
        } => std_rval_promotion(env, transformed),

        // If no transformation was applied, we can return the original expression as the final result
        CoercionResult::Unapplied { expr, .. } => Ok(expr),
    }
}
