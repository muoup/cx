use cx_mir::mir::{data::MIRType, expression::MIRExpression};
use cx_util::CXResult;

use crate::{
    environment::TypeEnvironment,
    log_typecheck_error,
    type_checking::coercion::{
        CoercionResult, implicit::conversion::try_implicit_coercion, try_explicit_cast,
    },
};

pub(crate) fn explicit_cast(
    env: &mut TypeEnvironment,
    value: MIRExpression,
    to_type: &MIRType,
) -> CXResult<MIRExpression> {
    let from_type = value.get_type();

    match try_explicit_cast(env, value, to_type)? {
        CoercionResult::Success { expr, .. } => Ok(expr),
        CoercionResult::Error { message, expr } => {
            log_typecheck_error!(env, expr.token_range.as_ref(), "{message}")
        }
        CoercionResult::IncompleteCast(expr) => log_typecheck_error!(
            env,
            expr.token_range.as_ref(),
            "No explicit cast from {} to {}",
            from_type,
            to_type
        ),
    }
}

pub fn implicit_cast(
    env: &mut TypeEnvironment,
    value: MIRExpression,
    to_type: &MIRType,
) -> CXResult<MIRExpression> {
    let from_type = value.get_type();

    if env.type_eq(&from_type, to_type) {
        return Ok(value);
    }

    match try_implicit_coercion(env, value, to_type)? {
        CoercionResult::Success { expr, .. } => Ok(expr),
        CoercionResult::IncompleteCast(expr) => log_typecheck_error!(
            env,
            expr.token_range.as_ref(),
            "No implicit cast from {} to {}",
            from_type,
            to_type
        ),
        CoercionResult::Error { message, expr } => {
            log_typecheck_error!(env, expr.token_range.as_ref(), "{message}")
        }
    }
}
