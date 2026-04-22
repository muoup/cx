use cx_ast::ast::CXExpr;
use cx_mir::mir::{
    data::{MIRType, same_type},
    expression::MIRExpression,
};
use cx_util::CXResult;

use crate::{
    environment::TypeEnvironment,
    log_typecheck_error,
    type_checking::coercion::{CoercionResult, implicit::{std_rval_promotion, try_implicit_cast}, try_explicit_cast},
};

pub(crate) fn coerce_value(
    env: &mut TypeEnvironment,
    _expr: &CXExpr,
    value: MIRExpression,
) -> CXResult<MIRExpression> {
    let value_type = value.get_type();
    let mem_ref_inner = env.type_context.mem_ref_inner(&value_type).cloned();

    if let Some(mem_ref_inner) = mem_ref_inner {
        if mem_ref_inner.is_str() || !env.is_copyable(&mem_ref_inner) {
            return Ok(value);
        }
    }

    std_rval_promotion(env, value)
}

pub(crate) fn explicit_cast(
    env: &mut TypeEnvironment,
    expr: &CXExpr,
    value: MIRExpression,
    to_type: &MIRType,
) -> CXResult<MIRExpression> {
    let from_type = value.get_type();

    match try_explicit_cast(env, value, to_type) {
        CoercionResult::Success { expr, .. } => Ok(expr),
        CoercionResult::Error { message, .. } => {
            log_typecheck_error!(env, expr.token_range(), "{message}")
        }
        CoercionResult::CastNotFound(_) => log_typecheck_error!(
            env,
            expr.token_range(),
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

    if same_type(&from_type, to_type) {
        return Ok(value);
    }

    match try_implicit_cast(env, value, to_type) {
        CoercionResult::Success { expr, .. } => Ok(expr),
        CoercionResult::CastNotFound(expr) => log_typecheck_error!(
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
