use cx_mir::mir::{data::MIRType, expression::MIRExpression};
use cx_util::CXResult;

use crate::{
    environment::TypeEnvironment,
    log_typecheck_error,
    type_checking::coercion::{CoercionResult, try_explicit_cast},
};

pub(crate) fn explicit_cast(
    env: &mut TypeEnvironment,
    value: MIRExpression,
    to_type: &MIRType,
) -> CXResult<MIRExpression> {
    let from_type = value.get_type();

    match try_explicit_cast(env, value, to_type)? {
        CoercionResult::Success { expr, .. } => Ok(expr),
        CoercionResult::Unapplied { expr, .. } => log_typecheck_error!(
            env,
            expr.token_range.as_ref(),
            "No explicit cast from {} to {}",
            from_type.display_with(&env.symbols),
            to_type.display_with(&env.symbols)
        ),
    }
}
