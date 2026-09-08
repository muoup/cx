use cx_log::CXResult;
use cx_log::catalogue::typecheck as catalogue;
use cx_thir::thir::{data::THIRType, expression::THIRExpression};

use crate::{
    environment::TypeEnvironment,
    type_checking::coercion::{CoercionResult, try_explicit_cast},
};

pub(crate) fn explicit_cast(
    env: &mut TypeEnvironment,
    value: THIRExpression,
    to_type: &THIRType,
) -> CXResult<THIRExpression> {
    let from_type = value.get_type();

    match try_explicit_cast(env, value, to_type)? {
        CoercionResult::Success { expr, .. } => Ok(expr),
        CoercionResult::Unapplied { expr, .. } => env.log_error(
            expr.token_range,
            &catalogue::NO_EXPLICIT_CAST_FROM_TO,
            (
                format!("{}", from_type.display_with(&env.symbols)),
                format!("{}", to_type.display_with(&env.symbols)),
            ),
        ),
    }
}
