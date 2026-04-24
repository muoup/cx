use cx_mir::mir::{
    expression::{MIRCoercion, MIRExpression, MIRExpressionKind},
    r#type::MIRType,
};
use cx_util::CXResult;

use crate::{
    environment::TypeEnvironment,
    log_typecheck_error,
    type_checking::coercion::{CoercionResult, implicit::conversion::try_implicit_coercion},
};

pub mod conversion;
pub mod promotion;

pub fn coercion_expr(
    expr: MIRExpression,
    target_type: MIRType,
    coercion: MIRCoercion,
) -> CXResult<CoercionResult> {
    let coerced = MIRExpression {
        token_range: expr.token_range.clone(),
        _type: target_type,
        kind: MIRExpressionKind::TypeConversion {
            operand: Box::new(expr),
            conversion: coercion,
        },
    };

    CoercionResult::success(coerced)
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

    try_implicit_coercion(env, value, to_type)?
        .catch_unapplied(|expr, _|
            log_typecheck_error!(
                env,
                expr.token_range.as_ref(),
                "No implicit cast from {} to {}",
                from_type,
                to_type
            )
        )
}
