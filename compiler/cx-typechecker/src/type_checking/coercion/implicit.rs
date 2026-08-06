use cx_log::CXResult;
use cx_thir::thir::{
    expression::{THIRCoercion, THIRExpression, THIRExpressionKind},
    r#type::THIRType,
};

use crate::{
    environment::TypeEnvironment,
    type_checking::coercion::{CoercionResult, implicit::conversion::try_implicit_coercion},
};

pub mod conversion;
pub mod promotion;

pub fn coercion_expr(
    expr: THIRExpression,
    target_type: THIRType,
    coercion: THIRCoercion,
) -> CXResult<CoercionResult> {
    let coerced = THIRExpression {
        token_range: expr.token_range.clone(),
        _type: target_type,
        kind: THIRExpressionKind::TypeConversion {
            operand: Box::new(expr),
            conversion: coercion,
        },
    };

    CoercionResult::success(coerced)
}

pub fn implicit_cast(
    env: &mut TypeEnvironment,
    value: THIRExpression,
    to_type: &THIRType,
) -> CXResult<THIRExpression> {
    let from_type = value.get_type();

    try_implicit_coercion(env, value, to_type)?.catch_unapplied(|expr, _| {
        env.log_error(
            expr.token_range,
            format!(
                "No implicit cast from {} to {}",
                from_type.display_with(&env.symbols),
                to_type.display_with(&env.symbols),
            ),
        )
    })
}
