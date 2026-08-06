use cx_log::CXResult;
use cx_thir::{
    thir::{
        expression::{THIRCoercion, THIRExpression, THIRExpressionKind},
        r#type::{THIRType, THIRTypeKind},
    },
    type_context::THIRTypeContext,
};

use crate::{
    environment::TypeEnvironment,
    type_checking::coercion::implicit::conversion::try_implicit_coercion,
};

pub mod explicit;
pub mod implicit;
pub mod result;

pub use result::{CoercionObstacle, CoercionResult};

pub fn try_explicit_cast(
    env: &mut TypeEnvironment,
    expr: THIRExpression,
    target_type: &THIRType,
) -> CXResult<CoercionResult> {
    try_implicit_coercion(env, expr, target_type)?.or_else(|expr| {
        let from_type = expr.get_type();
        let coerced = |conversion: THIRCoercion| {
            let coerced = THIRExpression {
                token_range: expr.token_range.clone(),
                _type: target_type.clone(),
                kind: THIRExpressionKind::TypeConversion {
                    operand: Box::new(expr.clone()),
                    conversion,
                },
            };

            CoercionResult::success(coerced)
        };

        match (&from_type.kind, &target_type.kind) {
            (THIRTypeKind::PointerTo { .. }, THIRTypeKind::PointerTo { .. }) => {
                coerced(THIRCoercion::ReinterpretBits)
            }

            (THIRTypeKind::PointerTo { .. }, THIRTypeKind::MemoryReference { .. })
                if env.symbols.is_c_str(&from_type) && env.symbols.is_cx_str(target_type) =>
            {
                coerced(THIRCoercion::ReinterpretBits)
            }

            (THIRTypeKind::PointerTo { .. }, THIRTypeKind::Integer { _type, .. }) => {
                coerced(THIRCoercion::PtrToInt { to_type: *_type })
            }

            (THIRTypeKind::Integer { signed, .. }, THIRTypeKind::PointerTo { .. }) => {
                coerced(THIRCoercion::IntToPtr { sextend: *signed })
            }

            _ => CoercionResult::unapplied(expr),
        }
    })
}
