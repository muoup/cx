use cx_mir::mir::{
    expression::{MIRCoercion, MIRExpression, MIRExpressionKind},
    r#type::{MIRType, MIRTypeKind},
};
use cx_util::CXResult;

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
    expr: MIRExpression,
    target_type: &MIRType,
) -> CXResult<CoercionResult> {
    try_implicit_coercion(env, expr, target_type)?.or_else(|expr| {
        let from_type = expr.get_type();
        let coerced = |conversion: MIRCoercion| {
            let coerced = MIRExpression {
                token_range: expr.token_range.clone(),
                _type: target_type.clone(),
                kind: MIRExpressionKind::TypeConversion {
                    operand: Box::new(expr.clone()),
                    conversion,
                },
            };

            CoercionResult::success(coerced)
        };

        match (&from_type.kind, &target_type.kind) {
            (MIRTypeKind::PointerTo { .. }, MIRTypeKind::PointerTo { .. }) => {
                coerced(MIRCoercion::ReinterpretBits)
            }

            (MIRTypeKind::PointerTo { .. }, MIRTypeKind::MemoryReference { .. })
                if env.symbols.is_c_str(&from_type)
                    && env.symbols.is_cx_str(target_type) =>
            {
                coerced(MIRCoercion::ReinterpretBits)
            }

            (MIRTypeKind::PointerTo { .. }, MIRTypeKind::Integer { _type, .. }) => {
                coerced(MIRCoercion::PtrToInt { to_type: *_type })
            }

            (MIRTypeKind::Integer { signed, .. }, MIRTypeKind::PointerTo { .. }) => {
                coerced(MIRCoercion::IntToPtr { sextend: *signed })
            }

            _ => CoercionResult::unapplied(expr),
        }
    })
}
