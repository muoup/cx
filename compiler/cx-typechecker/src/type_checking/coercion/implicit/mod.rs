use cx_mir::mir::{expression::{MIRCoercion, MIRExpression, MIRExpressionKind}, r#type::MIRType};

use crate::type_checking::coercion::CoercionResult;

pub mod conversion;
pub mod promotion;

pub fn coercion_expr(expr: MIRExpression, target_type: MIRType, coercion: MIRCoercion) -> CoercionResult {
    let coerced = MIRExpression {
        token_range: expr.token_range.clone(),
        _type: target_type,
        kind: MIRExpressionKind::TypeConversion {
            operand: Box::new(expr),
            conversion: coercion,
        },
    };

    CoercionResult::some(coerced)
}