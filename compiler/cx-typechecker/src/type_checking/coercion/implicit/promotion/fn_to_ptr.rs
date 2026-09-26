use cx_log::CXResult;
use cx_thir::thir::expression::{THIRExpression, THIRExpressionKind};

use crate::{environment::TypeEnvironment, type_checking::coercion::CoercionResult};

pub fn try_conversion(env: &mut TypeEnvironment, expr: THIRExpression) -> CXResult<CoercionResult> {
    if !expr.ty.is_function() {
        return CoercionResult::unapplied(expr);
    }

    let new_type = env.symbols.pointer_to(expr.ty.clone());

    let coerced = THIRExpression {
        token_range: expr.token_range.clone(),

        ty: new_type,
        kind: THIRExpressionKind::AddressOf {
            operand: Box::new(expr),
        },
    };

    CoercionResult::success(coerced)
}
