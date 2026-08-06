use cx_log::CXResult;
use cx_thir::thir::expression::{THIRCoercion, THIRExpression, THIRExpressionKind};

use crate::{environment::TypeEnvironment, type_checking::coercion::CoercionResult};

pub fn try_conversion(env: &mut TypeEnvironment, expr: THIRExpression) -> CXResult<CoercionResult> {
    if !expr._type.is_function() {
        return CoercionResult::unapplied(expr);
    }

    let new_type = env.symbols.pointer_to(expr._type.clone());

    let coerced = THIRExpression {
        token_range: expr.token_range.clone(),

        _type: new_type,
        kind: THIRExpressionKind::TypeConversion {
            operand: Box::new(expr),
            conversion: THIRCoercion::GetFnPtr,
        },
    };

    CoercionResult::success(coerced)
}
