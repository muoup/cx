use cx_log::CXResult;
use cx_mir::mir::{
    expression::{MIRCoercion, MIRExpression, MIRExpressionKind}
};

use crate::{environment::TypeEnvironment, type_checking::coercion::CoercionResult};

pub fn try_conversion(env: &mut TypeEnvironment, expr: MIRExpression) -> CXResult<CoercionResult> {
    if !expr._type.is_function() {
        return CoercionResult::unapplied(expr);
    }

    let new_type = env.symbols.pointer_to(expr._type.clone());

    let coerced = MIRExpression {
        token_range: expr.token_range.clone(),

        _type: new_type,
        kind: MIRExpressionKind::TypeConversion {
            operand: Box::new(expr),
            conversion: MIRCoercion::GetFnPtr,
        },
    };

    CoercionResult::success(coerced)
}
