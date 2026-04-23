use cx_mir::mir::{
    expression::{MIRCoercion, MIRExpression, MIRExpressionKind},
    r#type::MIRTypeKind,
};
use cx_util::CXResult;

use crate::{environment::TypeEnvironment, type_checking::coercion::CoercionResult};

pub fn try_conversion(env: &mut TypeEnvironment, expr: MIRExpression) -> CXResult<CoercionResult> {
    if !matches!(expr._type.kind, MIRTypeKind::Function { .. }) {
        return CoercionResult::unapplied(expr);
    }

    let new_type = env.type_context.pointer_to(expr._type.clone());

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
