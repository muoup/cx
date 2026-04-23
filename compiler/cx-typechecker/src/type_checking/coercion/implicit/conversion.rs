use cx_mir::mir::{
    expression::{MIRCoercion, MIRExpression, MIRExpressionKind},
    r#type::MIRType,
};
use cx_util::CXResult;

use crate::{environment::TypeEnvironment, type_checking::coercion::CoercionResult};

pub mod compatible;

pub fn try_implicit_coercion(
    env: &mut TypeEnvironment,
    expr: MIRExpression,
    target_type: &MIRType,
) -> CXResult<CoercionResult> {
    let from_type = expr.get_type();

    if env.type_eq(&from_type, target_type) {
        return Ok(CoercionResult::none(expr));
    }

    if compatible::compatible_types(env, &expr._type, target_type)? {
        return Ok(CoercionResult::some(MIRExpression {
            token_range: expr.token_range.clone(),
            kind: MIRExpressionKind::TypeConversion {
                conversion: MIRCoercion::Typechange,
                operand: Box::new(expr),
            },
            _type: target_type.clone(),
        }));
    }

    todo!()
}
