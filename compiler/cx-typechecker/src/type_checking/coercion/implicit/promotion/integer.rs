use cx_mir::mir::{
    expression::{MIRCoercion, MIRExpression, MIRExpressionKind},
    r#type::{MIRType, MIRTypeKind},
};

use crate::{environment::TypeEnvironment, type_checking::coercion::{CoercionResult, implicit::coercion_expr}};

pub fn try_promotion(env: &mut TypeEnvironment, expr: MIRExpression) -> CoercionResult {
    let MIRTypeKind::Integer {
        _type: self_int, ..
    } = expr._type.kind
    else {
        return CoercionResult::none(expr);
    };

    let integer_type = env
        .get_realized_type("int")
        .expect("int type should be defined");

    let MIRTypeKind::Integer { _type: int, .. } = integer_type.kind else {
        unreachable!("int type should be an integer");
    };

    let integer_rank = int.rank();
    let self_rank = self_int.rank();

    if self_rank >= integer_rank {
        return CoercionResult::none(expr);
    }

    try_conversion(env, expr, &integer_type)
}

pub fn try_conversion(
    _env: &mut TypeEnvironment,
    expr: MIRExpression,
    to_type: &MIRType,
) -> CoercionResult {
    let MIRTypeKind::Integer {
        _type: from_int,
        signed: from_signed,
    } = expr._type.kind
    else {
        return CoercionResult::none(expr);
    };
    let MIRTypeKind::Integer {
        _type: to_int,
        signed: to_signed,
    } = to_type.kind
    else {
        return CoercionResult::none(expr);
    };

    if from_int == to_int {
        if from_signed == to_signed {
            let coerced = MIRExpression {
                token_range: expr.token_range.clone(),
                _type: to_type.clone(),
                kind: MIRExpressionKind::Typechange(Box::new(expr)),
            };

            return CoercionResult::some(coerced);
        }
        
        return coercion_expr(expr, to_type.clone(), MIRCoercion::ReinterpretBits);
    }

    coercion_expr(
        expr,
        to_type.clone(),
        MIRCoercion::Integral {
            from_type: from_int,
            to_type: to_int,
            sextend: from_signed,
        },
    )
}
