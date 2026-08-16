use cx_log::CXResult;
use cx_thir::thir::{
    expression::{THIRCoercion, THIRExpression, THIRExpressionKind},
    r#type::{THIRType, THIRTypeKind},
};

use crate::{
    environment::TypeEnvironment,
    type_checking::coercion::{CoercionResult, implicit::coercion_expr},
};

pub fn try_promotion(env: &mut TypeEnvironment, expr: THIRExpression) -> CXResult<CoercionResult> {
    let THIRTypeKind::Integer {
        _type: self_int, ..
    } = expr._type.kind
    else {
        return CoercionResult::unapplied(expr);
    };

    let integer_type = env.get_intrinsic_type("int");

    let THIRTypeKind::Integer { _type: int, .. } = &integer_type.kind else {
        unreachable!("int type should be an integer");
    };

    let integer_rank = int.rank();
    let self_rank = self_int.rank();

    if self_rank >= integer_rank {
        return CoercionResult::unapplied(expr);
    }

    try_conversion(env, expr, &integer_type)
}

pub fn try_conversion(
    _env: &mut TypeEnvironment,
    expr: THIRExpression,
    to_type: &THIRType,
) -> CXResult<CoercionResult> {
    let THIRTypeKind::Integer {
        _type: from_int,
        signed: from_signed,
    } = expr._type.kind
    else {
        return CoercionResult::unapplied(expr);
    };
    let THIRTypeKind::Integer {
        _type: to_int,
        signed: to_signed,
    } = to_type.kind
    else {
        return CoercionResult::unapplied(expr);
    };

    if from_int == to_int {
        if from_signed == to_signed {
            let coerced = THIRExpression {
                token_range: expr.token_range.clone(),
                _type: to_type.clone(),
                kind: THIRExpressionKind::Typechange(Box::new(expr)),
            };

            return CoercionResult::success(coerced);
        }

        return coercion_expr(expr, to_type.clone(), THIRCoercion::ReinterpretBits);
    }

    coercion_expr(
        expr,
        to_type.clone(),
        THIRCoercion::Integral {
            from_type: from_int,
            to_type: to_int,
            sextend: from_signed,
        },
    )
}
