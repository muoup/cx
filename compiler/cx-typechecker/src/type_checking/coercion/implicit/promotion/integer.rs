use cx_log::CXResult;
use cx_thir::thir::{
    expression::{THIRCoercion, THIRExpression},
    r#type::{THIRType, THIRTypeKind},
};

use crate::{
    environment::TypeEnvironment,
    type_checking::coercion::{CoercionResult, implicit::coercion_expr},
};

pub fn try_promotion(env: &mut TypeEnvironment, expr: THIRExpression) -> CXResult<CoercionResult> {
    let THIRTypeKind::Integer { ty: self_int, .. } = expr.ty.kind else {
        return CoercionResult::unapplied(expr);
    };

    let integer_type = env.get_intrinsic_type("int");

    let THIRTypeKind::Integer { ty: int, .. } = &integer_type.kind else {
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
        ty: from_int,
        signed: from_signed,
    } = expr.ty.kind
    else {
        return CoercionResult::unapplied(expr);
    };
    let THIRTypeKind::Integer {
        ty: to_int,
        signed: to_signed,
    } = to_type.kind
    else {
        return CoercionResult::unapplied(expr);
    };

    if from_int == to_int {
        return coercion_expr(
            expr,
            to_type.clone(),
            if from_signed == to_signed {
                THIRCoercion::Typechange
            } else {
                THIRCoercion::Bitcast
            },
        );
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
