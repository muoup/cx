use cx_mir::mir::{
    expression::{MIRCoercion, MIRExpression, MIRExpressionKind},
    r#type::{MIRType, MIRTypeKind},
};
use cx_util::CXResult;

use crate::{
    environment::TypeEnvironment,
    type_checking::coercion::{
        CoercionResult,
        implicit::{
            implicit_cast,
            promotion::{integer, lvalue, std_rval_promotion},
        },
    },
};

pub mod compatible;

pub fn try_implicit_coercion(
    env: &mut TypeEnvironment,
    expr: MIRExpression,
    target_type: &MIRType,
) -> CXResult<CoercionResult> {
    let from_type = expr.get_type();

    if env.type_eq(&from_type, target_type) {
        return CoercionResult::success(expr);
    }

    if compatible::compatible_types(env, &expr._type, target_type)? {
        return CoercionResult::success(MIRExpression {
            token_range: expr.token_range.clone(),
            kind: MIRExpressionKind::TypeConversion {
                conversion: MIRCoercion::Typechange,
                operand: Box::new(expr),
            },
            _type: target_type.clone(),
        });
    }

    if expr._type.is_integer() {
        if let MIRTypeKind::Float { _type } = &target_type.kind {
            let MIRTypeKind::Integer { signed, .. } = &expr._type.kind else {
                unreachable!("integer type predicate should match integer kind");
            };
            let signed = *signed;

            return crate::type_checking::coercion::implicit::coercion_expr(
                expr,
                target_type.clone(),
                MIRCoercion::IntToFloat {
                    to_type: *_type,
                    sextend: signed,
                },
            );
        }

        return integer::try_conversion(env, expr, target_type);
    }

    if let (MIRTypeKind::Float { _type: from_float }, MIRTypeKind::Float { _type: to_float }) =
        (&expr._type.kind, &target_type.kind)
        && from_float != to_float
    {
        return crate::type_checking::coercion::implicit::coercion_expr(
            expr,
            target_type.clone(),
            MIRCoercion::FloatCast { to_type: *to_float },
        );
    }

    CoercionResult::unapplied(expr)
}

pub fn try_argument_conversion(
    env: &mut TypeEnvironment,
    expr: MIRExpression,
    argument_type: &MIRType,
) -> CXResult<MIRExpression> {
    let expr = if argument_type.is_memory_reference() {
        let expr_type = expr.get_type();
        if let Some(inner) = env.symbols.context.mem_ref_inner(&expr_type)
            && env.type_eq(inner, argument_type)
        {
            lvalue::try_conversion(env, expr)?.expr()
        } else {
            expr
        }
    } else {
        std_rval_promotion(env, expr)?
    };

    implicit_cast(env, expr, argument_type)
}
