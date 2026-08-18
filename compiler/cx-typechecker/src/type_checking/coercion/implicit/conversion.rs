use cx_log::CXResult;
use cx_thir::{
    thir::{
        expression::{
            THIRBinOp, THIRCoercion, THIRExpression, THIRExpressionKind, THIRFloatBinOp,
            THIRPtrBinOp,
        },
        r#type::{THIRIntType, THIRType, THIRTypeKind},
    },
    type_context::THIRTypeContext,
};

use crate::{
    environment::TypeEnvironment,
    type_checking::coercion::{
        CoercionResult,
        implicit::{
            self, coercion_expr,
            promotion::{integer, lvalue, std_rval_promotion, std_rval_promotion_coercion},
        },
    },
};

pub mod compatible;

pub fn try_implicit_coercion(
    env: &mut TypeEnvironment,
    expr: THIRExpression,
    target_type: &THIRType,
) -> CXResult<CoercionResult> {
    let from_type = expr.get_type();

    if env.type_eq(&from_type, target_type) {
        return CoercionResult::success(expr);
    }

    if compatible::compatible_types(env, &expr._type, target_type)? {
        return CoercionResult::success(THIRExpression {
            token_range: expr.token_range.clone(),
            kind: THIRExpressionKind::TypeConversion {
                conversion: THIRCoercion::Typechange,
                operand: Box::new(expr),
            },
            _type: target_type.clone(),
        });
    }

    match internal(env, expr, from_type, target_type)? {
        CoercionResult::Success { expr } => {
            if env.type_eq(expr.get_type_ref(), target_type) {
                CoercionResult::success(expr)
            } else {
                try_implicit_coercion(env, expr, target_type)
            }
        }

        other => Ok(other),
    }
}

fn internal(
    env: &mut TypeEnvironment,
    expr: THIRExpression,
    from_type: THIRType,
    target_type: &THIRType,
) -> CXResult<CoercionResult> {
    if env.symbols.is_cx_str(&from_type) && is_char_array(env, target_type) {
        return coercion_expr(expr, target_type.clone(), THIRCoercion::ReinterpretBits);
    }

    if env.symbols.is_cx_str(&from_type)
        && matches!(target_type.kind, THIRTypeKind::PointerTo { .. })
    {
        return coercion_expr(expr, target_type.clone(), THIRCoercion::ReinterpretBits);
    }

    if matches!(expr.kind, THIRExpressionKind::IntLiteral(0))
        && matches!(target_type.kind, THIRTypeKind::PointerTo { .. })
    {
        return coercion_expr(
            expr,
            target_type.clone(),
            THIRCoercion::IntToPtr { sextend: false },
        );
    }

    if let (
        THIRTypeKind::Array {
            inner_type: from_inner,
            ..
        },
        THIRTypeKind::PointerTo {
            inner_type: to_inner,
        },
    ) = (&from_type.kind, &target_type.kind)
        && compatible::compatible_types(
            env,
            env.symbols.resolve_type_id(*from_inner),
            env.symbols.resolve_type_id(*to_inner),
        )?
    {
        return coercion_expr(expr, target_type.clone(), THIRCoercion::ReinterpretBits);
    }

    if expr._type.is_integer() {
        if let THIRTypeKind::Float { _type } = &target_type.kind {
            let THIRTypeKind::Integer { signed, .. } = &expr._type.kind else {
                unreachable!("integer type predicate should match integer kind");
            };
            let signed = *signed;

            return coercion_expr(
                expr,
                target_type.clone(),
                THIRCoercion::IntToFloat {
                    to_type: *_type,
                    sextend: signed,
                },
            );
        }

        return integer::try_conversion(env, expr, target_type);
    }

    // TODO: Organize this into different XXX::try_conversion functions / modules
    match (&expr._type.kind, &target_type.kind) {
        (THIRTypeKind::Float { _type: from_float }, THIRTypeKind::Float { _type: to_float })
            if from_float != to_float =>
        {
            implicit::coercion_expr(
                expr,
                target_type.clone(),
                THIRCoercion::FloatCast { to_type: *to_float },
            )
        }

        (
            THIRTypeKind::Float { _type: from_float },
            THIRTypeKind::Integer {
                _type: THIRIntType::I1,
                ..
            },
        ) => CoercionResult::success(THIRExpression {
            _type: target_type.clone(),
            token_range: expr.token_range.clone(),
            kind: THIRExpressionKind::BinaryOperation {
                op: THIRBinOp::Float {
                    ftype: *from_float,
                    op: THIRFloatBinOp::FNE,
                },
                rhs: Box::new(THIRExpression {
                    _type: THIRTypeKind::Float { _type: *from_float }.into(),
                    token_range: expr.token_range.clone(),
                    kind: THIRExpressionKind::FloatLiteral(0.0.into()),
                }),
                lhs: Box::new(expr),
            },
        }),

        (
            THIRTypeKind::PointerTo { .. },
            THIRTypeKind::Integer {
                _type: THIRIntType::I1,
                ..
            },
        ) => CoercionResult::success(THIRExpression {
            _type: target_type.clone(),
            token_range: expr.token_range.clone(),
            kind: THIRExpressionKind::BinaryOperation {
                op: THIRBinOp::Pointer {
                    op: THIRPtrBinOp::NE,
                },
                rhs: Box::new(THIRExpression {
                    _type: from_type.clone(),
                    token_range: expr.token_range.clone(),
                    kind: THIRExpressionKind::TypeConversion {
                        conversion: THIRCoercion::IntToPtr { sextend: false },
                        operand: Box::new(THIRExpression {
                            _type: env.get_intrinsic_type("int"),
                            token_range: expr.token_range.clone(),
                            kind: THIRExpressionKind::IntLiteral(0),
                        }),
                    },
                }),
                lhs: Box::new(expr),
            },
        }),

        (
            THIRTypeKind::Float { .. },
            THIRTypeKind::Integer {
                signed,
                _type: to_int,
            },
        ) => implicit::coercion_expr(
            expr,
            target_type.clone(),
            THIRCoercion::FloatToInt {
                to_type: *to_int,
                sextend: *signed,
            },
        ),

        (THIRTypeKind::PointerTo { .. }, THIRTypeKind::Integer { _type: itype, .. }) => {
            implicit::coercion_expr(
                expr,
                target_type.clone(),
                THIRCoercion::PtrToInt { to_type: *itype },
            )
        }

        (
            THIRTypeKind::MemoryReference { inner_type: i1, .. },
            THIRTypeKind::MemoryReference { inner_type: i2, .. },
        ) => {
            let i1 = env.symbols.resolve_type_id(*i1);
            let i2 = env.symbols.resolve_type_id(*i2);

            if i1.is_memory_reference() {
                return lvalue::try_conversion(env, expr, false);
            }

            if compatible::compatible_types(env, i1, i2)? {
                return implicit::coercion_expr(
                    expr,
                    target_type.clone(),
                    THIRCoercion::ReinterpretBits,
                );
            }

            if env.symbols.cvr_compatible(i1, i2)
                && env.type_eq(
                    &i1.clone().without_specifiers(),
                    &i2.clone().without_specifiers(),
                )
            {
                return implicit::coercion_expr(
                    expr,
                    target_type.clone(),
                    THIRCoercion::ReinterpretBits,
                );
            }

            CoercionResult::unapplied(expr)
        }

        // Note: type 2 is not a memory reference due to previous case
        (THIRTypeKind::MemoryReference { .. }, _) => {
            std_rval_promotion(env, expr).and_then(CoercionResult::success)
        }

        (_, THIRTypeKind::PointerTo { inner_type })
            if env.type_eq(&from_type, env.symbols.resolve_type_id(*inner_type)) =>
        {
            std_rval_promotion_coercion(env, expr)
        }

        (
            THIRTypeKind::PointerTo {
                inner_type: from_ptr,
            },
            THIRTypeKind::PointerTo { inner_type: to_ptr },
        ) => {
            let from_inner = env.symbols.resolve_type_id(*from_ptr);
            let to_inner = env.symbols.resolve_type_id(*to_ptr);

            if env.symbols.resolve_type_id(*from_ptr).is_unit()
                || env.symbols.resolve_type_id(*to_ptr).is_unit()
            {
                return implicit::coercion_expr(
                    expr,
                    target_type.clone(),
                    THIRCoercion::ReinterpretBits,
                );
            }

            // If we are coercing T1* -> T2* and they are compatible as unqualified types, and we only
            // add cvr-specifiers to coerce, than this is a valid implicit cast
            if compatible::compatible_types(
                env,
                &from_inner.clone().without_specifiers(),
                &to_inner.clone().without_specifiers(),
            )? && from_inner.specifiers & to_inner.specifiers == from_inner.specifiers
            {
                return implicit::coercion_expr(
                    expr,
                    target_type.clone(),
                    THIRCoercion::ReinterpretBits,
                );
            }

            CoercionResult::unapplied(expr)
        }

        _ => CoercionResult::unapplied(expr),
    }
}

fn is_char_array(env: &TypeEnvironment, ty: &THIRType) -> bool {
    let ty = env.symbols.mem_ref_inner(ty).unwrap_or(ty);
    let THIRTypeKind::Array { inner_type, .. } = ty.kind else {
        return false;
    };
    matches!(
        env.symbols.resolve_type_id(inner_type).kind,
        THIRTypeKind::Integer {
            _type: THIRIntType::I8,
            signed: false,
        }
    )
}
