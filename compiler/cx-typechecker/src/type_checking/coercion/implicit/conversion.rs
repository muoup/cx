use cx_log::CXResult;
use cx_thir::{
    thir::{
        contextual_eq::TypeContextEqual,
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
            promotion::{integer, lvalue, std_rval_promotion_coercion},
        },
    },
};

pub mod compatible;

pub fn try_implicit_coercion(
    env: &mut TypeEnvironment,
    expr: THIRExpression,
    target_type: &THIRType,
) -> CXResult<CoercionResult> {
    let from_type = expr.ty.clone();

    if env.type_eq(&from_type, target_type) {
        return CoercionResult::success(expr);
    }

    if from_type.is_unreachable() {
        return coercion_expr(expr, target_type.clone(), THIRCoercion::Unreachable);
    }

    if compatible::compatible_types(env, &expr.ty, target_type)? {
        let conversion = if expr.ty.is_pointer() && target_type.is_pointer() {
            THIRCoercion::Bitcast
        } else {
            THIRCoercion::Typechange
        };
        return coercion_expr(expr, target_type.clone(), conversion);
    }

    match internal(env, expr, from_type, target_type)? {
        CoercionResult::Success { expr } => {
            if env.type_eq(&expr.ty, target_type) {
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
        return coercion_expr(expr, target_type.clone(), THIRCoercion::StringToArray);
    }

    if env.symbols.is_cx_str(&from_type)
        && matches!(target_type.kind, THIRTypeKind::PointerTo { .. })
    {
        return CoercionResult::success(THIRExpression {
            token_range: expr.token_range.clone(),
            ty: target_type.clone(),
            kind: THIRExpressionKind::AddressOf {
                operand: Box::new(expr),
            },
        });
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
        return CoercionResult::success(THIRExpression {
            token_range: expr.token_range.clone(),
            ty: target_type.clone(),
            kind: THIRExpressionKind::AddressOf {
                operand: Box::new(expr),
            },
        });
    }

    if expr.ty.is_integer() {
        if let THIRTypeKind::Float { ty } = &target_type.kind {
            let THIRTypeKind::Integer { signed, .. } = &expr.ty.kind else {
                unreachable!("integer type predicate should match integer kind");
            };
            let signed = *signed;

            return coercion_expr(
                expr,
                target_type.clone(),
                THIRCoercion::IntToFloat {
                    to_type: *ty,
                    sextend: signed,
                },
            );
        }

        return integer::try_conversion(env, expr, target_type);
    }

    // TODO: Organize this into different XXX::try_conversion functions / modules
    match (&expr.ty.kind, &target_type.kind) {
        (THIRTypeKind::Float { ty: from_float }, THIRTypeKind::Float { ty: to_float })
            if from_float != to_float =>
        {
            implicit::coercion_expr(
                expr,
                target_type.clone(),
                THIRCoercion::FloatCast { to_type: *to_float },
            )
        }

        (
            THIRTypeKind::Float { ty: from_float },
            THIRTypeKind::Integer {
                ty: THIRIntType::I1,
                ..
            },
        ) => CoercionResult::success(THIRExpression {
            ty: target_type.clone(),
            token_range: expr.token_range.clone(),
            kind: THIRExpressionKind::BinaryOperation {
                op: THIRBinOp::Float {
                    ftype: *from_float,
                    op: THIRFloatBinOp::FNE,
                },
                rhs: Box::new(THIRExpression {
                    ty: THIRTypeKind::Float { ty: *from_float }.into(),
                    token_range: expr.token_range.clone(),
                    kind: THIRExpressionKind::FloatLiteral(0.0.into()),
                }),
                lhs: Box::new(expr),
            },
        }),

        (
            THIRTypeKind::PointerTo { .. },
            THIRTypeKind::Integer {
                ty: THIRIntType::I1,
                ..
            },
        ) => CoercionResult::success(THIRExpression {
            ty: target_type.clone(),
            token_range: expr.token_range.clone(),
            kind: THIRExpressionKind::BinaryOperation {
                op: THIRBinOp::Pointer {
                    op: THIRPtrBinOp::NE,
                },
                rhs: Box::new(THIRExpression {
                    ty: from_type.clone(),
                    token_range: expr.token_range.clone(),
                    kind: THIRExpressionKind::TypeConversion {
                        conversion: THIRCoercion::IntToPtr { sextend: false },
                        operand: Box::new(THIRExpression {
                            ty: env.get_intrinsic_type("int"),
                            token_range: expr.token_range.clone(),
                            kind: THIRExpressionKind::IntLiteral(0),
                        }),
                    },
                }),
                lhs: Box::new(expr),
            },
        }),

        (THIRTypeKind::Float { .. }, THIRTypeKind::Integer { signed, ty: to_int }) => {
            implicit::coercion_expr(
                expr,
                target_type.clone(),
                THIRCoercion::FloatToInt {
                    to_type: *to_int,
                    sextend: *signed,
                },
            )
        }

        (THIRTypeKind::PointerTo { .. }, THIRTypeKind::Integer { ty: itype, .. }) => {
            implicit::coercion_expr(
                expr,
                target_type.clone(),
                THIRCoercion::PtrToInt { to_type: *itype },
            )
        }

        (THIRTypeKind::Function { .. }, THIRTypeKind::PointerTo { inner_type, .. })
            if from_type.contextual_eq(env.symbols.resolve_type_id(*inner_type), &env.symbols) =>
        {
            CoercionResult::success(THIRExpression {
                token_range: expr.token_range.clone(),
                ty: target_type.clone(),
                kind: THIRExpressionKind::AddressOf {
                    operand: Box::new(expr),
                },
            })
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
                    THIRCoercion::Typechange,
                );
            }

            if i1.cvr_compatible_with(i2)
                && env.type_eq(
                    &i1.clone().without_specifiers(),
                    &i2.clone().without_specifiers(),
                )
            {
                return implicit::coercion_expr(
                    expr,
                    target_type.clone(),
                    THIRCoercion::Typechange,
                );
            }

            CoercionResult::unapplied(expr)
        }

        // Note: to type is not a memory reference due to previous case
        (THIRTypeKind::MemoryReference { .. }, _) => std_rval_promotion_coercion(env, expr),

        (
            THIRTypeKind::PointerTo {
                inner_type: from_ptr,
            },
            THIRTypeKind::PointerTo { inner_type: to_ptr },
        ) => {
            let from_inner = env.symbols.resolve_type_id(*from_ptr);
            let to_inner = env.symbols.resolve_type_id(*to_ptr);

            if env.symbols.resolve_type_id(*from_ptr).is_void()
                || env.symbols.resolve_type_id(*to_ptr).is_void()
            {
                return implicit::coercion_expr(expr, target_type.clone(), THIRCoercion::Bitcast);
            }

            // If we are coercing T1* -> T2* and they are compatible as unqualified types, and we only
            // add cvr-specifiers to coerce, than this is a valid implicit cast
            if compatible::compatible_types(
                env,
                &from_inner.clone().without_specifiers(),
                &to_inner.clone().without_specifiers(),
            )? && from_inner.specifiers & to_inner.specifiers == from_inner.specifiers
            {
                return implicit::coercion_expr(expr, target_type.clone(), THIRCoercion::Bitcast);
            }

            CoercionResult::unapplied(expr)
        }

        _ => CoercionResult::unapplied(expr),
    }
}

pub(crate) fn is_char_array(env: &TypeEnvironment, ty: &THIRType) -> bool {
    let ty = env.symbols.mem_ref_inner(ty).unwrap_or(ty);

    let Some(inner_type) = env.symbols.array_inner(ty) else {
        return false;
    };

    matches!(
        inner_type.kind,
        THIRTypeKind::Integer {
            ty: THIRIntType::I8,
            signed: false,
        }
    )
}
