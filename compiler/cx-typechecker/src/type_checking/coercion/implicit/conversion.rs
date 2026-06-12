use cx_ast::ast::modifiers::CX_CONST;
use cx_log::CXResult;
use cx_mir::{
    mir::{
        expression::{MIRBinOp, MIRCoercion, MIRExpression, MIRExpressionKind, MIRFloatBinOp, MIRPtrBinOp},
        r#type::{MIRIntegerType, MIRType, MIRTypeKind},
    },
    type_context::MIRTypeContext,
};

use crate::{
    environment::TypeEnvironment,
    type_checking::coercion::{
        CoercionResult,
        implicit::{
            self, coercion_expr, implicit_cast, promotion::{integer, lvalue, std_rval_promotion, std_rval_promotion_coercion}
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

            return coercion_expr(
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

    // TODO: Organize this into different XXX::try_conversion functions / modules
    match (&expr._type.kind, &target_type.kind) {
        (MIRTypeKind::Float { _type: from_float }, MIRTypeKind::Float { _type: to_float })
            if from_float != to_float =>
        {
            implicit::coercion_expr(
                expr,
                target_type.clone(),
                MIRCoercion::FloatCast { to_type: *to_float },
            )
        }

        (MIRTypeKind::Float { _type: from_float }, MIRTypeKind::Integer { _type: MIRIntegerType::I1, .. }) =>
        {
            CoercionResult::success(
                MIRExpression {
                    _type: target_type.clone(),
                    token_range: expr.token_range.clone(),
                    kind: MIRExpressionKind::BinaryOperation {
                        op: MIRBinOp::Float { ftype: *from_float, op: MIRFloatBinOp::FNE },
                        rhs: Box::new(MIRExpression {
                            _type: MIRTypeKind::Float { _type: *from_float }.into(),
                            token_range: expr.token_range.clone(),
                            kind: MIRExpressionKind::FloatLiteral(0.0.into())
                        }),
                        lhs: Box::new(expr),
                    },
                }
            )
        },

        (MIRTypeKind::PointerTo { .. }, MIRTypeKind::Integer { _type: MIRIntegerType::I1, .. }) => {
            CoercionResult::success(
                MIRExpression {
                    _type: target_type.clone(),
                    token_range: expr.token_range.clone(),
                    kind: MIRExpressionKind::BinaryOperation {
                        op: MIRBinOp::Pointer { op: MIRPtrBinOp::NE },
                        rhs: Box::new(MIRExpression {
                            _type: from_type.clone(),
                            token_range: expr.token_range.clone(),
                            kind: MIRExpressionKind::TypeConversion { 
                                conversion: MIRCoercion::IntToPtr { sextend: false },
                                operand: Box::new(MIRExpression {
                                    _type: env.get_intrinsic_type("int"),
                                    token_range: expr.token_range.clone(),
                                    kind: MIRExpressionKind::IntLiteral(0)
                                })
                            }
                        }),
                        lhs: Box::new(expr),
                    },
                }
            )
        },

        (MIRTypeKind::Float { .. }, MIRTypeKind::Integer { signed, _type: to_int }) => {
            implicit::coercion_expr(
                expr,
                target_type.clone(),
                MIRCoercion::FloatToInt { 
                    to_type: *to_int,
                    sextend: *signed,
                }
            )
        },

        (MIRTypeKind::PointerTo { .. }, MIRTypeKind::Integer { _type: itype, .. }) => {
            implicit::coercion_expr(
                expr, 
                target_type.clone(),
                MIRCoercion::PtrToInt { 
                    to_type: *itype
                }
            )
        }

        (MIRTypeKind::MemoryReference { inner_type, .. }, _)
            if !target_type.is_memory_reference() =>
        {
            lvalue::try_conversion(env, expr)?
                .and_then(|expr| try_implicit_coercion(env, expr, target_type))
        }

        (_, MIRTypeKind::PointerTo { inner_type })
            if env.type_eq(&from_type, env.symbols.resolve_type_id(*inner_type)) =>
        {
            std_rval_promotion_coercion(env, expr)
        }

        (
            MIRTypeKind::PointerTo {
                inner_type: from_ptr,
            },
            MIRTypeKind::PointerTo { inner_type: to_ptr },
        ) => {
            let from_inner = env.symbols.resolve_type_id(*from_ptr);
            let to_inner = env.symbols.resolve_type_id(*to_ptr);

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
                    MIRCoercion::ReinterpretBits,
                );
            }

            CoercionResult::unapplied(expr)
        }

        _ => CoercionResult::unapplied(expr),
    }
}

// FIXME: Do we need this?
pub fn try_argument_conversion(
    env: &mut TypeEnvironment,
    expr: MIRExpression,
    argument_type: &MIRType,
) -> CXResult<MIRExpression> {
    if let Some(expr) = try_deferred_by_value_argument(env, &expr, argument_type) {
        return implicit_cast(env, expr, argument_type);
    }

    let expr = if argument_type.is_memory_reference() {
        let expr_type = expr.get_type();
        if let Some(inner) = env.symbols.mem_ref_inner(&expr_type)
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

fn try_deferred_by_value_argument(
    env: &TypeEnvironment,
    expr: &MIRExpression,
    argument_type: &MIRType,
) -> Option<MIRExpression> {
    if argument_type.is_memory_reference() {
        return None;
    }

    let expr_type = expr.get_type();
    let inner = env.symbols.mem_ref_inner(&expr_type)?;
    let unqualified_inner = inner.without_specifier(CX_CONST);

    if !is_deferred_by_value_candidate(&unqualified_inner)
        || unqualified_inner.is_nocopy()
        || !env.type_eq(&unqualified_inner, argument_type)
    {
        return None;
    }

    Some(MIRExpression {
        token_range: expr.token_range.clone(),
        _type: argument_type.clone(),
        kind: MIRExpressionKind::ByValueArgument {
            source: Box::new(expr.clone()),
        },
    })
}

fn is_deferred_by_value_candidate(ty: &MIRType) -> bool {
    matches!(
        ty.kind,
        MIRTypeKind::Structured { .. }
            | MIRTypeKind::Union { .. }
            | MIRTypeKind::TaggedUnion { .. }
    )
}
