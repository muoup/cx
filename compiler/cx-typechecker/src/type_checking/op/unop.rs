use cx_hir::ast::{
    expression::{HIRExpression, HIRUnOp},
    types::HIRType,
};
use cx_log::CXResult;
use cx_thir::{
    EnvironmentNamespace,
    thir::{
        expression::{THIRCoercion, THIRExpression, THIRExpressionKind, THIRUnOp},
        r#type::{THIRIntType, THIRType, THIRTypeKind},
    },
    type_context::THIRTypeContext,
};
use cx_tokens::TokenRange;

use crate::{
    environment::TypeEnvironment,
    symbol::completion::complete_type,
    type_checking::{
        coercion::{
            explicit::explicit_cast,
            implicit::{implicit_cast, promotion::std_rval_promotion},
        },
        op::binop::is::typecheck_is,
        result::TypecheckResult,
        typechecker::typecheck_expr,
        value::moves::typecheck_move,
    },
};

pub fn typecheck_unop(
    env: &mut TypeEnvironment,
    namespace: &EnvironmentNamespace,
    op: &HIRUnOp,
    operand: &HIRExpression,
) -> CXResult<TypecheckResult> {
    Ok(match op {
        HIRUnOp::Move => typecheck_expr(env, namespace, operand, None)
            .and_then(|v| typecheck_move(env, namespace, v, operand))?,

        HIRUnOp::PreIncrement(increment_amount) | HIRUnOp::PostIncrement(increment_amount) => {
            let operand = typecheck_expr(env, namespace, operand, None)
                .and_then(|v| v.standard_ready_coerce(env, operand.token_range()))?;

            let Some(inner) = env.symbols.mem_ref_inner(&operand._type).cloned() else {
                return env.log_error(
                    &operand.token_range,
                    format!(
                        "Cannot apply pre-increment to non-reference type {}",
                        operand._type.display_with(&env.symbols)
                    ),
                );
            };

            match &inner.kind {
                THIRTypeKind::PointerTo { .. } | THIRTypeKind::Integer { .. } => match op {
                    HIRUnOp::PreIncrement(_) => TypecheckResult::new(
                        operand._type.clone(),
                        THIRExpressionKind::UnaryOperation {
                            op: THIRUnOp::PreIncrement(*increment_amount),
                            operand: Box::new(operand),
                        },
                    ),
                    HIRUnOp::PostIncrement(_) => TypecheckResult::new(
                        inner.clone(),
                        THIRExpressionKind::UnaryOperation {
                            op: THIRUnOp::PostIncrement(*increment_amount),
                            operand: Box::new(operand),
                        },
                    ),
                    _ => unreachable!(),
                },

                _ => {
                    return env.log_error(
                        &operand.token_range,
                        format!(
                            "Pre-increment operator requires an integer or pointer type, found {}",
                            inner.display_with(&env.symbols)
                        ),
                    );
                }
            }
        }

        HIRUnOp::LNot => {
            let operand = typecheck_expr(env, namespace, operand, None)
                .and_then(|v| v.standard_ready_coerce(env, operand.token_range()))
                .and_then(|v| std_rval_promotion(env, v))
                .and_then(|v| implicit_cast(env, v, &THIRType::bool()))?;

            TypecheckResult::new(
                THIRTypeKind::Integer {
                    _type: THIRIntType::I1,
                    signed: false,
                }
                .into(),
                THIRExpressionKind::UnaryOperation {
                    operand: Box::new(operand),
                    op: THIRUnOp::LNOT,
                },
            )
        }

        HIRUnOp::BNot => {
            let operand = typecheck_expr(env, namespace, operand, None)
                .and_then(|v| v.standard_ready_coerce(env, operand.token_range()))
                .and_then(|v| std_rval_promotion(env, v))?;

            if !operand._type.is_integer() {
                return env.log_error(
                    &operand.token_range,
                    format!(
                        "Bitwise NOT operator requires an integer type, found {}",
                        operand._type.display_with(&env.symbols)
                    ),
                );
            }

            TypecheckResult::new(
                operand._type.clone(),
                THIRExpressionKind::UnaryOperation {
                    operand: Box::new(operand),
                    op: THIRUnOp::BNOT,
                },
            )
        }

        HIRUnOp::Negative => {
            let operand = typecheck_expr(env, namespace, operand, None)
                .and_then(|v| v.standard_ready_coerce(env, operand.token_range()))
                .and_then(|v| std_rval_promotion(env, v))?;

            let operator = match &operand._type.kind {
                THIRTypeKind::Integer { .. } => THIRUnOp::NEG,
                THIRTypeKind::Float { .. } => THIRUnOp::FNEG,

                _ => {
                    return env.log_error(
                        &operand.token_range,
                        format!(
                            "Negation operator requires an integer or float type, found {}",
                            operand.display_with(&env.symbols)
                        ),
                    );
                }
            };

            TypecheckResult::new(
                operand._type.clone(),
                THIRExpressionKind::UnaryOperation {
                    operand: Box::new(operand),
                    op: operator,
                },
            )
        }

        HIRUnOp::AddressOf => {
            let operand = typecheck_expr(env, namespace, operand, None)
                .and_then(|v| v.standard_ready_coerce(env, operand.token_range()))?;

            let Some(inner) = env.symbols.mem_ref_inner(&operand._type).cloned() else {
                return env.log_error(
                    &operand.token_range,
                    "Cannot take the address of a non-reference type".to_string(),
                );
            };

            // AddressOf just returns the operand (which is a reference) as a pointer
            TypecheckResult::from(THIRExpression {
                token_range: operand.token_range.clone(),
                _type: env.symbols.pointer_to(inner.clone()),
                kind: THIRExpressionKind::TypeConversion {
                    operand: Box::new(operand),
                    conversion: THIRCoercion::ReinterpretBits,
                },
            })
        }

        HIRUnOp::Dereference => {
            let operand = typecheck_expr(env, namespace, operand, None)
                .and_then(|v| v.standard_ready_coerce(env, operand.token_range()))
                .and_then(|v| std_rval_promotion(env, v))?;

            if env.function.in_safe_context()
                && matches!(operand._type.kind, THIRTypeKind::PointerTo { .. })
            {
                return env.log_error(
                    &operand.token_range,
                    "Dereferencing raw pointers is not allowed in safe contexts".to_string(),
                );
            }

            let Some(inner) = env.symbols.ptr_inner(&operand._type).cloned() else {
                return env.log_error(
                    &operand.token_range,
                    format!(
                        "Cannot dereference non-pointer type {}",
                        operand._type.display_with(&env.symbols)
                    ),
                );
            };

            // Dereference returns a memory reference to the inner type
            TypecheckResult::from(THIRExpression {
                token_range: TokenRange::internal(),
                kind: THIRExpressionKind::Typechange(Box::new(operand)),
                _type: env.symbols.mem_ref_to(inner),
            })
        }

        HIRUnOp::ExplicitCast(to_type) => {
            let to_type = complete_type(env, namespace, to_type)?;

            let operand = typecheck_expr(env, namespace, operand, Some(&to_type))
                .and_then(|v| v.standard_ready_coerce(env, operand.token_range()))
                .and_then(|v| std_rval_promotion(env, v))?;

            TypecheckResult::from(explicit_cast(env, operand, &to_type)?)
        }

        HIRUnOp::Is(pattern) => typecheck_is(env, namespace, operand, pattern, operand)?,
    })
}

pub(crate) fn typecheck_sizeof_type(
    env: &mut TypeEnvironment,
    namespace: &EnvironmentNamespace,
    _expr: &HIRExpression,
    ty: &HIRType,
) -> CXResult<TypecheckResult> {
    let tc_type = complete_type(env, namespace, ty)?;

    Ok(sizeof_result(_expr.range.clone(), tc_type))
}

pub(crate) fn typecheck_alignof_type(
    env: &mut TypeEnvironment,
    namespace: &EnvironmentNamespace,
    expr: &HIRExpression,
    ty: &HIRType,
) -> CXResult<TypecheckResult> {
    let tc_type = complete_type(env, namespace, ty)?;
    Ok(alignof_result(expr.range.clone(), tc_type))
}

pub(crate) fn typecheck_alignof_expr(
    env: &mut TypeEnvironment,
    namespace: &EnvironmentNamespace,
    expr: &HIRExpression,
) -> CXResult<TypecheckResult> {
    let tc_expr = typecheck_expr(env, namespace, expr, None)
        .and_then(|v| v.standard_ready_coerce(env, expr.token_range()))?;
    let _type = env
        .symbols
        .mem_ref_inner(&tc_expr._type)
        .cloned()
        .unwrap_or(tc_expr._type);
    Ok(alignof_result(tc_expr.token_range, _type))
}

pub(crate) fn typecheck_sizeof_expr(
    env: &mut TypeEnvironment,
    namespace: &EnvironmentNamespace,
    expr: &HIRExpression,
) -> CXResult<TypecheckResult> {
    let tc_expr = typecheck_expr(env, namespace, expr, None)
        .and_then(|v| v.standard_ready_coerce(env, expr.token_range()))?;

    let _type = env
        .symbols
        .mem_ref_inner(&tc_expr._type)
        .cloned()
        .unwrap_or(tc_expr._type);
    Ok(sizeof_result(tc_expr.token_range, _type))
}

fn alignof_result(range: TokenRange, _type: THIRType) -> TypecheckResult {
    TypecheckResult::from(THIRExpression {
        token_range: range,
        kind: THIRExpressionKind::AlignOf { _type },
        _type: THIRType::from(THIRTypeKind::Integer {
            _type: THIRIntType::I64,
            signed: false,
        }),
    })
}

fn sizeof_result(range: TokenRange, _type: THIRType) -> TypecheckResult {
    TypecheckResult::from(THIRExpression {
        token_range: range,
        kind: THIRExpressionKind::SizeOf { _type },
        _type: THIRType::from(THIRTypeKind::Integer {
            _type: THIRIntType::I64,
            signed: false,
        }),
    })
}
