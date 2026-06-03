use cx_ast::ast::{
    expression::{CXExpression, CXUnOp},
    types::CXType,
};
use cx_mir::{
    mir::{
        expression::{MIRCoercion, MIRExpression, MIRExpressionKind, MIRUnOp},
        program::EnvironmentNamespace,
        r#type::{MIRIntegerType, MIRType, MIRTypeKind},
    },
    symbol::completion::complete_type,
    type_context::MIRTypeContext,
};
use cx_util::CXResult;

use crate::{
    environment::TypeEnvironment,
    log_typecheck_error,
    type_checking::{
        coercion::{
            explicit::explicit_cast,
            implicit::{implicit_cast, promotion::std_rval_promotion},
        },
        op::binop::is::typecheck_is,
        result::TypecheckResult,
        typechecker::typecheck_expr,
    },
};

pub fn typecheck_unop(
    env: &mut TypeEnvironment,
    namespace: &EnvironmentNamespace,
    op: &CXUnOp,
    operand: &CXExpression,
) -> CXResult<TypecheckResult> {
    Ok(match op {
        CXUnOp::PreIncrement(increment_amount) | CXUnOp::PostIncrement(increment_amount) => {
            let operand = typecheck_expr(env, namespace, operand, None)
                .and_then(|v| v.standard_ready_coerce(env, operand.token_range()))?;

            let Some(inner) = env.symbols.mem_ref_inner(&operand._type).cloned() else {
                return log_typecheck_error!(
                    env,
                    operand.token_range.as_ref(),
                    "Cannot apply pre-increment to non-reference type {}",
                    operand._type.display_with(&env.symbols)
                );
            };

            match &inner.kind {
                MIRTypeKind::PointerTo { .. } | MIRTypeKind::Integer { .. } => match op {
                    CXUnOp::PreIncrement(_) => TypecheckResult::new(
                        operand._type.clone(),
                        MIRExpressionKind::UnaryOperation {
                            op: MIRUnOp::PreIncrement(*increment_amount),
                            operand: Box::new(operand),
                        },
                    ),
                    CXUnOp::PostIncrement(_) => TypecheckResult::new(
                        inner.clone(),
                        MIRExpressionKind::UnaryOperation {
                            op: MIRUnOp::PostIncrement(*increment_amount),
                            operand: Box::new(operand),
                        },
                    ),
                    _ => unreachable!(),
                },

                _ => {
                    return log_typecheck_error!(
                        env,
                        operand.token_range.as_ref(),
                        "Pre-increment operator requires an integer or pointer type, found {}",
                        inner.display_with(&env.symbols)
                    );
                }
            }
        }

        CXUnOp::LNot => {
            let operand = typecheck_expr(env, namespace, operand, None)
                .and_then(|v| v.standard_ready_coerce(env, operand.token_range()))
                .and_then(|v| std_rval_promotion(env, v))
                .and_then(|v| implicit_cast(env, v, &MIRType::bool()))?;

            TypecheckResult::new(
                MIRTypeKind::Integer {
                    _type: MIRIntegerType::I1,
                    signed: false,
                }
                .into(),
                MIRExpressionKind::UnaryOperation {
                    operand: Box::new(operand),
                    op: MIRUnOp::LNOT,
                },
            )
        }

        CXUnOp::BNot => {
            let operand = typecheck_expr(env, namespace, operand, None)
                .and_then(|v| v.standard_ready_coerce(env, operand.token_range()))
                .and_then(|v| std_rval_promotion(env, v))?;

            if !operand._type.is_integer() {
                return log_typecheck_error!(
                    env,
                    operand.token_range.as_ref(),
                    "Bitwise NOT operator requires an integer type, found {}",
                    operand._type.display_with(&env.symbols)
                );
            }

            TypecheckResult::new(
                operand._type.clone(),
                MIRExpressionKind::UnaryOperation {
                    operand: Box::new(operand),
                    op: MIRUnOp::BNOT,
                },
            )
        }

        CXUnOp::Negative => {
            let operand = typecheck_expr(env, namespace, operand, None)
                .and_then(|v| v.standard_ready_coerce(env, operand.token_range()))
                .and_then(|v| std_rval_promotion(env, v))?;

            let operator = match &operand._type.kind {
                MIRTypeKind::Integer { .. } => MIRUnOp::NEG,
                MIRTypeKind::Float { .. } => MIRUnOp::FNEG,

                _ => {
                    return log_typecheck_error!(
                        env,
                        operand.token_range.as_ref(),
                        "Negation operator requires an integer or float type, found {}",
                        operand.display_with(&env.symbols)
                    );
                }
            };

            TypecheckResult::new(
                operand._type.clone(),
                MIRExpressionKind::UnaryOperation {
                    operand: Box::new(operand),
                    op: operator,
                },
            )
        }

        CXUnOp::AddressOf => {
            let operand = typecheck_expr(env, namespace, operand, None)
                .and_then(|v| v.standard_ready_coerce(env, operand.token_range()))?;

            let Some(inner) = env.symbols.mem_ref_inner(&operand._type).cloned() else {
                return log_typecheck_error!(
                    env,
                    operand.token_range.as_ref(),
                    "Cannot take the address of a non-reference type"
                );
            };

            // AddressOf just returns the operand (which is a reference) as a pointer
            TypecheckResult::from(MIRExpression {
                token_range: operand.token_range.clone(),
                _type: env.symbols.pointer_to(inner.clone()),
                kind: MIRExpressionKind::TypeConversion {
                    operand: Box::new(operand),
                    conversion: MIRCoercion::ReinterpretBits,
                },
            })
        }

        CXUnOp::Dereference => {
            let operand = typecheck_expr(env, namespace, operand, None)
                .and_then(|v| v.standard_ready_coerce(env, operand.token_range()))
                .and_then(|v| std_rval_promotion(env, v))?;

            let Some(inner) = env.symbols.ptr_inner(&operand._type).cloned() else {
                return log_typecheck_error!(
                    env,
                    operand.token_range.as_ref(),
                    "Cannot dereference non-pointer type {}",
                    operand._type.display_with(&env.symbols)
                );
            };

            // Dereference returns a memory reference to the inner type
            TypecheckResult::from(MIRExpression {
                token_range: None,
                kind: MIRExpressionKind::Typechange(Box::new(operand)),
                _type: env.symbols.mem_ref_to(inner),
            })
        }

        CXUnOp::ExplicitCast(to_type) => {
            let to_type = complete_type(&mut env.symbols, namespace, to_type)?;

            let operand = typecheck_expr(env, namespace, operand, Some(&to_type))
                .and_then(|v| v.standard_ready_coerce(env, operand.token_range()))
                .and_then(|v| std_rval_promotion(env, v))?;

            TypecheckResult::from(explicit_cast(env, operand, &to_type)?)
        }

        CXUnOp::Is(pattern) => typecheck_is(env, namespace, operand, pattern, operand)?,
    })
}

pub(crate) fn typecheck_sizeof_type(
    env: &mut TypeEnvironment,
    namespace: &EnvironmentNamespace,
    _expr: &CXExpression,
    ty: &CXType,
) -> CXResult<TypecheckResult> {
    let tc_type = complete_type(&mut env.symbols, namespace, ty)?;
    Ok(sizeof_result(tc_type.padded_size(&env.symbols)))
}

pub(crate) fn typecheck_sizeof_expr(
    env: &mut TypeEnvironment,
    namespace: &EnvironmentNamespace,
    expr: &CXExpression,
) -> CXResult<TypecheckResult> {
    let tc_expr = typecheck_expr(env, namespace, expr, None)
        .and_then(|v| v.standard_ready_coerce(env, expr.token_range()))?;

    Ok(sizeof_result(tc_expr._type.padded_size(&env.symbols)))
}

fn sizeof_result(size: usize) -> TypecheckResult {
    TypecheckResult::from(MIRExpression {
        token_range: None,
        kind: MIRExpressionKind::IntLiteral(size as i64, MIRIntegerType::I64, false),
        _type: MIRType::from(MIRTypeKind::Integer {
            _type: MIRIntegerType::I64,
            signed: false,
        }),
    })
}
