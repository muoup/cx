use cx_ast::ast::{CXExpression, CXUnOp};
use cx_mir::mir::{
    expression::{MIRCoercion, MIRExpression, MIRExpressionKind, MIRUnOp},
    program::MIRBaseMappings,
    r#type::{MIRIntegerType, MIRType, MIRTypeKind},
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
        result::TypecheckResult,
        typechecker::typecheck_expr,
    },
};

pub fn dispatch(
    env: &mut TypeEnvironment,
    base_data: &MIRBaseMappings,
    op: &CXUnOp,
    operand: &CXExpression,
) -> CXResult<TypecheckResult> {
    Ok(match op {
        CXUnOp::PreIncrement(increment_amount) | CXUnOp::PostIncrement(increment_amount) => {
            let operand = typecheck_expr(env, base_data, operand, None)?.into_expression();

            let Some(inner) = env.symbols.context.mem_ref_inner(&operand._type).cloned() else {
                return log_typecheck_error!(
                    env,
                    operand.token_range.as_ref(),
                    "Cannot apply pre-increment to non-reference type {}",
                    operand._type.display_with(&env.symbols.context)
                );
            };

            match &inner.kind {
                MIRTypeKind::PointerTo { .. } | MIRTypeKind::Integer { .. } => match op {
                    CXUnOp::PreIncrement(_) => TypecheckResult::new_base(
                        operand._type.clone(),
                        MIRExpressionKind::UnaryOperation {
                            op: MIRUnOp::PreIncrement(*increment_amount),
                            operand: Box::new(operand),
                        },
                    ),
                    CXUnOp::PostIncrement(_) => TypecheckResult::new_base(
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
                        inner.display_with(&env.symbols.context)
                    );
                }
            }
        }

        CXUnOp::LNot => {
            let operand = typecheck_expr(env, base_data, operand, None)
                .and_then(|v| std_rval_promotion(env, v.into_expression()))
                .and_then(|v| implicit_cast(env, v, &MIRType::bool()))?;

            TypecheckResult::new_base(
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
            let operand = typecheck_expr(env, base_data, operand, None)
                .and_then(|v| std_rval_promotion(env, v.into_expression()))?;

            if !operand._type.is_integer() {
                return log_typecheck_error!(
                    env,
                    operand.token_range.as_ref(),
                    "Bitwise NOT operator requires an integer type, found {}",
                    operand._type.display_with(&env.symbols.context)
                );
            }

            TypecheckResult::new_base(
                operand._type.clone(),
                MIRExpressionKind::UnaryOperation {
                    operand: Box::new(operand),
                    op: MIRUnOp::BNOT,
                },
            )
        }

        CXUnOp::Negative => {
            let operand = typecheck_expr(env, base_data, operand, None)
                .and_then(|v| std_rval_promotion(env, v.into_expression()))?;

            let operator = match &operand._type.kind {
                MIRTypeKind::Integer { .. } => MIRUnOp::NEG,
                MIRTypeKind::Float { .. } => MIRUnOp::FNEG,

                _ => {
                    return log_typecheck_error!(
                        env,
                        operand.token_range.as_ref(),
                        "Negation operator requires an integer or float type, found {}",
                        operand.display_with(&env.symbols.context)
                    );
                }
            };

            TypecheckResult::new_base(
                operand._type.clone(),
                MIRExpressionKind::UnaryOperation {
                    operand: Box::new(operand),
                    op: operator,
                },
            )
        }

        CXUnOp::AddressOf => {
            let operand = typecheck_expr(env, base_data, operand, None)?.into_expression();

            let Some(inner) = env.symbols.context.mem_ref_inner(&operand._type).cloned() else {
                return log_typecheck_error!(
                    env,
                    operand.token_range.as_ref(),
                    "Cannot take the address of a non-reference type"
                );
            };

            // AddressOf just returns the operand (which is a reference) as a pointer
            TypecheckResult::from(MIRExpression {
                token_range: operand.token_range.clone(),
                _type: env.symbols.context.pointer_to(inner.clone()),
                kind: MIRExpressionKind::TypeConversion {
                    operand: Box::new(operand),
                    conversion: MIRCoercion::ReinterpretBits,
                },
            })
        }

        CXUnOp::Dereference => {
            let operand = typecheck_expr(env, base_data, operand, None)
                .and_then(|v| std_rval_promotion(env, v.into_expression()))?;

            let Some(inner) = env.symbols.context.ptr_inner(&operand._type).cloned() else {
                return log_typecheck_error!(
                    env,
                    operand.token_range.as_ref(),
                    "Cannot dereference non-pointer type {}",
                    operand._type.display_with(&env.symbols.context)
                );
            };

            // Dereference returns a memory reference to the inner type
            TypecheckResult::from(MIRExpression {
                token_range: None,
                kind: MIRExpressionKind::Typechange(Box::new(operand)),
                _type: env.symbols.context.mem_ref_to(inner),
            })
        }

        CXUnOp::ExplicitCast(to_type) => {
            let to_type = env.complete_type(base_data, &CXExpression::default(), to_type)?;

            let operand = typecheck_expr(env, base_data, operand, Some(&to_type))
                .and_then(|v| std_rval_promotion(env, v.into_expression()))?;

            TypecheckResult::from(explicit_cast(env, operand, &to_type)?)
        }
    })
}
