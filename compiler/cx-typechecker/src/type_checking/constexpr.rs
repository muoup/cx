use cx_mir::mir::expression::{
    MIRBinOp, MIRCoercion, MIRExpression, MIRExpressionKind, MIRIntegerBinOp,
};
use cx_util::CXResult;

use crate::{environment::TypeEnvironment, log_typecheck_error};

pub enum ConstexprResult {
    Integer(i64),
}

// FIXME: We currently use a very oversimplified evaluation engine where all integers are handled as i64,
// this could cause some very subtle parity problems, however the edge cases this creates seem pretty unlikely for common use.
// Regardless, this should be addressed in the future.

pub fn constexpr_evaluate(
    env: &mut TypeEnvironment,
    expr: MIRExpression,
) -> CXResult<ConstexprResult> {
    Ok(match expr.kind {
        MIRExpressionKind::IntLiteral(val, _type, _) => ConstexprResult::Integer(val),

        MIRExpressionKind::BinaryOperation {
            lhs,
            rhs,
            op: MIRBinOp::Integer { itype: _, op },
        } => {
            let Some(lhs) = constexpr_evaluate(env, *lhs)?.get_integer() else {
                unreachable!()
            };
            let Some(rhs) = constexpr_evaluate(env, *rhs)?.get_integer() else {
                unreachable!()
            };

            ConstexprResult::Integer(match op {
                MIRIntegerBinOp::ADD => lhs + rhs,
                MIRIntegerBinOp::SUB => lhs - rhs,
                MIRIntegerBinOp::MUL | MIRIntegerBinOp::IMUL => lhs * rhs,
                MIRIntegerBinOp::DIV | MIRIntegerBinOp::IDIV => lhs / rhs,
                MIRIntegerBinOp::MOD | MIRIntegerBinOp::IMOD => lhs % rhs,
                MIRIntegerBinOp::BAND => lhs & rhs,
                MIRIntegerBinOp::BOR => lhs | rhs,
                MIRIntegerBinOp::BXOR => lhs ^ rhs,
                MIRIntegerBinOp::SHL => lhs << rhs,
                MIRIntegerBinOp::ASHR | MIRIntegerBinOp::LSHR => lhs >> rhs,
                MIRIntegerBinOp::EQ => i64::from(lhs == rhs),
                MIRIntegerBinOp::NE => i64::from(lhs != rhs),
                MIRIntegerBinOp::LT | MIRIntegerBinOp::ILT => i64::from(lhs < rhs),
                MIRIntegerBinOp::LE | MIRIntegerBinOp::ILE => i64::from(lhs <= rhs),
                MIRIntegerBinOp::GT | MIRIntegerBinOp::IGT => i64::from(lhs > rhs),
                MIRIntegerBinOp::GE | MIRIntegerBinOp::IGE => i64::from(lhs >= rhs),
                MIRIntegerBinOp::LAND => i64::from(lhs != 0 && rhs != 0),
                MIRIntegerBinOp::LOR => i64::from(lhs != 0 || rhs != 0),
            })
        }

        MIRExpressionKind::UnaryOperation { operand, op } => {
            let Some(operand) = constexpr_evaluate(env, *operand)?.get_integer() else {
                unreachable!()
            };

            ConstexprResult::Integer(match op {
                cx_mir::mir::expression::MIRUnOp::NEG | cx_mir::mir::expression::MIRUnOp::INEG => {
                    -operand
                }
                cx_mir::mir::expression::MIRUnOp::BNOT => !operand,
                cx_mir::mir::expression::MIRUnOp::LNOT => i64::from(operand == 0),
                _ => {
                    return log_typecheck_error!(
                        env,
                        expr.token_range.as_ref(),
                        "Invalid unary expression in constexpr context"
                    );
                }
            })
        }

        MIRExpressionKind::TypeConversion {
            operand,
            conversion,
        } => match conversion {
            MIRCoercion::Integral { .. } => constexpr_evaluate(env, *operand)?,

            _ => todo!(),
        },

        MIRExpressionKind::If {
            condition,
            then_branch,
            else_branch,
        } => {
            let Some(condition) = constexpr_evaluate(env, *condition)?.get_integer() else {
                unreachable!()
            };

            if condition != 0 {
                constexpr_evaluate(env, *then_branch)?
            } else if let Some(else_branch) = else_branch {
                constexpr_evaluate(env, *else_branch)?
            } else {
                return log_typecheck_error!(
                    env,
                    expr.token_range.as_ref(),
                    "Invalid conditional expression in constexpr context"
                );
            }
        }

        _ => {
            return log_typecheck_error!(
                env,
                expr.token_range.as_ref(),
                "Invalid expression in constexpr context"
            );
        }
    })
}

impl ConstexprResult {
    pub fn get_integer(self) -> Option<i64> {
        match self {
            ConstexprResult::Integer(i) => Some(i),
        }
    }
}
