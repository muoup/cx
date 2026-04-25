use cx_mir::mir::expression::{MIRBinOp, MIRExpression, MIRExpressionKind, MIRIntegerBinOp};
use cx_util::CXResult;

use crate::{environment::TypeEnvironment, log_typecheck_error};

pub enum ConstexprResult {
    Integer(u64),
}

// FIXME: Currently we are going to be using a very oversimplified evaluation engine where all integers are handled as u64,
// this could cause some very subtle parity problems, however the cases where this would happen are very slim, but this should
// be addressed in the future.

pub fn constexpr_evaluate(
    env: &mut TypeEnvironment,
    expr: MIRExpression,
) -> CXResult<ConstexprResult> {
    Ok(match expr.kind {
        MIRExpressionKind::IntLiteral(val, _type, _) => ConstexprResult::Integer(val as u64),

        MIRExpressionKind::BinaryOperation {
            lhs,
            rhs,
            op: MIRBinOp::Integer { itype: _, op }
        } => {
            let Some(lhs) = constexpr_evaluate(env, *lhs)?
                .get_integer() else { unreachable!() };
            let Some(rhs) = constexpr_evaluate(env, *rhs)?
                .get_integer() else { unreachable!() };
            
            ConstexprResult::Integer(match op {
                MIRIntegerBinOp::ADD => lhs + rhs,
                MIRIntegerBinOp::SUB => lhs + rhs,
                MIRIntegerBinOp::MUL => lhs + rhs,
                MIRIntegerBinOp::DIV => lhs + rhs,
                
                _ => todo!()
            })
        },
        
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
    pub fn get_integer(self) -> Option<u64> {
        match self {
            ConstexprResult::Integer(i) => Some(i),
        }
    }
}
