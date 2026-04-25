use cx_mir::mir::expression::MIRExpression;
use cx_util::CXResult;

use crate::{environment::TypeEnvironment, log_typecheck_error};

pub enum ConstexprResult {
    Integer(u64)
}

pub fn constexpr_evaluate(
    env: &mut TypeEnvironment, 
    expr: MIRExpression
) -> CXResult<ConstexprResult> {
    match expr {
        _ => log_typecheck_error!(
            env,
            expr.token_range.as_ref(),
            "Invalid expression in constexpr context"
        )
    }
}

impl ConstexprResult {
    pub fn get_integer(self) -> Option<u64> {
        match self {
            ConstexprResult::Integer(i) => Some(i),
        }
    }
}