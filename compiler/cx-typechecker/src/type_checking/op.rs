use crate::{environment::TypeEnvironment, type_checking::result::TypecheckResult};
use cx_ast::ast::expression::CXBinOp;
use cx_log::CXResult;
use cx_mir::mir::expression::MIRExpression;

pub use unop::typecheck_unop;

pub mod binop;
pub mod unop;

pub fn typecheck_binop(
    env: &mut TypeEnvironment,
    op: &CXBinOp,
    lhs: MIRExpression,
    rhs: MIRExpression,
) -> CXResult<TypecheckResult> {
    binop::dispatch(env, op, lhs, rhs)
}
