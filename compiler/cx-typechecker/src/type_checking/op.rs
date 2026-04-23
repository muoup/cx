use crate::{environment::TypeEnvironment, type_checking::result::TypecheckResult};
use cx_ast::ast::CXBinOp;
use cx_mir::mir::expression::{MIRExpression, MIRUnOp};
use cx_util::CXResult;

pub mod binop;

pub fn typecheck_binop(
    env: &mut TypeEnvironment,
    op: &CXBinOp,
    lhs: MIRExpression,
    rhs: MIRExpression,
) -> CXResult<TypecheckResult> {
    binop::dispatch(env, &op, lhs, rhs)
}

pub fn typecheck_unop(
    _env: &mut TypeEnvironment,
    _op: MIRUnOp,
    _operand: MIRExpression,
) -> CXResult<TypecheckResult> {
    todo!()
}
