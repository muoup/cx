use cx_ast::ast::CXBinOp;
use cx_mir::mir::expression::{MIRExpression, MIRUnOp};
use cx_util::CXResult;
use crate::{environment::TypeEnvironment, type_checking::result::TypecheckResult};

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
    env: &mut TypeEnvironment,
    op: MIRUnOp,
    operand: MIRExpression,
) -> CXResult<TypecheckResult> {
    todo!()
}