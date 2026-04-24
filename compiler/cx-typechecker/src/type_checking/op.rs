use crate::{environment::TypeEnvironment, type_checking::result::TypecheckResult};
use cx_ast::ast::{CXBinOp, CXExpression, CXUnOp};
use cx_mir::mir::{expression::MIRExpression, program::MIRBaseMappings};
use cx_util::CXResult;

pub mod binop;
pub mod unop;

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
    base_data: &MIRBaseMappings,
    op: &CXUnOp,
    operand: &CXExpression,
) -> CXResult<TypecheckResult> {
    unop::dispatch(env, base_data, op, operand)
}
