use cx_mir::mir::expression::{MIRBinOp, MIRExpression};
use cx_util::CXResult;
use crate::{environment::TypeEnvironment, type_checking::result::TypecheckResult};

pub mod arithmetic;

pub fn typecheck_binop(
    env: &mut TypeEnvironment,
    lhs: MIRExpression,
    rhs: MIRExpression,
    op: MIRBinOp,
) -> CXResult<TypecheckResult> {
    todo!()
}

pub fn typecheck_unop(
    env: &mut TypeEnvironment,
    operand: MIRExpression,
    op: cx_mir::mir::expression::MIRUnOp,
) -> CXResult<TypecheckResult> {
    todo!()
}