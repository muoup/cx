use cx_bytecode_data::{BCFunctionPrototype, BCValue};
use cx_typechecker_data::mir::{
    expression::{MIRBinOp, MIRExpression},
    types::MIRType,
};
use cx_util::CXResult;

use crate::builder::BCBuilder;

// New expression-based lowering - to be implemented in expressions.rs
#[allow(dead_code)]
pub fn lower_expression_binop(
    builder: &mut BCBuilder,
    lhs: &MIRExpression,
    rhs: &MIRExpression,
    op: &MIRBinOp,
    result_type: &MIRType,
) -> CXResult<BCValue> {
    let _ = builder;
    let _ = lhs;
    let _ = rhs;
    let _ = op;
    let _ = result_type;
    todo!("Expression-based binop lowering not yet implemented - see expressions.rs")
}

#[allow(dead_code)]
pub fn lower_expression_call_params(
    builder: &mut BCBuilder,
    params: &[MIRExpression],
    prototype: &BCFunctionPrototype,
) -> CXResult<Vec<BCValue>> {
    let _ = builder;
    let _ = params;
    let _ = prototype;
    todo!("Expression-based call params lowering not yet implemented - see expressions.rs")
}
