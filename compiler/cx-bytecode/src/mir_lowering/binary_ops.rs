use cx_bytecode_data::{BCFunctionPrototype, BCValue};
use cx_typechecker_data::mir::{
    expression::{MIRBinOp, MIRExpression},
    types::MIRType,
};
use cx_util::CXResult;

use crate::builder::BCBuilder;

// DEPRECATED: Old value-based lowering - no longer needed for expression tree MIR
// Use `lower_expression` from expressions.rs instead
#[allow(dead_code)]
#[deprecated(note = "Use lower_expression from expressions.rs instead")]
pub(crate) fn lower_binop(
    builder: &mut BCBuilder,
    result: &(), // MIRRegister no longer exists
    op: &MIRBinOp,
    lhs: &(), // MIRValue no longer exists
    rhs: &(), // MIRValue no longer exists
) -> CXResult<BCValue> {
    let _ = builder;
    let _ = result;
    let _ = op;
    let _ = lhs;
    let _ = rhs;
    todo!("lower_binop is deprecated - use lower_expression from expressions.rs for expression tree lowering")
}

// DEPRECATED: Old value-based lowering - no longer needed for expression tree MIR
#[allow(dead_code)]
pub fn lower_call_params(
    builder: &mut BCBuilder,
    params: &[()], // MIRValue no longer exists
    prototype: &BCFunctionPrototype,
) -> CXResult<Vec<BCValue>> {
    let _ = builder;
    let _ = params;
    let _ = prototype;
    todo!("lower_call_params is deprecated - use lower_expression from expressions.rs for expression tree lowering")
}

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
