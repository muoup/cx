use cx_bytecode_data::{
    types::{BCType, BCTypeKind},
    BCCoercionType, BCInstructionKind, BCValue,
};
use cx_typechecker_data::mir::expression::{MIRCoercion, MIRExpression};
use cx_util::CXResult;

use crate::{builder::BCBuilder, mir_lowering::instructions::lower_value};

// DEPRECATED: Old value-based lowering - no longer needed for expression tree MIR
// Use `lower_expression` from expressions.rs instead
#[allow(dead_code)]
#[deprecated(note = "Use lower_expression from expressions.rs instead")]
pub fn lower_coercion(
    builder: &mut BCBuilder,
    result: (), // MIRRegister no longer exists
    value: &(), // MIRValue no longer exists
    coercion_type: MIRCoercion,
) -> CXResult<BCValue> {
    let _ = builder;
    let _ = result;
    let _ = value;
    let _ = coercion_type;
    todo!("lower_coercion is deprecated - use lower_expression from expressions.rs for expression tree lowering")
}

// New expression-based lowering - to be implemented in expressions.rs
#[allow(dead_code)]
pub fn lower_expression_coercion(
    builder: &mut BCBuilder,
    value: &MIRExpression,
    coercion_type: MIRCoercion,
    result_type: &cx_typechecker_data::mir::types::MIRType,
) -> CXResult<BCValue> {
    let _ = builder;
    let _ = value;
    let _ = coercion_type;
    let _ = result_type;
    todo!("Expression-based coercion lowering not yet implemented - see expressions.rs")
}
