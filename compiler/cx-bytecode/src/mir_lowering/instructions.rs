use cx_bytecode_data::{
    types::{BCIntegerType, BCType, BCTypeKind},
    BCGlobalType, BCGlobalValue, BCValue,
};
// NOTE: Old SSA-style MIR types no longer exist - MIR now uses expression trees (MIRExpression)
// This file will be replaced with expression-based lowering in expressions.rs
use cx_typechecker_data::mir::program::{MIRGlobalVarKind, MIRGlobalVariable};
use cx_util::{identifier::CXIdent, CXResult};

use crate::{ builder::BCBuilder};

const LIVENESS_TYPE: BCType = BCType {
    kind: BCTypeKind::Integer(BCIntegerType::I8),
};

// DEPRECATED: Old instruction-based lowering - no longer needed for expression tree MIR
// Use `lower_expression` from expressions.rs instead
#[allow(dead_code)]
#[deprecated(note = "Use lower_expression from expressions.rs instead")]
pub fn lower_instruction(
    builder: &mut BCBuilder,
    instruction: &(), // Placeholder type - MIRInstruction no longer exists
) -> CXResult<BCValue> {
    let _ = builder;
    let _ = instruction;
    todo!("lower_instruction is deprecated - use lower_expression from expressions.rs for expression tree lowering")
}

// DEPRECATED: Old value lowering - MIRValue no longer exists, use MIRExpression instead
#[allow(dead_code)]
#[deprecated(note = "Use lower_expression from expressions.rs instead")]
pub(crate) fn lower_value(builder: &mut BCBuilder, value: &()) -> CXResult<BCValue> {
    let _ = builder;
    let _ = value;
    todo!("lower_value is deprecated - use lower_expression from expressions.rs for expression tree lowering")
}

// Lifetime handling functions - stubbed for now, will be reimplemented for expression trees
// TODO: Implement lifetime handling for expression-based MIR

#[allow(dead_code)]
pub fn handle_lifetime_start(
    _builder: &mut BCBuilder,
    _name: &CXIdent,
    _region: &CXIdent,
    _type: &cx_typechecker_data::mir::types::MIRType,
) -> CXResult<BCValue> {
    // TODO: Implement lifetime start for expression trees
    todo!("Lifetime handling not yet implemented for expression tree MIR")
}

#[allow(dead_code)]
pub fn handle_lifetime_end(
    _builder: &mut BCBuilder,
    _name: &CXIdent,
    _region: &CXIdent,
    _type: &cx_typechecker_data::mir::types::MIRType,
) -> CXResult<BCValue> {
    // TODO: Implement lifetime end for expression trees
    todo!("Lifetime handling not yet implemented for expression tree MIR")
}

#[allow(dead_code)]
pub fn handle_leak_lifetime(_builder: &mut BCBuilder, _region: &CXIdent) -> CXResult<BCValue> {
    // TODO: Implement leak lifetime for expression trees
    todo!("Lifetime handling not yet implemented for expression tree MIR")
}

// Keep lower_global_value as it's still useful for lowering global variables
pub(crate) fn lower_global_value(
    builder: &mut BCBuilder,
    global: &MIRGlobalVariable,
) -> CXResult<BCGlobalValue> {
    let bc_linkage = builder.convert_linkage(global.linkage);

    match &global.kind {
        MIRGlobalVarKind::StringLiteral { name, value } => Ok(BCGlobalValue {
            name: name.clone(),
            _type: BCGlobalType::StringLiteral(value.clone()),
            linkage: bc_linkage,
        }),

        MIRGlobalVarKind::Variable {
            name,
            _type,
            initializer,
        } => {
            let bc_type = builder.convert_cx_type(_type);
            let bc_initializer = *initializer;

            Ok(BCGlobalValue {
                name: name.clone(),
                _type: BCGlobalType::Variable {
                    _type: bc_type,
                    initial_value: bc_initializer,
                },
                linkage: bc_linkage,
            })
        }
    }
}
