use std::collections::HashMap;

use cx_parsing_data::{ast::CXGlobalVariable, data::{CXLinkageMode, ModuleResource}, naive_map::{CXNaiveFnMap, CXNaiveTypeMap}};
use cx_util::identifier::CXIdent;

use crate::mir::{expression::MIRInstruction, types::{CXFunctionPrototype, CXType}};

#[derive(Debug, Clone)]
pub struct MIRUnit {
    pub prototypes: Vec<CXFunctionPrototype>,
    pub functions: Vec<MIRFunction>
}

#[derive(Debug, Clone)]
pub struct MIRBaseMappings {
    pub unit: String,
    pub type_data: CXNaiveTypeMap,
    pub fn_data: CXNaiveFnMap,
    pub global_variables: HashMap<String, ModuleResource<CXGlobalVariable>>,
}
    
#[derive(Debug, Clone)]
pub struct MIRFunction {
    pub prototype: CXFunctionPrototype,
    pub basic_blocks: Vec<MIRBasicBlock>,
}

#[derive(Debug, Clone)]
pub struct MIRBasicBlock {
    pub id: CXIdent,
    pub instructions: Vec<MIRInstruction>,
}

#[derive(Debug, Clone)]
pub struct MIRGlobalVariable {
    pub kind: MIRGlobalVarKind,
    pub is_mutable: bool,
    pub linkage: CXLinkageMode
}

#[derive(Debug, Clone)]
pub enum MIRGlobalVarKind {
    StringLiteral {
        name: CXIdent,
        value: String,
    },
    Variable {
        name: CXIdent,
        _type: CXType,
        initializer: Option<i64>,
    },   
}