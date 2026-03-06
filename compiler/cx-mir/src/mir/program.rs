use std::collections::HashMap;

use cx_ast::{
    ast::CXGlobalVariable,
    data::{CXLinkageMode, ModuleResource},
    type_map::{CXFnMap, CXTypeMap},
};
use cx_util::identifier::CXIdent;

use crate::mir::{
    expression::MIRExpression,
    types::{MIRFunctionPrototype, MIRType},
};

#[derive(Debug, Clone)]
pub struct MIRUnit {
    pub prototypes: Vec<MIRFunctionPrototype>,
    pub functions: Vec<MIRFunction>,
    pub global_variables: Vec<MIRGlobalVariable>,
}

#[derive(Debug, Clone)]
pub struct MIRBaseMappings {
    pub unit: String,
    pub type_data: CXTypeMap,
    pub fn_data: CXFnMap,
    pub global_variables: HashMap<String, ModuleResource<CXGlobalVariable>>,
}

#[derive(Debug, Clone)]
pub struct MIRFunction {
    pub prototype: MIRFunctionPrototype,
    pub body: MIRExpression,
}

#[derive(Debug, Clone)]
pub struct MIRGlobalVariable {
    pub kind: MIRGlobalVarKind,
    pub is_mutable: bool,
    pub linkage: CXLinkageMode,
}

#[derive(Debug, Clone)]
pub enum MIRGlobalVarKind {
    StringLiteral {
        name: CXIdent,
        value: String,
    },
    Variable {
        name: CXIdent,
        _type: MIRType,
        initializer: Option<i64>,
    },
}
