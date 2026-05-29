use std::path::PathBuf;

use cx_ast::ast::modifiers::CXLinkageMode;
use cx_util::{identifier::CXIdent, namespace::NamespacePath};

use crate::mir::{
    data::{MIRFunctionPrototype, MIRType, MIRTypeContext},
    expression::MIRExpression,
};

#[derive(Debug, Clone)]
pub struct MIRUnit {
    pub prototypes: Vec<MIRFunctionPrototype>,
    pub functions: Vec<MIRFunction>,
    pub global_variables: Vec<MIRGlobalVariable>,
    pub type_definitions: MIRTypeContext,
    pub source_path: PathBuf,
}

// #[derive(Debug, Clone)]
// pub struct EnvironmentNamespace {
//     pub unit: String,
//     pub namespace: NamespacePath,
//     pub type_data: CXTypeMap,
//     pub fn_data: CXFnMap,
//     pub global_variables: HashMap<String, ModuleResource<CXGlobalVariable>>,
// }

pub type EnvironmentNamespace = NamespacePath;

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
