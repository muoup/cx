use std::{collections::HashMap, path::PathBuf};
use cx_util::namespace::NamespacePath;

use crate::mir::{
    data::{MIRFunctionPrototype, MIRType, MIRTypeId},
    expression::MIRExpression, global::MIRGlobalVariable,
};

#[derive(Debug, Clone)]
pub struct MIRUnit {
    pub prototypes: Vec<MIRFunctionPrototype>,
    pub functions: Vec<MIRFunction>,
    pub global_variables: Vec<MIRGlobalVariable>,
    pub type_definitions: HashMap<MIRTypeId, MIRType>,
    pub source_path: PathBuf,
}

pub type EnvironmentNamespace = NamespacePath;

#[derive(Debug, Clone)]
pub struct MIRFunction {
    pub prototype: MIRFunctionPrototype,
    pub body: MIRExpression,
}
