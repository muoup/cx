use std::path::PathBuf;
use cx_util::namespace::NamespacePath;

use crate::{mir::{
    data::MIRFunctionPrototype,
    expression::MIRExpression, global::MIRGlobalVariable,
}, registry::MIRDecomposedRegistry};

#[derive(Debug, Clone)]
pub struct MIRUnit {
    pub prototypes: Vec<MIRFunctionPrototype>,
    pub functions: Vec<MIRFunction>,
    pub global_variables: Vec<MIRGlobalVariable>,
    pub registry: MIRDecomposedRegistry,
    pub source_path: PathBuf,
}

pub type EnvironmentNamespace = NamespacePath;

#[derive(Debug, Clone)]
pub struct MIRFunction {
    pub prototype: MIRFunctionPrototype,
    pub body: MIRExpression,
}
