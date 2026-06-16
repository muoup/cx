use std::path::PathBuf;

use cx_ast::ast::function::CXFunctionKind;
use cx_util::namespace::NamespacePath;

use crate::{
    mir::{
        data::{MIRFunction, MIRTemplateInput},
        global::MIRGlobalVariable,
    },
    registry::MIRDecomposedRegistry,
};

pub mod intrinsic_types;
pub mod mir;
pub mod registry;
pub mod symbol;
pub mod type_context;

mod format;

pub struct MIRGenerationRequest {
    pub module_origin: Option<String>,
    pub kind: CXFunctionKind,
    pub input: MIRTemplateInput,
}

#[derive(Debug, Clone)]
pub struct MIRUnit {
    pub functions: Vec<MIRFunction>,
    pub global_variables: Vec<MIRGlobalVariable>,
    pub registry: MIRDecomposedRegistry,
    pub source_path: PathBuf,
}

pub type EnvironmentNamespace = NamespacePath;
