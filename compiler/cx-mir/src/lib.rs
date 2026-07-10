use cx_ast::ast::function::CXFunctionKind;
pub use cx_util::namespace::EnvironmentNamespace;

use crate::{
    mir::{
        data::{MIRFunction, MIRTemplateInput},
        global::MIRGlobalVariable,
    },
    registry::MIRDecomposedRegistry,
};

pub mod architecture;
pub mod intrinsic_types;
pub mod layout;
pub mod mir;
pub mod registry;
pub mod symbol;
pub mod type_context;

pub use architecture::ArchitectureConfig;

mod format;

pub struct MIRGenerationRequest {
    pub module_origin: Option<String>,
    pub kind: CXFunctionKind,
    pub input: MIRTemplateInput,
}

#[derive(Debug, Clone)]
pub struct MIRUnit {
    pub source_namespace: EnvironmentNamespace,
    pub functions: Vec<MIRFunction>,
    pub global_variables: Vec<MIRGlobalVariable>,
    pub registry: MIRDecomposedRegistry,
}
