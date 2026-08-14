use cx_hir::ast::function::HIRFunctionKind;
pub use cx_util::namespace::EnvironmentNamespace;

use crate::{
    thir::{
        data::{THIRFunction, MIRTemplateInput},
        global::MIRGlobalVariable,
    },
    registry::THIRDecomposedRegistry,
};

pub mod intrinsic_types;
pub mod layout;
pub mod thir;
pub mod registry;
pub mod symbol;
pub mod type_context;

mod format;

pub struct MIRGenerationRequest {
    pub module_origin: Option<String>,
    pub kind: HIRFunctionKind,
    pub input: MIRTemplateInput,
}

#[derive(Debug, Clone)]
pub struct THIRUnit {
    pub source_namespace: EnvironmentNamespace,
    pub functions: Vec<THIRFunction>,
    pub global_variables: Vec<MIRGlobalVariable>,
    pub registry: THIRDecomposedRegistry,
}
