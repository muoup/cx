mod log;
use cx_namespace::module::NamespacePath;

use crate::{
    registry::THIRDecomposedRegistry,
    thir::{comptime::THIRComptimeFn, data::THIRFunction, global::THIRGlobalVariable},
};

pub mod intrinsic_types;
pub mod registry;
pub mod symbol;
pub mod thir;
pub mod type_context;

mod format;

#[derive(Debug, Clone)]
pub struct THIRUnit {
    source_namespace: NamespacePath,
    registry: THIRDecomposedRegistry,

    functions: Vec<THIRFunction>,
    comptime_functions: Vec<THIRComptimeFn>,
    global_variables: Vec<THIRGlobalVariable>,
}

impl THIRUnit {
    pub fn new(
        source_namespace: NamespacePath,
        registry: THIRDecomposedRegistry,
        functions: Vec<THIRFunction>,
        comptime_functions: Vec<THIRComptimeFn>,
        global_variables: Vec<THIRGlobalVariable>,
    ) -> Self {
        Self {
            source_namespace,
            registry,
            functions,
            comptime_functions,
            global_variables,
        }
    }

    pub fn source_namespace(&self) -> &NamespacePath {
        &self.source_namespace
    }

    pub fn registry(&self) -> &THIRDecomposedRegistry {
        &self.registry
    }

    pub fn functions(&self) -> &[THIRFunction] {
        &self.functions
    }

    pub fn comptime_functions(&self) -> &[THIRComptimeFn] {
        &self.comptime_functions
    }

    pub fn global_variables(&self) -> &[THIRGlobalVariable] {
        &self.global_variables
    }
}
