use cx_namespace::module::NamespacePath;

use crate::{
    registry::THIRDecomposedRegistry,
    thir::{
        comptime::THIRComptimeFn,
        data::THIRFunction,
        global::THIRGlobalVariable,
    },
};

pub mod intrinsic_types;
pub mod registry;
pub mod symbol;
pub mod thir;
pub mod type_context;

mod format;

#[derive(Debug, Clone)]
pub struct THIRUnit {
    pub source_namespace: NamespacePath,
    pub registry: THIRDecomposedRegistry,

    pub functions: Vec<THIRFunction>,
    pub comptime_functions: Vec<THIRComptimeFn>,
    pub global_variables: Vec<THIRGlobalVariable>,
}
