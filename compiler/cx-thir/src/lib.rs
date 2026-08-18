pub use cx_util::namespace::EnvironmentNamespace;

use crate::{
    registry::THIRDecomposedRegistry,
    thir::{
        data::THIRFunction,
        global::MIRGlobalVariable,
    },
};

pub mod intrinsic_types;
pub mod layout;
pub mod registry;
pub mod symbol;
pub mod thir;
pub mod type_context;

mod format;

#[derive(Debug, Clone)]
pub struct THIRUnit {
    pub source_namespace: EnvironmentNamespace,
    pub functions: Vec<THIRFunction>,
    pub global_variables: Vec<MIRGlobalVariable>,
    pub registry: THIRDecomposedRegistry,
}
