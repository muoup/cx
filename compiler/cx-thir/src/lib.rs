pub use cx_util::namespace::EnvironmentNamespace;

use crate::{
    registry::THIRDecomposedRegistry,
    thir::{
        data::{THIRComptimeFnPrototype, THIRFunction},
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
    pub source_naamespace: EnvironmentNamespace,
    pub registry: THIRDecomposedRegistry,

    pub functions: Vec<THIRFunction>,
    pub comptime_functions: Vec<THIRComptimeFnPrototype>,
    pub global_variables: Vec<THIRGlobalVariable>,
}
