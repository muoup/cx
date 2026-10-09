pub mod def;
pub mod function;
pub mod global;

use cx_namespace::module::NamespacePath;

use crate::unit::def::HMIRDef;

#[derive(Debug, Clone)]
pub struct HMIRUnit {
    namespace: NamespacePath,
    defs: Vec<HMIRDef>,
}
