pub mod def;
pub mod function;
pub mod global;

use std::collections::HashMap;

use cx_namespace::module::NamespacePath;

use crate::{
    HMIRDefID,
    ty::{HMIRType, HMIRTypeID},
    unit::def::HMIRDef,
};

#[derive(Debug, Clone)]
pub struct HMIRUnit {
    namespace: NamespacePath,

    types: HashMap<HMIRTypeID, HMIRType>,
    defs: HashMap<HMIRDefID, HMIRDef>,
}

impl HMIRUnit {
    pub fn new(
        namespace: NamespacePath,
        types: HashMap<HMIRTypeID, HMIRType>,
        defs: HashMap<HMIRDefID, HMIRDef>,
    ) -> Self {
        Self {
            namespace,
            types,
            defs,
        }
    }

    pub fn namespace(&self) -> &NamespacePath {
        &self.namespace
    }

    pub fn resolve_type(&self, id: HMIRTypeID) -> &HMIRType {
        self.types.get(&id).expect("Type ID not found in unit")
    }

    pub fn types(&self) -> impl Iterator<Item = (&HMIRTypeID, &HMIRType)> {
        self.types.iter()
    }

    pub fn resolve_def(&self, id: HMIRDefID) -> &HMIRDef {
        self.defs.get(&id).expect("Def ID not found in unit")
    }

    pub fn defs(&self) -> impl Iterator<Item = (&HMIRDefID, &HMIRDef)> {
        self.defs.iter()
    }
}
