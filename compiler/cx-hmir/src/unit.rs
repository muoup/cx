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

    externs: HashMap<NamespacePath, Vec<NamespacePath>>,
    symbols: HashMap<String, HMIRDefID>,
    
    types: HashMap<HMIRTypeID, HMIRType>,
    defs: HashMap<HMIRDefID, HMIRDef>,
}

impl HMIRUnit {
    pub fn new(
        namespace: NamespacePath,
        externs: HashMap<NamespacePath, Vec<NamespacePath>>,
        symbols: HashMap<String, HMIRDefID>,
        types: HashMap<HMIRTypeID, HMIRType>,
        defs: HashMap<HMIRDefID, HMIRDef>,
    ) -> Self {
        Self {
            namespace,
            symbols,
            externs,
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

    pub fn find_symbol(&self, name: &str) -> Option<HMIRDefID> {
        self.symbols.get(name).copied()
    }

    pub fn resolve_def(&self, id: HMIRDefID) -> Option<&HMIRDef> {
        self.defs.get(&id)
    }

    pub fn defs(&self) -> impl Iterator<Item = (&HMIRDefID, &HMIRDef)> {
        self.defs.iter()
    }

    pub fn resolve_extern(&self, namespace: &NamespacePath) -> &[NamespacePath] {
        self.externs.get(namespace)
            .map(|v| v.as_slice())
            .unwrap_or(&[])
    }

    pub fn externs(&self) -> impl Iterator<Item = (&NamespacePath, &Vec<NamespacePath>)> {
        self.externs.iter()
    }
}
