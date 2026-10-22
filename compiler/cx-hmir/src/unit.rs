pub mod def;
pub mod function;
pub mod global;
pub mod symbols;

use std::collections::HashMap;

use cx_namespace::module::NamespacePath;

use crate::{
    HMIRDefID,
    ty::{HMIRType, HMIRTypeID},
    unit::{def::HMIRDef, symbols::HMIRSymbolTable},
};

#[derive(Debug, Clone)]
pub struct HMIRUnit {
    namespace: NamespacePath,
    symbols: HMIRSymbolTable,

    types: HashMap<HMIRTypeID, HMIRType>,
    defs: HashMap<HMIRDefID, HMIRDef>,
}

impl HMIRUnit {
    pub fn new(
        namespace: NamespacePath,
        symbols: HMIRSymbolTable,
        types: HashMap<HMIRTypeID, HMIRType>,
        defs: HashMap<HMIRDefID, HMIRDef>,
    ) -> Self {
        Self {
            namespace,
            symbols,
            types,
            defs,
        }
    }

    pub fn namespace(&self) -> &NamespacePath {
        &self.namespace
    }

    pub fn symbols(&self) -> &HMIRSymbolTable {
        &self.symbols
    }

    pub fn resolve_type(&self, id: HMIRTypeID) -> &HMIRType {
        self.types.get(&id).expect("Type ID not found in unit")
    }

    pub fn types(&self) -> impl Iterator<Item = (&HMIRTypeID, &HMIRType)> {
        self.types.iter()
    }

    pub fn resolve_def(&self, id: HMIRDefID) -> Option<&HMIRDef> {
        self.defs.get(&id)
    }

    pub fn defs(&self) -> impl Iterator<Item = (&HMIRDefID, &HMIRDef)> {
        self.defs.iter()
    }
}
