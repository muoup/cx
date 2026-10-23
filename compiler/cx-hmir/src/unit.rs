pub mod def;
pub mod function;
pub mod global;
pub mod id;
pub mod symbols;

use std::collections::HashMap;

use cx_namespace::module::NamespacePath;

use crate::{
    HMIRDefID,
    ty::{HMIRType, HMIRTypeID},
    unit::{def::HMIRDef, id::HMIRUnitID, symbols::HMIRSymbolTable},
};

#[derive(Debug, Clone)]
pub struct HMIRUnit {
    id: HMIRUnitID,
    namespace: NamespacePath,
    symbols: HMIRSymbolTable,

    types: HashMap<HMIRTypeID, HMIRType>,
    defs: HashMap<HMIRDefID, HMIRDef>,
}

impl HMIRUnit {
    pub fn new(
        id: HMIRUnitID,
        namespace: NamespacePath,
        symbols: HMIRSymbolTable,
        types: HashMap<HMIRTypeID, HMIRType>,
        defs: HashMap<HMIRDefID, HMIRDef>,
    ) -> Self {
        Self {
            id,
            namespace,
            symbols,
            types,
            defs,
        }
    }

    pub fn id(&self) -> HMIRUnitID {
        self.id
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
        if id.brand() != self.id {
            return None;
        }
        self.defs.get(&id)
    }

    pub fn defs(&self) -> impl Iterator<Item = (&HMIRDefID, &HMIRDef)> {
        self.defs.iter()
    }
}
