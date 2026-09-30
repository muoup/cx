use cx_namespace::module::NamespacePath;

use crate::{
    def::{HMIRDef, HMIRDefID},
    ty::interner::HMIRTypeInterner,
};

#[derive(Debug, Clone)]
pub struct HMIRUnit {
    namespace: NamespacePath,
    defs: Vec<HMIRDef>,
    types: HMIRTypeInterner,
}

impl HMIRUnit {
    pub fn new(namespace: NamespacePath) -> Self {
        Self {
            namespace,
            defs: Vec::new(),
            types: HMIRTypeInterner::default(),
        }
    }

    pub fn namespace(&self) -> &NamespacePath {
        &self.namespace
    }

    pub fn push_def(&mut self, def: HMIRDef) -> HMIRDefID {
        self.defs.push(def);
        HMIRDefID::new(self.defs.len() - 1)
    }

    pub fn def(&self, id: HMIRDefID) -> &HMIRDef {
        &self.defs[id.index()]
    }

    pub fn def_mut(&mut self, id: HMIRDefID) -> &mut HMIRDef {
        &mut self.defs[id.index()]
    }

    pub fn defs(&self) -> impl Iterator<Item = (HMIRDefID, &HMIRDef)> {
        self.defs
            .iter()
            .enumerate()
            .map(|(index, def)| (HMIRDefID::new(index), def))
    }

    pub fn types(&self) -> &HMIRTypeInterner {
        &self.types
    }

    pub fn types_mut(&mut self) -> &mut HMIRTypeInterner {
        &mut self.types
    }
}
