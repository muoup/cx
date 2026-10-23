use std::collections::HashMap;

use cx_hmir::{HMIRDef, HMIRDefID, HMIRUnit, HMIRUnitID, unit::symbols::HMIRSymbolTable};
use cx_namespace::module::NamespacePath;

pub(crate) struct HMIRUnitBuilder {
    id: HMIRUnitID,
    namespace: NamespacePath,
    defs: HashMap<HMIRDefID, HMIRDef>,
}

impl HMIRUnitBuilder {
    pub(crate) fn new(id: HMIRUnitID, namespace: NamespacePath) -> Self {
        Self {
            id,
            namespace,
            defs: HashMap::new(),
        }
    }

    pub(crate) fn push_def(&mut self, def: HMIRDef) -> HMIRDefID {
        let id = HMIRDefID::new(self.id, self.defs.len());
        self.defs.insert(id, def);
        id
    }

    pub(crate) fn finish(self, symbols: HMIRSymbolTable) -> HMIRUnit {
        HMIRUnit::new(self.id, self.namespace, symbols, self.defs)
    }
}
