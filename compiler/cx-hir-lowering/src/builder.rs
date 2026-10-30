use rustc_hash::FxHashMap;

use cx_hmir::{
    HMIRDeclaration, HMIRDef, HMIRDefID, HMIRUnit, HMIRUnitID, unit::symbols::HMIRSymbolTable,
};
use cx_namespace::module::NamespacePath;

pub(crate) struct HMIRUnitBuilder {
    id: HMIRUnitID,
    namespace: NamespacePath,
    defs: FxHashMap<HMIRDefID, HMIRDef>,
}

impl HMIRUnitBuilder {
    pub(crate) fn new(id: HMIRUnitID, namespace: NamespacePath) -> Self {
        Self {
            id,
            namespace,
            defs: FxHashMap::default(),
        }
    }

    pub(crate) fn push_def(&mut self, def: HMIRDef) -> HMIRDefID {
        let id = HMIRDefID::new(self.id, self.defs.len());
        self.defs.insert(id, def);
        id
    }

    pub(crate) fn extend_def(&mut self, id: HMIRDefID, declarations: Vec<HMIRDeclaration>) {
        self.defs
            .get_mut(&id)
            .expect("definition was lowered")
            .extend(declarations);
    }

    pub(crate) fn finish(self, symbols: HMIRSymbolTable) -> HMIRUnit {
        HMIRUnit::new(self.id, self.namespace, symbols, self.defs)
    }
}
