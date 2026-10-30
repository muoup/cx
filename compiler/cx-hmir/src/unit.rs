pub mod def;
pub mod function;
pub mod global;
pub mod id;
pub mod symbols;

use rustc_hash::FxHashMap;

use cx_namespace::module::NamespacePath;

use crate::{
    HMIRDefID,
    body::{HMIRBody, HMIRBodyID},
    unit::{def::HMIRDef, id::HMIRUnitID, symbols::HMIRSymbolTable},
};

#[derive(Debug, Clone)]
pub struct HMIRUnit {
    id: HMIRUnitID,
    namespace: NamespacePath,
    symbols: HMIRSymbolTable,

    defs: FxHashMap<HMIRDefID, HMIRDef>,
    bodies: FxHashMap<HMIRBodyID, (HMIRDefID, usize, bool)>,
}

impl HMIRUnit {
    pub fn new(
        id: HMIRUnitID,
        namespace: NamespacePath,
        symbols: HMIRSymbolTable,
        defs: FxHashMap<HMIRDefID, HMIRDef>,
    ) -> Self {
        let mut bodies = FxHashMap::default();
        for (def_id, def) in &defs {
            for (declaration_index, declaration) in def.declarations().iter().enumerate() {
                match declaration.kind() {
                    Some(def::HMIRDefKind::Function(function)) => {
                        bodies.insert(function.body().id(), (*def_id, declaration_index, false));
                        bodies.insert(
                            function.signature().contract().body().id(),
                            (*def_id, declaration_index, true),
                        );
                    }
                    Some(def::HMIRDefKind::Global(global)) => {
                        bodies.insert(global.body().id(), (*def_id, declaration_index, false));
                    }
                    Some(def::HMIRDefKind::ComptimeGlobal(global)) => {
                        bodies.insert(global.body().id(), (*def_id, declaration_index, false));
                    }
                    None => {}
                }
            }
        }

        Self {
            id,
            namespace,
            symbols,
            defs,
            bodies,
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

    pub fn body(&self, id: HMIRBodyID) -> Option<&HMIRBody> {
        let (def_id, declaration_index, contract) = self.bodies.get(&id)?;
        let declaration = self
            .defs
            .get(def_id)?
            .declarations()
            .get(*declaration_index)?;

        match declaration.kind()? {
            def::HMIRDefKind::Function(function) if *contract => {
                Some(function.signature().contract().body())
            }
            def::HMIRDefKind::Function(function) => Some(function.body()),
            def::HMIRDefKind::Global(global) => Some(global.body()),
            def::HMIRDefKind::ComptimeGlobal(global) => Some(global.body()),
        }
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
