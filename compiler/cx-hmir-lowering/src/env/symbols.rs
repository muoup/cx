use std::collections::HashMap;

use cx_hmir::{HMIRDef, HMIRDefID, HMIRUnit};
use cx_namespace::module::QualifiedName;
use cx_pipeline_data::db::ModuleMap;

///
/// Handles accessing the HMIR environment. An HMIR environment contains both the HMIR module itself, as well as
/// public interfaces of imported modules. For things like comptime functions, the symbol environment is used as
/// a lazy loader that handles caching and loading such that external symbols can be used as if they were local.
///
#[derive(Debug)]
pub struct HMIRSymbolEnv<'global, 'hmir> {
    module_map: &'global ModuleMap<HMIRUnit>,
    local_unit: &'hmir HMIRUnit,

    external_defs: HashMap<QualifiedName, HMIRDefID>,
    external_cache: HashMap<HMIRDefID, &'global HMIRDef>,
    external_def_counter: usize,
}

impl<'global, 'hmir> HMIRSymbolEnv<'global, 'hmir> {
    fn allocate_external_id(&self) -> HMIRDefID {
        HMIRDefID::new(self.external_def_counter | (1 << 63)) // Set the highest bit to indicate external
    }

    fn is_external_id(&self, def_id: HMIRDefID) -> bool {
        def_id.index() & (1 << 63) != 0
    }

    pub fn query_external(&mut self, name: &QualifiedName) -> HMIRDefID {
        if let Some(&def_id) = self.external_defs.get(name) {
            return def_id;
        }

        let def_id = self.allocate_external_id();
        self.external_defs.insert(name.clone(), def_id);
        def_id
    }

    pub fn resolve_definition(&self, def_id: HMIRDefID) -> &HMIRDef {
        match self.is_external_id(def_id) {
            true => self
                .external_cache
                .get(&def_id)
                .expect("External symbol not found"),
            false => self
                .local_unit
                .resolve_def(def_id)
                .expect("Local symbol not found"),
        }
    }
}
