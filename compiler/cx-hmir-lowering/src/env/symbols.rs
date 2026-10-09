use cx_hmir::{HMIRDef, HMIRDefID, HMIRUnit};
use cx_pipeline_data::db::ModuleData;

///
/// Handles accessing the HMIR environment. An HMIR environment contains both the HMIR module itself, as well as
/// public interfaces of imported modules. For things like comptime functions, the symbol environment is used as
/// a lazy loader that handles caching and loading such that external symbols can be used as if they were local.
///
pub struct HMIRSymbolEnv<'global, 'hmir> {
    module_map: &'global ModuleMap<HMIRUnit>,
    local_unit: &'hmir HMIRUnit,

    external_defs: HashMap<QualifiedName, HMIRDefID>,
    external_cache: HashMap<HMIRDefID, &'global HMIRDef>,
    external_def_counter: usize,
}

impl HMIRSymbolEnv {
    fn allocate_external_id(&self) -> HMIRDefID {
        HMIRDefID::new(external_def_counter | (1 << 63)) // Set the highest bit to indicate external
    }

    fn is_external_id(&self, def_id: HMIRDefID) -> bool {
        def_id.as_u64() & (1 << 63) != 0
    }

    pub fn query_external(&self, name: &QualifiedName) -> HMIRDefID {
        if let Some(&def_id) = self.external_defs.get(name) {
            def_id
        }

        let def_id = self.allocate_external_id();
        self.external_defs.insert(name.clone(), def_id);
        def_id
    }

    pub fn resolve_definition(&self, def_id: HMIRDefId) -> &HMIRDef {
        match self.is_external_id(def_id) {
            true => self
                .external_cache
                .get(&def_id)
                .expect("External symbol not found"),
            false => self
                .local_unit
                .definitions()
                .get(&def_id)
                .expect("Local symbol not found"),
        }
    }
}
