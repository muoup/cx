use std::{collections::HashMap, sync::Arc};

use cx_hmir::{HMIRDef, HMIRDefID, HMIRUnit};
use cx_namespace::module::{NamespacePath, QualifiedName};
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

    defs: HashMap<HMIRDefID, &'global HMIRDef>,
    
    unit_cache: HashMap<NamespacePath, Arc<HMIRUnit>>,
    def_cache: HashMap<QualifiedName, HMIRDefID>,

    next_external_def_id: usize,
}

impl<'global, 'hmir> HMIRSymbolEnv<'global, 'hmir> {
    pub fn new(module_map: &'global ModuleMap<HMIRUnit>, local_unit: &'hmir HMIRUnit) -> Self {
        Self {
            module_map,
            local_unit,

            defs: HashMap::new(),

            unit_cache: HashMap::new(),
            def_cache: HashMap::new(),

            next_external_def_id: 0
        }
    }

    pub fn local_unit(&self) -> &'hmir HMIRUnit {
        self.local_unit
    }

    fn allocate_external_id(&mut self) -> HMIRDefID {
        let id = HMIRDefID::new(self.next_external_def_id | (1 << 63)); // Set the highest bit to indicate external
        self.next_external_def_id += 1;
        id
    }

    fn is_external_id(&self, def_id: HMIRDefID) -> bool {
        def_id.index() & (1 << 63) != 0
    }

    pub fn query_external(&mut self, name: &QualifiedName) -> HMIRDefID {
        if let Some(&def_id) = self.def_cache.get(name) {
            return def_id;
        }

        let candidates = self
            .local_unit
            .resolve_extern(&name.namespace)
            .iter()
            .map(|ns| self.resolve_unit(ns))
            .collect::<Vec<_>>();

        let mut potential = vec![];
        for candidate in candidates {
            if let Some(def_id) = candidate.find_symbol(&name.name.as_str()) {
                // SAFETY: The Arc we own is derived from the arc in the module map, which we can assume will last the length
                // of the pipeline.
                let reference = unsafe { std::mem::transmute::<&HMIRDef, &'global HMIRDef>(candidate.resolve_def(def_id).unwrap()) };

                potential.push((candidate.clone(), reference));
            }
        }

        match potential.len() {
            0 => panic!("External symbol not found: {}", name),
            1 => {
                let (_, def) = potential.pop().unwrap();
                let id = self.allocate_external_id();
                
                self.def_cache.insert(name.clone(), id);
                self.defs.insert(id, def);

                id
            }
            _ => panic!("Ambiguous external symbol: {}", name),
        }
    }

    fn resolve_unit<'s>(&'s mut self, namespace: &NamespacePath) -> Arc<HMIRUnit> {
        if let Some(unit) = self.unit_cache.get(namespace) {
            return unit.clone();
        }

        let unit = self.module_map.get(namespace);

        self.unit_cache.insert(namespace.clone(), unit.clone());
        unit
    }

    pub fn resolve_definition(&self, def_id: HMIRDefID) -> &HMIRDef {
        match self.is_external_id(def_id) {
            true => self
                .defs
                .get(&def_id)
                .expect("External symbol not found"),
            false => self
                .local_unit
                .resolve_def(def_id)
                .expect("Local symbol not found"),
        }
    }
}
