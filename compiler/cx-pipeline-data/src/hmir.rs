use std::{
    collections::HashMap,
    sync::{Arc, RwLock, RwLockReadGuard},
};

use cx_hmir::{
    HMIRDefID, HMIRUnit, HMIRUnitAllocator, HMIRUnitID, unit::symbols::HMIRLookupClass,
};
use cx_namespace::module::{NamespacePath, QualifiedName};

use crate::db::ModuleMap;

#[derive(Debug)]
pub struct HMIRUnitRegistry {
    allocator: HMIRUnitAllocator,
    units: ModuleMap<HMIRUnit>,
    index: RwLock<UnitIndex>,
}

#[derive(Debug, Default)]
struct UnitIndex {
    namespaces: HashMap<HMIRUnitID, NamespacePath>,
    names: HashMap<(QualifiedName, HMIRLookupClass), Vec<Vec<HMIRDefID>>>,
}

impl HMIRUnitRegistry {
    pub fn new() -> Self {
        Self {
            allocator: HMIRUnitAllocator::default(),
            units: ModuleMap::new(".cx-hmir"),
            index: RwLock::new(UnitIndex::default()),
        }
    }

    pub fn allocate(&self) -> HMIRUnitID {
        self.allocator.allocate()
    }

    pub fn insert(&self, unit: HMIRUnit) {
        let mut index = self
            .index
            .write()
            .expect("Failed to acquire write lock on HMIR unit index");
        if let Some(previous) = self.get(unit.namespace()) {
            index.remove(&previous);
        }
        index.add(&unit);
        self.units.insert(unit.namespace().clone(), unit);
    }

    pub fn get(&self, namespace: &NamespacePath) -> Option<Arc<HMIRUnit>> {
        self.units.lock().get(namespace).cloned()
    }

    pub fn owner(&self, id: HMIRUnitID) -> Option<Arc<HMIRUnit>> {
        let namespace = self.read().namespaces.get(&id)?.clone();
        self.get(&namespace).filter(|unit| unit.id() == id)
    }

    pub fn groups(&self, name: &QualifiedName, class: HMIRLookupClass) -> Vec<Vec<HMIRDefID>> {
        self.read()
            .names
            .get(&(name.clone(), class))
            .cloned()
            .unwrap_or_default()
    }

    fn read(&self) -> RwLockReadGuard<'_, UnitIndex> {
        self.index
            .read()
            .expect("Failed to acquire read lock on HMIR unit index")
    }
}

impl UnitIndex {
    fn add(&mut self, unit: &HMIRUnit) {
        self.namespaces.insert(unit.id(), unit.namespace().clone());
        for (name, class, defs) in unit.symbols().groups() {
            self.names
                .entry((name.clone(), class))
                .or_default()
                .push(defs.to_vec());
        }
    }

    fn remove(&mut self, unit: &HMIRUnit) {
        self.namespaces.remove(&unit.id());
        for (name, class, _) in unit.symbols().groups() {
            let key = (name.clone(), class);
            let Some(groups) = self.names.get_mut(&key) else {
                continue;
            };
            groups.retain(|group| group[0].brand() != unit.id());
            if groups.is_empty() {
                self.names.remove(&key);
            }
        }
    }
}
