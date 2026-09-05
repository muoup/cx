use crate::CompilationUnit;
use cx_hir::ast::HIR;
use cx_hir::registry::GlobalSymbolRegistry;
use cx_lmir::LMIRUnit;
use cx_mir::MIRUnit;
use cx_namespace::module::NamespacePath;
use cx_preparse_data::PreparseContents;
use cx_preparse_data::registry::GlobalPreparseRegistry;
use cx_thir::THIRUnit;
use cx_tokens::token::Token;
use std::collections::HashMap;
use std::sync::{Arc, RwLock, RwLockReadGuard, RwLockWriteGuard};

// TODO: For large codebases, this should eventually should support unloading infrequently used data
// to save memory, but for now, this is not a priority.

#[derive(Debug)]
pub struct ModuleData {
    module_units: RwLock<HashMap<NamespacePath, CompilationUnit>>,

    pub preparse_registry: GlobalPreparseRegistry,
    pub symbol_registry: GlobalSymbolRegistry,

    pub lex_tokens: ModuleMap<Box<[Token]>>,
    pub preparse_base: ModuleMap<PreparseContents>,

    pub hir: ModuleMap<HIR>,
    pub base_mappings: ModuleMap<NamespacePath>,

    pub thir: ModuleMap<THIRUnit>,
    pub mir: ModuleMap<MIRUnit>,
    pub lmir: ModuleMap<LMIRUnit>,
}

impl ModuleData {
    pub fn new() -> Self {
        ModuleData {
            module_units: RwLock::new(HashMap::new()),

            preparse_registry: GlobalPreparseRegistry::default(),
            symbol_registry: GlobalSymbolRegistry::default(),

            lex_tokens: ModuleMap::new(".cx-tokens"),
            preparse_base: ModuleMap::new(".cx-preparse"),
            hir: ModuleMap::new(".cx-hir"),

            base_mappings: ModuleMap::new(".cx-structure-data"),
            thir: ModuleMap::new(".cx-thir"),
            mir: ModuleMap::new(".cx-mir"),
            lmir: ModuleMap::new(".cx-lmir"),
        }
    }

    pub fn register_unit(&self, unit: &CompilationUnit) {
        self.module_units
            .write()
            .expect("register_unit: Deadlock detected")
            .insert(unit.module().clone(), unit.clone());
    }

    pub fn unit_for_module(&self, module: &NamespacePath) -> Option<CompilationUnit> {
        self.module_units
            .read()
            .expect("unit_for_module: Deadlock detected")
            .get(module)
            .cloned()
    }
}

#[derive(Debug)]
pub struct ModuleMap<Data> {
    pub storage_extension: String,
    loaded_data: RwLock<HashMap<NamespacePath, Arc<Data>>>,
}

impl<Data> ModuleMap<Data> {
    pub fn new(data_suffix: &str) -> Self {
        ModuleMap {
            storage_extension: data_suffix.to_string(),
            loaded_data: RwLock::new(HashMap::new()),
        }
    }

    pub fn take(&self, key: &NamespacePath) -> Data {
        let mut lock = self
            .loaded_data
            .write()
            .expect("Failed to acquire write lock on loaded data");

        let removed = lock.remove(&key).expect("Data not found in the module map");

        Arc::try_unwrap(removed)
            .ok()
            .expect("Failed to unwrap Arc, data is still shared")
    }

    pub fn take_lock(
        &self,
        key: &NamespacePath,
    ) -> (
        RwLockWriteGuard<'_, HashMap<NamespacePath, Arc<Data>>>,
        Data,
    ) {
        let mut lock = self.lock_mut();

        let data = lock.remove(&key).expect("Data not found in the module map");

        // wait until data has only one reference
        while Arc::strong_count(&data) > 1 {
            std::thread::yield_now();
        }

        (lock, Arc::try_unwrap(data).ok().unwrap())
    }

    pub fn get(&self, key: &NamespacePath) -> Arc<Data> {
        let lock = self
            .loaded_data
            .read()
            .expect("Failed to acquire read lock on loaded data");

        lock.get(&key)
            .unwrap_or_else(|| {
                println!(
                    "Data with suffix {} does not contain information for unit: {}",
                    self.storage_extension, key
                );
                println!("Keys: {:?}", lock.keys().collect::<Vec<_>>());
                panic!("Data not found in the module map")
            })
            .clone()
    }

    pub fn get_cloned(&self, key: &NamespacePath) -> Data
    where
        Data: Clone,
    {
        self.get(key).as_ref().clone()
    }

    pub fn insert(&self, key: impl Into<NamespacePath>, data: Data) {
        let mut lock = self
            .loaded_data
            .write()
            .expect("Failed to acquire write lock on loaded data");

        lock.insert(key.into(), Arc::from(data));
    }

    pub fn take_all(&self) -> Vec<Data> {
        let mut lock = self
            .loaded_data
            .write()
            .expect("Failed to acquire write lock on loaded data");

        lock.drain()
            .map(|(_, arc)| Arc::try_unwrap(arc).ok().expect("Failed to unwrap Arc"))
            .collect()
    }

    pub fn lock(&self) -> RwLockReadGuard<'_, HashMap<NamespacePath, Arc<Data>>> {
        self.loaded_data
            .read()
            .expect("Failed to acquire read lock on loaded data")
    }

    pub fn lock_mut(&self) -> RwLockWriteGuard<'_, HashMap<NamespacePath, Arc<Data>>> {
        self.loaded_data
            .write()
            .expect("Failed to acquire write lock on loaded data")
    }
}
