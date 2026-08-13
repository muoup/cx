use crate::internal_storage::{retrieve_data, store_data};
use crate::{CompilationUnit, GlobalCompilationContext};
use cx_ast::decomposition::CXGenerationAST;
use cx_ast::registry::GlobalSymbolRegistry;
use cx_lmir::LMIRUnit;
use cx_log::error::CXErrContext;
use cx_log::error::context::{CXInternalContext, CXUnderlineContext};
use cx_mir::MIRUnit;
use cx_preparse_data::PreparseContents;
use cx_preparse_data::registry::GlobalPreparseRegistry;
use cx_thir::THIRUnit;
use cx_tokens::TokenRange;
use cx_tokens::token::Token;
use cx_util::namespace::EnvironmentNamespace;
use speedy::{LittleEndian, Readable, Writable};
use std::collections::{HashMap, HashSet};
use std::sync::{Arc, RwLock, RwLockReadGuard, RwLockWriteGuard};
// TODO: For large codebases, this should eventually should support unloading infrequently used data
// to save memory, but for now, this is not a priority.

#[derive(Debug)]
pub struct ModuleData {
    module_units: RwLock<HashMap<EnvironmentNamespace, CompilationUnit>>,
    pub do_not_reexport: RwLock<HashSet<EnvironmentNamespace>>,

    pub preparse_registry: GlobalPreparseRegistry,
    pub symbol_registry: GlobalSymbolRegistry,

    pub lex_tokens: ModuleMap<Box<[Token]>>,
    pub preparse_base: ModuleMap<PreparseContents>,

    pub generation_ast: ModuleMap<CXGenerationAST>,
    pub base_mappings: ModuleMap<EnvironmentNamespace>,

    pub thir: ModuleMap<THIRUnit>,
    pub mir: ModuleMap<MIRUnit>,
    pub lmir: ModuleMap<LMIRUnit>,
}

impl Default for ModuleData {
    fn default() -> Self {
        Self::new()
    }
}

impl ModuleData {
    pub fn new() -> Self {
        ModuleData {
            module_units: RwLock::new(HashMap::new()),
            do_not_reexport: RwLock::new(HashSet::new()),
            preparse_registry: GlobalPreparseRegistry::default(),
            symbol_registry: GlobalSymbolRegistry::default(),

            lex_tokens: ModuleMap::new(".cx-tokens"),
            preparse_base: ModuleMap::new(".cx-preparse"),
            generation_ast: ModuleMap::new(".cx-naive-ast"),

            base_mappings: ModuleMap::new(".cx-structure-data"),
            thir: ModuleMap::new(".cx-thir"),
            mir: ModuleMap::new(".cx-mir"),
            lmir: ModuleMap::new(".cx-lmir"),
        }
    }

    pub fn store_data(&self, context: &GlobalCompilationContext) {
        self.preparse_base.store_all_data(context);
    }

    pub fn register_unit(&self, unit: &CompilationUnit) {
        self.module_units
            .write()
            .expect("register_unit: Deadlock detected")
            .insert(unit.namespace().clone(), unit.clone());
    }

    pub fn unit_for_namespace(&self, namespace: &EnvironmentNamespace) -> Option<CompilationUnit> {
        self.module_units
            .read()
            .expect("unit_for_namespace: Deadlock detected")
            .get(namespace)
            .cloned()
    }

    pub fn no_reexport(&self, unit: &CompilationUnit) -> bool {
        self.do_not_reexport
            .read()
            .expect("no_reexport: Deadlock detected")
            .contains(unit.namespace())
    }

    pub fn set_no_reexport(&self, unit: &CompilationUnit) {
        self.do_not_reexport
            .write()
            .expect("set_no_reexport: Deadlock detected")
            .insert(unit.namespace().clone());
    }

    pub fn convert_token_range(&self, range: &TokenRange) -> CXErrContext {
        match range {
            TokenRange::Source {
                namespace,
                start_token,
                end_token,
            } => {
                let lock = self.lex_tokens.lock();

                let Some(tokens) = lock.get(namespace) else {
                    return CXInternalContext::error(format!(
                        "failed to resolve diagnostic context: no tokens found for namespace {namespace}"
                    ));
                };

                let Some(start) = tokens.get(*start_token) else {
                    return CXInternalContext::error(format!(
                        "failed to resolve diagnostic context: start token {start_token} not found in namespace {namespace}"
                    ));
                };

                let Some(end) = tokens.get(end_token.saturating_sub(1)) else {
                    return CXInternalContext::error(format!(
                        "failed to resolve diagnostic context: end token {end_token} not found in namespace {namespace}"
                    ));
                };

                Box::new(CXUnderlineContext::new(
                    start.file_origin.as_ref().to_path_buf(),
                    start.byte_start_index,
                    end.byte_end_index,
                ))
            }
            TokenRange::Internal => {
                CXInternalContext::error("diagnostic originated in compiler-generated code")
            }
            TokenRange::Error(range_error) => {
                CXInternalContext::error(format!("failed to determine source range: {range_error}"))
            }
        }
    }
}

#[derive(Debug)]
pub struct ModuleMap<Data> {
    pub storage_extension: String,
    loaded_data: RwLock<HashMap<EnvironmentNamespace, Arc<Data>>>,
}

pub trait ModuleMapKey {
    fn module_key(&self) -> EnvironmentNamespace;
}

impl ModuleMapKey for EnvironmentNamespace {
    fn module_key(&self) -> EnvironmentNamespace {
        self.clone()
    }
}

impl ModuleMapKey for CompilationUnit {
    fn module_key(&self) -> EnvironmentNamespace {
        self.namespace().clone()
    }
}

impl<T: ModuleMapKey + ?Sized> ModuleMapKey for &T {
    fn module_key(&self) -> EnvironmentNamespace {
        (*self).module_key()
    }
}

impl<Data> ModuleMap<Data> {
    pub fn new(data_suffix: &str) -> Self {
        ModuleMap {
            storage_extension: data_suffix.to_string(),
            loaded_data: RwLock::new(HashMap::new()),
        }
    }

    pub fn take(&self, key: impl ModuleMapKey) -> Data {
        let key = key.module_key();
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
        key: impl ModuleMapKey,
    ) -> (
        RwLockWriteGuard<'_, HashMap<EnvironmentNamespace, Arc<Data>>>,
        Data,
    ) {
        let key = key.module_key();
        let mut lock = self.lock_mut();

        let data = lock.remove(&key).expect("Data not found in the module map");

        // wait until data has only one reference
        while Arc::strong_count(&data) > 1 {
            std::thread::yield_now();
        }

        (lock, Arc::try_unwrap(data).ok().unwrap())
    }

    pub fn get(&self, key: impl ModuleMapKey) -> Arc<Data> {
        let key = key.module_key();
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

    pub fn get_cloned(&self, key: impl ModuleMapKey) -> Data
    where
        Data: Clone,
    {
        self.get(key).as_ref().clone()
    }

    pub fn insert(&self, key: impl Into<EnvironmentNamespace>, data: Data) {
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

    pub fn lock(&self) -> RwLockReadGuard<'_, HashMap<EnvironmentNamespace, Arc<Data>>> {
        self.loaded_data
            .read()
            .expect("Failed to acquire read lock on loaded data")
    }

    pub fn lock_mut(&self) -> RwLockWriteGuard<'_, HashMap<EnvironmentNamespace, Arc<Data>>> {
        self.loaded_data
            .write()
            .expect("Failed to acquire write lock on loaded data")
    }
}

impl<'a, Data: Readable<'a, LittleEndian> + Writable<LittleEndian> + Clone> ModuleMap<Data> {
    pub fn load_data(
        &self,
        context: &GlobalCompilationContext,
        unit: &CompilationUnit,
    ) -> Option<()> {
        let data = retrieve_data::<HashMap<EnvironmentNamespace, Data>>(
            context,
            unit,
            &self.storage_extension,
        )?;
        let mut lock = self
            .loaded_data
            .write()
            .expect("Failed to acquire write lock on loaded data");

        lock.extend(data.into_iter().map(|(k, v)| (k, Arc::new(v))));

        Some(())
    }

    pub fn store_all_data(&self, context: &GlobalCompilationContext) {
        let lock = self
            .loaded_data
            .read()
            .expect("Failed to acquire read lock on loaded data");

        for namespace in lock.keys() {
            let Some(unit) = context.module_db.unit_for_namespace(namespace) else {
                continue;
            };

            if context.module_db.no_reexport(&unit) {
                continue;
            }

            self.store_data(context, &unit);
        }
    }

    pub fn store_data(&self, context: &GlobalCompilationContext, unit: &CompilationUnit) {
        let data_copy = self.get_cloned(unit);
        store_data(context, unit, &self.storage_extension, data_copy);
    }
}
