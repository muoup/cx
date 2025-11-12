use crate::internal_storage::{retrieve_data, store_data};
use crate::{CompilationUnit, GlobalCompilationContext};
use cx_parsing_data::ast::CXAST;
use cx_parsing_data::PreparseContents;
use cx_lexer_data::token::Token;
use cx_mir_data::MIRUnit;
use cx_typechecker_data::ast::{TCAST, TCBaseMappings};
use speedy::{LittleEndian, Readable, Writable};
use std::collections::{HashMap, HashSet};
use std::sync::{Arc, RwLock, RwLockReadGuard, RwLockWriteGuard};
// TODO: For large codebases, this should eventually should support unloading infrequently used data
// to save memory, but for now, this is not a priority.

#[derive(Debug)]
pub struct ModuleData {
    pub do_not_reexport: RwLock<HashSet<CompilationUnit>>,

    pub lex_tokens: ModuleMap<Vec<Token>>,
    pub preparse_base: ModuleMap<PreparseContents>,

    pub naive_ast: ModuleMap<CXAST>,
    pub base_mappings: ModuleMap<TCBaseMappings>,
    
    pub typechecked_ast: ModuleMap<TCAST>,

    pub bytecode: ModuleMap<MIRUnit>,
}

impl Default for ModuleData {
    fn default() -> Self {
        Self::new()
    }
}

impl ModuleData {
    pub fn new() -> Self {
        ModuleData {
            do_not_reexport: RwLock::new(HashSet::new()),

            lex_tokens: ModuleMap::new(".cx-tokens"),

            preparse_base: ModuleMap::new(".cx-preparse"),

            naive_ast: ModuleMap::new(".cx-naive-ast"),

            base_mappings: ModuleMap::new(".cx-structure-data"),
            typechecked_ast: ModuleMap::new(".cx-typechecked-ast"),

            bytecode: ModuleMap::new(".cx-bytecode"),
        }
    }

    pub fn store_data(&self, context: &GlobalCompilationContext) {
        self.preparse_base.store_all_data(context);
    }

    pub fn no_reexport(&self, unit: &CompilationUnit) -> bool {
        self.do_not_reexport
            .read()
            .expect("no_reexport: Deadlock detected")
            .contains(unit)
    }

    pub fn set_no_reexport(&self, unit: &CompilationUnit) {
        self.do_not_reexport
            .write()
            .expect("set_no_reexport: Deadlock detected")
            .insert(unit.clone());
    }
}

#[derive(Debug)]
pub struct ModuleMap<Data> {
    pub storage_extension: String,
    loaded_data: RwLock<HashMap<CompilationUnit, Arc<Data>>>,
}

impl<'a, Data> ModuleMap<Data> {
    pub fn new(data_suffix: &str) -> Self {
        ModuleMap {
            storage_extension: data_suffix.to_string(),
            loaded_data: RwLock::new(HashMap::new()),
        }
    }

    pub fn take(&self, unit: &CompilationUnit) -> Data {
        let mut lock = self
            .loaded_data
            .write()
            .expect("Failed to acquire write lock on loaded data");

        let removed = lock.remove(unit).expect("Data not found in the module map");

        Arc::try_unwrap(removed)
            .ok()
            .expect("Failed to unwrap Arc, data is still shared")
    }

    pub fn take_lock(
        &self,
        unit: &CompilationUnit,
    ) -> (
        RwLockWriteGuard<'_, HashMap<CompilationUnit, Arc<Data>>>,
        Data,
    ) {
        let mut lock = self.lock_mut();

        let data = lock.remove(unit).expect("Data not found in the module map");

        // wait until data has only one reference
        while Arc::strong_count(&data) > 1 {
            std::thread::yield_now();
        }

        (lock, Arc::try_unwrap(data).ok().unwrap())
    }

    pub fn get(&self, unit: &CompilationUnit) -> Arc<Data> {
        let lock = self
            .loaded_data
            .read()
            .expect("Failed to acquire read lock on loaded data");

        lock.get(unit)
            .unwrap_or_else(|| {
                println!(
                    "Data with suffix {} does not contain information for unit: {}",
                    self.storage_extension, unit.identifier()
                );
                println!("Keys: {:?}", lock.keys().collect::<Vec<_>>());
                panic!("Data not found in the module map")
            })
            .clone()
    }

    pub fn get_cloned(&self, unit: &CompilationUnit) -> Data
    where
        Data: Clone,
    {
        self.get(unit).as_ref().clone()
    }

    pub fn insert(&self, unit: CompilationUnit, data: Data) {
        let mut lock = self
            .loaded_data
            .write()
            .expect("Failed to acquire write lock on loaded data");

        lock.insert(unit, Arc::from(data));
    }

    pub fn lock(&self) -> RwLockReadGuard<'_, HashMap<CompilationUnit, Arc<Data>>> {
        self.loaded_data
            .read()
            .expect("Failed to acquire read lock on loaded data")
    }

    pub fn lock_mut(&self) -> RwLockWriteGuard<'_, HashMap<CompilationUnit, Arc<Data>>> {
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
        let data = retrieve_data::<HashMap<CompilationUnit, Data>>(
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

        for unit in lock.keys() {
            if context.module_db.no_reexport(unit) {
                continue;
            }

            self.store_data(context, unit);
        }
    }

    pub fn store_data(&self, context: &GlobalCompilationContext, unit: &CompilationUnit) {
        let data_copy = self.get_cloned(unit);
        store_data(context, unit, &self.storage_extension, data_copy);
    }
}
