use crate::internal_storage::{retrieve_data, store_data};
use crate::{CompilationUnit, GlobalCompilationContext};
use cx_data_lexer::token::Token;
use cx_data_ast::parse::ast::CXAST;
use cx_data_ast::parse::maps::{CXFunctionMap, CXTypeMap};
use cx_data_bytecode::node_type_map::TypeCheckData;
use cx_data_bytecode::ProgramBytecode;
use speedy::{LittleEndian, Readable, Writable};
use std::collections::{HashMap, HashSet};
use std::sync::{Arc, RwLock, RwLockReadGuard, RwLockWriteGuard};

// TODO: For large codebases, this should eventually should support unloading infrequently used data
// to save memory, but for now, this is not a priority.

#[derive(Debug)]
pub struct ModuleData {
    pub do_not_reexport: RwLock<HashSet<CompilationUnit>>,
    
    pub lex_tokens: ModuleMap<Vec<Token>>,
    
    pub naive_type_data: ModuleMap<CXTypeMap>,
    pub naive_function_data: ModuleMap<CXFunctionMap>,
    
    pub import_data: ModuleMap<Vec<String>>,

    pub naive_ast: ModuleMap<CXAST>,
    pub typechecked_ast: ModuleMap<CXAST>,
    pub typecheck_data: ModuleMap<TypeCheckData>,
    
    pub bytecode_data: ModuleMap<ProgramBytecode>
}

impl ModuleData {
    pub fn new() -> Self {
        ModuleData {
            do_not_reexport: RwLock::new(HashSet::new()),
            
            lex_tokens: ModuleMap::new(".cx-tokens"),
            
            naive_type_data: ModuleMap::new(".cx-types"),
            naive_function_data: ModuleMap::new(".cx-fns"),
            
            import_data: ModuleMap::new(".cx-imports"),

            naive_ast: ModuleMap::new(".cx-naive-ast"),
            typechecked_ast: ModuleMap::new(".cx-ast"),

            typecheck_data: ModuleMap::new(".cx-typecheck-data"),
            bytecode_data: ModuleMap::new(".cx-bytecode")
        }
    }
    
    pub fn store_data(&self, context: &GlobalCompilationContext) {
        self.naive_type_data.store_all_data(context);
        self.naive_function_data.store_all_data(context);
    }
    
    pub fn no_reexport(&self, unit: &CompilationUnit) -> bool {
        self.do_not_reexport.read()
            .expect("no_reexport: Deadlock detected")
            .contains(unit)
    }
    
    pub fn set_no_reexport(&self, unit: &CompilationUnit) {
        self.do_not_reexport.write()
            .expect("set_no_reexport: Deadlock detected")
            .insert(unit.clone());
    }
}

#[derive(Debug)]
pub struct ModuleMap<Data> {
    pub storage_extension: String,
    loaded_data: RwLock<HashMap<CompilationUnit, Arc<Data>>>
}

impl<'a, Data> ModuleMap<Data> {
    pub fn new(data_suffix: &str) -> Self {
        ModuleMap {
            storage_extension: data_suffix.to_string(),
            loaded_data: RwLock::new(HashMap::new())
        }
    }
    
    pub fn take(&self, unit: &CompilationUnit) -> Data {
        let mut lock = self.loaded_data.write()
            .expect("Failed to acquire write lock on loaded data");

        let removed = lock.remove(unit)
            .expect("Data not found in the module map");
     
        Arc::try_unwrap(removed)
            .ok()
            .expect("Failed to unwrap Arc, data is still shared")
    }
    
    pub fn take_lock(&self, unit: &CompilationUnit) -> (RwLockWriteGuard<'_, HashMap<CompilationUnit, Arc<Data>>>, Data) {
        let mut lock = self.lock_mut();

        let data = lock.remove(unit)
            .expect("Data not found in the module map");

        // wait until data has only one reference
        while Arc::strong_count(&data) > 1 {
            std::thread::yield_now();
        }
        
        (lock, Arc::try_unwrap(data).ok().unwrap())
    }

    pub fn get(&self, unit: &CompilationUnit) -> Arc<Data> {
        let lock = self.loaded_data.read()
            .expect("Failed to acquire read lock on loaded data");

        lock.get(unit)
            .unwrap_or_else(|| {
                println!("Data with suffix {} does not contain information for unit: {}", self.storage_extension, unit);
                println!("Keys in map: {:?}", lock.keys().collect::<Vec<_>>());
                panic!("Data not found in the module map")
            })
            .clone()
    }

    pub fn get_cloned(&self, unit: &CompilationUnit) -> Data
        where Data: Clone {
        self.get(unit)
            .as_ref()
            .clone()
    }
    
    pub fn insert(&self, unit: CompilationUnit, data: Data) {
        let mut lock = self.loaded_data.write()
            .expect("Failed to acquire write lock on loaded data");

        lock.insert(unit, Arc::from(data));
    }

    pub fn lock(&self) -> RwLockReadGuard<'_, HashMap<CompilationUnit, Arc<Data>>> {
        self.loaded_data.read()
            .expect("Failed to acquire read lock on loaded data")
    }

    pub fn lock_mut(&self) -> RwLockWriteGuard<'_, HashMap<CompilationUnit, Arc<Data>>> {
        self.loaded_data.write()
            .expect("Failed to acquire write lock on loaded data")
    }
}

impl <'a, Data: Readable<'a, LittleEndian> + Writable<LittleEndian> + Clone> ModuleMap<Data> {
    pub fn load_data(&self, context: &GlobalCompilationContext, unit: &CompilationUnit) -> Option<()> {
        let data = retrieve_data::<HashMap<CompilationUnit, Data>>(context, unit, &self.storage_extension)?;
        let mut lock = self.loaded_data.write()
            .expect("Failed to acquire write lock on loaded data");

        lock.extend(data.into_iter().map(|(k, v)| (k, Arc::new(v))));

        Some(())
    }
    
    pub fn store_all_data(&self, context: &GlobalCompilationContext) {
        let lock = self.loaded_data.read()
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