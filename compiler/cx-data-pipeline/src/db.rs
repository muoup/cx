use crate::{CompilationUnit, GlobalCompilationContext};
use cx_data_ast::parse::maps::{CXFunctionMap, CXTypeMap};
use std::collections::HashMap;
use std::sync::{Arc, LazyLock, OnceLock, RwLock, RwLockReadGuard, RwLockWriteGuard};
use cx_data_ast::lex::token::Token;
use cx_data_ast::parse::ast::CXAST;
use cx_data_bytecode::node_type_map::TypeCheckData;
use cx_data_bytecode::ProgramBytecode;
// TODO: For large codebases, this should eventually should support unloading infrequently used data
// to save memory, but for now, this is not a priority.

#[derive(Debug)]
pub struct ModuleData {
    pub lex_tokens: ModuleMap<Vec<Token>>,
    
    pub naive_type_data: ModuleMap<CXTypeMap>,
    pub full_type_data: ModuleMap<CXTypeMap>,
    
    pub function_data: ModuleMap<CXFunctionMap>,
    pub import_data: ModuleMap<Vec<String>>,

    pub naive_ast: ModuleMap<CXAST>,
    pub typechecked_ast: ModuleMap<CXAST>,
    pub typecheck_data: ModuleMap<TypeCheckData>,

    pub bytecode_data: ModuleMap<ProgramBytecode>
}

impl ModuleData {
    pub fn new() -> Self {
        ModuleData {
            lex_tokens: ModuleMap::new(".cx-tokens"),
            
            naive_type_data: ModuleMap::new(".cx-types"),
            full_type_data: ModuleMap::new(".cx-full-types"),
            
            function_data: ModuleMap::new(".cx-fns"),
            import_data: ModuleMap::new(".cx-imports"),

            naive_ast: ModuleMap::new(".cx-naive-ast"),
            typechecked_ast: ModuleMap::new(".cx-ast"),

            typecheck_data: ModuleMap::new(".cx-typecheck-data"),
            bytecode_data: ModuleMap::new(".cx-bytecode")
        }
    }
}

#[derive(Debug)]
pub struct ModuleMap<Data> {
    _storage_extension: String,
    loaded_data: RwLock<HashMap<CompilationUnit, Arc<Data>>>
}

impl<'a, Data> ModuleMap<Data> {
    pub fn new(data_suffix: &str) -> Self {
        ModuleMap {
            _storage_extension: data_suffix.to_string(),
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

    pub fn get(&self, unit: &CompilationUnit) -> Arc<Data> {
        let lock = self.loaded_data.read()
            .expect("Failed to acquire read lock on loaded data");

        lock.get(unit)
            .expect("Data not found in the module map")
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
    
    // fn load(&mut self, unit: &CompilationUnit) -> Option<()> {
    //     let path = format!("{}.{}", unit, self.data_suffix);
    //     let file_contents = std::fs::read_to_string(&path).ok()
    //         .unwrap_or_else(|| panic!("File not found: {}", path));
    //     
    //     let data: Data = serde_json::from_str(&file_contents).ok()?;
    //     
    //     self.loaded_data.insert(unit.clone(), data);
    //     
    //     Some(())
    // }
}