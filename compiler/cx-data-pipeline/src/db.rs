use crate::{CompilationUnit, GlobalCompilationContext};
use cx_data_ast::parse::ast::CXFunctionPrototype;
use cx_data_ast::parse::maps::{CXFunctionMap, CXTypeMap};
use cx_data_ast::parse::value_type::CXType;
use cx_data_ast::preparse::{CXPreparseToken, CXPreparseTokens};
use std::collections::HashMap;
use std::sync::MutexGuard;
use cx_data_ast::PreparseContents;
// TODO: For large codebases, this should eventually should support unloading infrequently used data
// to save memory, but for now, this is not a priority.

impl GlobalCompilationContext {
    pub fn request_lock(&self) -> Option<MutexGuard<'_, ModuleData>> {
        self.module_db.lock().ok()
    }
    
    pub fn request_type(&self, module: &CompilationUnit, type_name: &str) -> Option<CXType> {
        self.module_db.lock()
            .ok()?
            .type_data
            .get(module)
            .and_then(|data| data.get(type_name))
            .cloned()
    }
    
    pub fn request_function(&self, module: &CompilationUnit, function_name: &str) -> Option<CXFunctionPrototype> {
        self.module_db.lock()
            .ok()?
            .function_data
            .get(module)
            .and_then(|data| data.get(function_name))
            .cloned()
    }
    
    pub fn request_imports(&self, module: &CompilationUnit) -> Option<Vec<String>> {
        self.module_db.lock()
            .ok()?
            .import_data
            .get(module)
            .cloned()
    }
    
    pub fn request_identifiers(&self, module: &CompilationUnit) -> Option<CXPreparseTokens> {
        self.module_db.lock()
            .ok()?
            .identifier_data
            .get(module)
            .cloned()
    }
}

#[derive(Debug)]
pub struct ModuleData {
    pub type_data: ModuleMap<CXTypeMap>,
    pub function_data: ModuleMap<CXFunctionMap>,
    pub identifier_data: ModuleMap<CXPreparseTokens>,
    pub import_data: ModuleMap<Vec<String>>,
}

impl ModuleData {
    pub fn new() -> Self {
        ModuleData {
            type_data: ModuleMap::new(".cx-types"),
            function_data: ModuleMap::new(".cx-fns"),
            identifier_data: ModuleMap::new(".cx-imports"),
            import_data: ModuleMap::new(".cx-imports"),
        }
    }
}

#[derive(Debug)]
pub struct ModuleMap<Data> {
    data_suffix: String,
    loaded_data: HashMap<CompilationUnit, Data>
}

impl<'a, Data> ModuleMap<Data> {
    pub fn new(data_suffix: &str) -> Self {
        ModuleMap {
            data_suffix: data_suffix.to_string(),
            loaded_data: HashMap::new()
        }
    }
    
    pub fn get(&mut self, unit: &CompilationUnit) -> Option<&Data> {
        if !self.loaded_data.contains_key(unit) {
            todo!("Lazy data reloading is not implemented yet");
            // self.load(unit)?;
        }
      
        self.loaded_data.get(unit)
    }
    
    pub fn insert(&mut self, unit: CompilationUnit, data: Data) {
        self.loaded_data.insert(unit, data);
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