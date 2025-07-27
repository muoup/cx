use crate::{CompilationUnit, GlobalCompilationContext};
use cx_data_ast::parse::maps::{CXFunctionMap, CXTypeMap};
use serde::de::DeserializeOwned;
use std::collections::HashMap;
use cx_data_ast::parse::ast::CXFunctionPrototype;
use cx_data_ast::parse::value_type::CXType;

// TODO: For large codebases, this should eventually should support unloading infrequently used data
// to save memory, but for now, this is not a priority.

impl GlobalCompilationContext {
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
}

pub struct ModuleData {
    type_data: ModuleMap<CXTypeMap>,
    function_data: ModuleMap<CXFunctionMap>
}

pub struct ModuleMap<Data: DeserializeOwned> {
    data_suffix: String,
    loaded_data: HashMap<CompilationUnit, Data>
}

impl<'a, Data: DeserializeOwned> ModuleMap<Data> {
    pub fn get(&mut self, unit: &CompilationUnit) -> Option<&Data> {
        if !self.loaded_data.contains_key(unit) {
            self.load(unit)?;
        }
        
        self.loaded_data.get(unit)
    }
    
    fn load(&mut self, unit: &CompilationUnit) -> Option<()> {
        let path = format!("{}.{}", unit, self.data_suffix);
        let file_contents = std::fs::read_to_string(&path).ok()
            .unwrap_or_else(|| panic!("File not found: {}", path));
        
        let data: Data = serde_json::from_str(&file_contents).ok()?;
        
        self.loaded_data.insert(unit.clone(), data);
        
        Some(())
    }
}