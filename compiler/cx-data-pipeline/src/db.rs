use crate::{CompilationUnit, GlobalCompilationContext};
use cx_data_ast::parse::maps::{CXFunctionMap, CXTypeMap};
use std::collections::HashMap;
use std::sync::{RwLockReadGuard, RwLockWriteGuard};
use cx_data_ast::parse::ast::CXAST;
use cx_data_bytecode::node_type_map::TypeCheckData;
use cx_data_bytecode::ProgramBytecode;
// TODO: For large codebases, this should eventually should support unloading infrequently used data
// to save memory, but for now, this is not a priority.

impl GlobalCompilationContext {
    pub fn request_access_mut(&self) -> RwLockWriteGuard<ModuleData> {
        self.module_db.write().expect("Deadlock detected")
    }
    
    pub fn request_access(&self) -> RwLockReadGuard<ModuleData> {
        self.module_db.read().expect("Deadlock detected")
    }
    
    pub fn take_resource<Getter, Resource>(&self, module: &CompilationUnit, getter: Getter) 
                                           -> Option<Resource>
        where Getter: Fn(&mut ModuleData) -> Option<&mut ModuleMap<Resource>>
    {
        let mut module_data = self.request_access_mut();
        
        getter(&mut module_data)
            .and_then(|map| map.loaded_data.remove(module))
    }
    
    pub fn give_resource<Getter, Resource>(&self, module: CompilationUnit, getter: Getter, resource: Resource) 
                                         -> Option<()>
        where Getter: Fn(&mut ModuleData) -> Option<&mut ModuleMap<Resource>>
    {
        let mut module_data = self.request_access_mut();
        
        getter(&mut module_data)
            .and_then(|map| {
                map.insert(module, resource);
                Some(())
            })
    }
    
    pub fn request_resource<Getter, Mapping, Resource>(&self, module: &CompilationUnit, getter: Getter, mapping: Mapping)
                                         -> Option<()> 
        where Getter: Fn(&mut ModuleData) -> Option<&mut ModuleMap<Mapping>>,
              Mapping: Fn(Resource) -> Resource
    {
        Some(())
    }
    
    pub fn request_imports(&self, module: &CompilationUnit) -> Option<Vec<String>> {
        let mut module_data = self.request_access_mut();
        module_data.import_data.get(module).cloned()
    }
}

#[derive(Debug)]
pub struct ModuleData {
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
    loaded_data: HashMap<CompilationUnit, Data>
}

impl<'a, Data> ModuleMap<Data> {
    pub fn new(data_suffix: &str) -> Self {
        ModuleMap {
            _storage_extension: data_suffix.to_string(),
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
    
    pub fn get_cloned(&mut self, unit: &CompilationUnit) -> Option<Data>
        where Data: Clone {
        self.get(unit).cloned()
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