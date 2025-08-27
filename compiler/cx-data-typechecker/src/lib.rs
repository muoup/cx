pub mod ast;
pub mod cx_types;
pub mod intrinsic_types;

mod format;

use std::collections::{HashMap, HashSet};
use speedy::{Readable, Writable};
use cx_data_ast::parse::ast::CXAST;
use cx_data_ast::preparse::naive_types::ModuleResource;
use cx_data_ast::preparse::templates::{CXFunctionTemplate, CXTypeTemplate};
use cx_types::CXType;
use cx_util::scoped_map::ScopedMap;
use crate::cx_types::{CXFunctionPrototype, CXTemplateInput};
 
#[derive(Debug, Default, Clone, Readable, Writable)]
pub struct TemplateCache<Template> {
    pub template: ModuleResource<Template>,
    pub instantiated: HashSet<CXTemplateInput>
}

#[derive(Debug, Default, Clone, Readable, Writable)]
pub struct GenericMap<Standard, Template> {
    pub standard: HashMap<String, Standard>,
    pub templates: HashMap<String, TemplateCache<Template>>,
}

pub type CXTypeMap = GenericMap<CXType, CXTypeTemplate>;
pub type CXFunctionMap = GenericMap<CXFunctionPrototype, CXFunctionTemplate>;

pub struct TCEnvironment {
    pub type_map: CXTypeMap,
    pub fn_map: CXFunctionMap,
    
    pub current_function: Option<CXFunctionPrototype>,
    pub symbol_table: ScopedMap<CXType>,
}

impl<Type, TemplatedType> GenericMap<Type, TemplatedType> {
    pub fn new() -> Self {
        GenericMap {
            standard: HashMap::new(),
            templates: HashMap::new(),
        }
    }
    
    pub fn insert_standard(&mut self, name: String, item: Type) {
        self.standard.insert(name, item);
    }
    
    pub fn insert_template(&mut self, name: String, item: ModuleResource<TemplatedType>) {
        self.templates.insert(name, TemplateCache {
            template: item,
            instantiated: HashSet::new(),
        });
    }
    
    pub fn get(&self, name: &str) -> Option<&Type> {
        self.standard.get(name)
    }
    
    pub fn get_template(&self, name: &str) -> Option<&TemplateCache<TemplatedType>> {
        self.templates
            .get(name)
    }
}

impl TCEnvironment {
    pub fn push_scope(&mut self) {
        self.symbol_table.push_scope();
    }
    
    pub fn pop_scope(&mut self) {
        self.symbol_table.pop_scope();
    }
    
    pub fn insert_symbol(&mut self, name: String, ty: CXType) {
        self.symbol_table.insert(name, ty);
    }

    pub fn symbol_type(&self, name: &str) -> Option<&CXType> {
        self.symbol_table.get(name)
    }

    pub fn get_func(&self, name: &str) -> Option<&CXFunctionPrototype> {
        self.fn_map.standard.get(name)
    }

    pub fn get_type(&self, name: &str) -> Option<&CXType> {
        self.type_map.standard.get(name)
    }

    pub fn current_function(&self) -> &CXFunctionPrototype {
        self.current_function.as_ref()
            .unwrap()
    }
}