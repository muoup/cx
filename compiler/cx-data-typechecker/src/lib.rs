pub mod ast;
pub mod cx_types;
pub mod intrinsic_types;

pub mod format;

use std::collections::{HashMap, HashSet};
use speedy::{Readable, Writable};
use cx_data_ast::preparse::naive_types::ModuleResource;
use cx_data_ast::preparse::templates::{CXFunctionTemplate, CXTypeTemplate};
use cx_types::CXType;
use crate::cx_types::{CXFunctionPrototype, CXTemplateInput};
 
#[derive(Debug, Default, Clone, Readable, Writable)]
pub struct TemplateCache<Template> {
    pub template: ModuleResource<Template>,
    pub instantiated: HashSet<CXTemplateInput>
}

#[derive(Debug, Default, Clone, Readable, Writable)]
pub struct GenericData<Standard, Template> {
    pub standard: GenericMap<Standard>,
    pub templates: HashMap<String, TemplateCache<Template>>,
}

pub type GenericMap<Type> = HashMap<String, Type>;

pub type CXTypeData = GenericData<CXType, CXTypeTemplate>;
pub type CXFnData = GenericData<CXFunctionPrototype, CXFunctionTemplate>;

pub type CXTypeMap = GenericMap<CXType>;
pub type CXFnMap = GenericMap<CXFunctionPrototype>;

impl<Type, TemplatedType> GenericData<Type, TemplatedType> {
    pub fn new() -> Self {
        GenericData {
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

    pub fn get_template_mut(&mut self, name: &str) -> Option<&mut TemplateCache<TemplatedType>> {
        self.templates
            .get_mut(name)
    }
}
