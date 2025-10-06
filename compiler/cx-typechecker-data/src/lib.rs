pub mod ast;
pub mod cx_types;
pub mod intrinsic_types;
pub mod function_map;

mod format;

use crate::cx_types::CXTemplateInput;
use cx_parsing_data::preparse::naive_types::ModuleResource;
use cx_parsing_data::preparse::templates::CXTypeTemplate;
use cx_types::CXType;
use speedy::{Readable, Writable};
use std::collections::{HashMap, HashSet};

#[derive(Debug, Default, Clone, Readable, Writable)]
pub struct TemplateCache<Template> {
    pub template: ModuleResource<Template>,
    pub instantiated: HashSet<CXTemplateInput>,
}

#[derive(Debug, Default, Clone, Readable, Writable)]
pub struct GenericData<Standard, Template> {
    pub standard: GenericMap<Standard>,
    pub templates: HashMap<String, TemplateCache<Template>>,
}

pub type GenericMap<Type> = HashMap<String, Type>;

pub type CXTypeData = GenericData<CXType, CXTypeTemplate>;
pub type CXTypeMap = GenericMap<CXType>;

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
        self.templates.insert(
            name,
            TemplateCache {
                template: item,
                instantiated: HashSet::new(),
            },
        );
    }

    pub fn get(&self, name: &str) -> Option<&Type> {
        self.standard.get(name)
    }

    pub fn get_template(&self, name: &str) -> Option<&TemplateCache<TemplatedType>> {
        self.templates.get(name)
    }

    pub fn get_template_mut(&mut self, name: &str) -> Option<&mut TemplateCache<TemplatedType>> {
        self.templates.get_mut(name)
    }
}
