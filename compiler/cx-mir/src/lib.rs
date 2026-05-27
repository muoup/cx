pub mod function_map;
pub mod intrinsic_types;
pub mod symbols;

pub mod mir;

use cx_ast::data::{CXTypeTemplate, ModuleResource};
use speedy::{Readable, Writable};
use std::collections::HashMap;

use crate::mir::{data::MIRTemplateInput, r#type::MIRType};

#[derive(Debug, Default, Clone, Readable, Writable)]
pub struct TemplateCache<Template> {
    pub template: ModuleResource<Template>,
    pub instantiated: Vec<MIRTemplateInput>,
}

#[derive(Debug, Default, Clone, Readable, Writable)]
pub struct GenericData<Standard, Template> {
    pub standard: GenericMap<Standard>,
    pub templates: HashMap<String, TemplateCache<Template>>,
}

pub type GenericMap<Type> = HashMap<String, Type>;

pub type CXTypeData = GenericData<MIRType, CXTypeTemplate>;
pub type CXTypeMap = GenericMap<MIRType>;

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
                instantiated: Vec::new(),
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
