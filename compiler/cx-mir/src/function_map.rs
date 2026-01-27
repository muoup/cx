use std::collections::HashMap;

use cx_ast::data::{CXFunctionKey, CXFunctionTemplate, ModuleResource};
use speedy::{Readable, Writable};

use crate::mir::types::MIRFunctionPrototype;

pub type CXFnMap = HashMap<String, MIRFunctionPrototype>;

#[derive(Debug, Default, Clone, Readable, Writable)]
pub struct CXFnData {
    map: CXFnMap,
    templates: HashMap<CXFunctionKey, ModuleResource<CXFunctionTemplate>>,
}

impl CXFnData {
    pub fn new() -> Self {
        CXFnData {
            map: HashMap::new(),
            templates: HashMap::new(),
        }
    }

    pub fn insert_standard(&mut self, prototype: MIRFunctionPrototype) {
        self.map.insert(prototype.name.to_string(), prototype);
    }

    pub fn insert_template(
        &mut self,
        name: CXFunctionKey,
        template: ModuleResource<CXFunctionTemplate>,
    ) {
        self.templates.insert(name, template);
    }

    pub fn iter(&self) -> impl Iterator<Item = (&String, &MIRFunctionPrototype)> {
        self.map.iter()
    }

    pub fn standard_fns(&self) -> impl Iterator<Item = &MIRFunctionPrototype> {
        self.map.values()
    }

    pub fn get(&self, name: &str) -> Option<&MIRFunctionPrototype> {
        self.map.get(name)
    }

    pub fn get_template(
        &self, name: &CXFunctionKey,
    ) -> Option<&ModuleResource<CXFunctionTemplate>> {
        self.templates.get(name)
    }
}
