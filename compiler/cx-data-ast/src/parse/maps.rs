use crate::parse::ast::{CXFunctionPrototype, CXGlobalStmt};
use crate::parse::template::{CXFunctionGenerator, CXTemplateInput, CXTemplateTypeGen, CXTypeGenerator, TemplateGenerator};
use crate::parse::value_type::CXType;
use speedy::{Readable, Writable};
use std::collections::HashMap;

#[derive(Debug, Clone, Readable, Writable)]
pub struct CXMap<Generator: TemplateGenerator> {
    types: HashMap<String, Generator::Output>,

    templated_types: HashMap<String, CXTemplateTypeGen<Generator>>
}

impl<Generator: TemplateGenerator> CXMap<Generator> {
    pub fn new() -> Self {
        CXMap {
            types: HashMap::new(),
            templated_types: HashMap::new(),
        }
    }

    pub fn iter(&self) -> impl Iterator<Item = (&String, &Generator::Output)> {
        self.types.iter()
    }

    pub fn values(&self) -> impl Iterator<Item = &Generator::Output> {
        self.types.values()
    }

    pub fn templates(&self) -> impl Iterator<Item = &CXTemplateTypeGen<Generator>> {
        self.templated_types.values()
    }

    pub fn keys(&self) -> impl Iterator<Item = &String> {
        self.types.keys()
    }
    
    pub fn template_args(&self, name: &str) -> Option<&[String]> {
        self.templated_types.get(name)
            .map(|template| template.generator.args())
    }

    pub fn contains_key(&self, name: &str) -> bool {
        self.types.contains_key(name)
    }

    pub fn insert(&mut self, name: String, ty: Generator::Output) -> Option<Generator::Output> {
        self.types.insert(name, ty)
    }

    pub fn remove(&mut self, name: &str) -> Option<Generator::Output> {
        self.types.remove(name)
    }

    pub(crate) fn get_generator(&self, name: &str) -> Option<&CXTemplateTypeGen<Generator>> {
        self.templated_types.get(name)
    }

    pub fn get(&self, name: &str) -> Option<&Generator::Output> {
        self.types.get(name)
    }

    pub fn get_mut(&mut self, name: &str) -> Option<&mut Generator::Output> {
        self.types.get_mut(name)
    }

    pub fn get_template(&self, map: &CXTypeMap, name: &str, input: CXTemplateInput) -> Option<Generator::Output> {
        self.templated_types.get(name)
            .and_then(|template| template.generator.get_template(map, input))
    }

    pub fn template_source(&self, name: &str) -> Option<String> {
        self.templated_types.get(name)
            .and_then(|template| template.module_origin.clone())
    }

    pub fn has_template(&self, name: &str) -> bool {
        self.templated_types.contains_key(name)
    }

    pub fn insert_template(&mut self, name: String, template: CXTemplateTypeGen<Generator>) {
        self.templated_types.insert(name, template);
    }
}

impl<Generator: TemplateGenerator> Default for CXMap<Generator> {
    fn default() -> Self {
        CXMap {
            types: HashMap::new(),
            templated_types: HashMap::new()
        }
    }
}

pub type CXTypeMap = CXMap<CXTypeGenerator>;
pub type CXFunctionMap = CXMap<CXFunctionGenerator>;
pub type CXDestructorMap = HashMap<CXType, String>;

#[derive(Debug, Clone, Readable, Writable)]
pub struct CXTemplateRequest {
    pub template_name: String,
    pub input: CXTemplateInput
}