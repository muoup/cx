use std::collections::HashMap;
use speedy::{Readable, Writable};
use cx_util::CXResult;
use crate::parse::ast::CXFunctionPrototype;
use crate::parse::template::{CXTemplateTypeGen, CXTemplateInput, CXTemplateOutput, CXTemplateGenerator};
use crate::parse::value_type::CXType;

#[derive(Debug, Clone, Default, Readable, Writable)]
pub struct CXMap<Output> {
    types: HashMap<String, Output>,
    templated_types: HashMap<String, CXTemplateTypeGen>
}

impl<Output> CXMap<Output> {
    pub fn new() -> Self {
        CXMap {
            types: HashMap::new(),
            templated_types: HashMap::new(),
        }
    }
    
    pub fn iter(&self) -> impl Iterator<Item = (&String, &Output)> {
        self.types.iter()
    }
    
    pub fn values(&self) -> impl Iterator<Item = &Output> {
        self.types.values()
    }
    
    pub fn keys(&self) -> impl Iterator<Item = &String> {
        self.types.keys()
    }

    pub fn extend<I: IntoIterator<Item = (String, Output)>>(&mut self, iter: I) {
        self.types.extend(iter);
    }
    
    pub fn contains_key(&self, name: &str) -> bool {
        self.types.contains_key(name)
    }
    
    pub fn insert(&mut self, name: String, ty: Output) -> Option<Output> {
        self.types.insert(name, ty)
    }
    
    pub fn remove(&mut self, name: &str) -> Option<Output> {
        self.types.remove(name)
    }

    pub fn get(&self, name: &str) -> Option<&Output> {
        self.types.get(name)
    }

    pub fn get_mut(&mut self, name: &str) -> Option<&mut Output> {
        self.types.get_mut(name)
    }
    
    pub fn insert_template(&mut self, name: String, template: CXTemplateTypeGen) {
        self.templated_types.insert(name, template);
    }
}

pub type CXTypeMap = CXMap<CXType>;
pub type CXFunctionMap = CXMap<CXFunctionPrototype>;

impl CXTypeMap {
    pub fn get_template(&mut self, name: &str, template_input: CXTemplateInput) -> CXResult<CXType> {
        self.templated_types.get_mut(name)
            .and_then(|template| template.generate(template_input))
            .and_then(|output| match output {
                CXTemplateOutput::Type(ty) => Some(ty),
                _ => None,
            })
    }
}

impl CXFunctionMap {
    pub fn get_template(&mut self, name: &str, template_input: CXTemplateInput) -> CXResult<CXFunctionPrototype> {
        self.templated_types.get_mut(name)
            .and_then(|template| template.generate(template_input))
            .and_then(|output| match output {
                CXTemplateOutput::FunctionPrototype(fn_proto) => Some(fn_proto),
                _ => None,
            })
    }
}