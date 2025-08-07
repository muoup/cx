use std::collections::HashMap;
use std::sync::RwLock;
use speedy::{Readable, Writable};
use cx_util::CXResult;
use cx_util::mangling::{mangle_templated_fn, mangle_templated_type};
use cx_util::rwlockser::RwLockSer;
use crate::parse::ast::{CXFunctionPrototype, CXGlobalStmt, CXAST};
use crate::parse::template::{CXTemplateTypeGen, CXTemplateInput, CXTemplateOutput, CXTemplateGenerator};
use crate::parse::value_type::CXType;

#[derive(Debug, Default, Clone, Readable, Writable)]
pub struct CXMap<Output, Generator> {
    types: HashMap<String, Output>,

    pub templated_types: HashMap<String, Generator>
}

pub type GlobalExpressionContainer = Vec<CXGlobalStmt>;

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
    
    pub fn template_iter(&self) -> impl Iterator<Item = (&String, &CXTemplateTypeGen)> {
        self.templated_types.iter()
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
    
    pub fn extend_templates(&mut self, other: &Self) {
        for (name, template) in other.templated_types.iter() {
            let new_template = CXTemplateTypeGen {
                module_origin: template.module_origin.clone(),
                generic_types: template.generic_types.clone(),
                generator: template.generator.clone(),
                generated: RwLockSer::new(HashMap::new())
            };
            
            self.templated_types.insert(name.clone(), new_template);
        }
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

    pub fn get_generator(&self, name: &str) -> Option<&CXTemplateTypeGen> {
        self.templated_types.get(name)
    }
    
    pub fn get_generator_mut(&mut self, name: &str) -> Option<&mut CXTemplateTypeGen> {
        self.templated_types.get_mut(name)
    }
    
    pub fn template_source(&self, name: &str) -> Option<String> {
        self.templated_types.get(name)
            .and_then(|template| template.module_origin.clone())
    }
    
    pub fn has_template(&self, name: &str) -> bool {
        self.templated_types.contains_key(name)
    }
    
    pub fn insert_template(&mut self, name: String, template: CXTemplateTypeGen) {
        self.templated_types.insert(name, template);
    }
}

pub type CXTypeMap = CXMap<CXType>;
pub type CXFunctionMap = CXMap<CXFunctionPrototype>;

#[derive(Debug, Clone, Readable, Writable)]
pub struct CXTemplateRequest {
    pub template_name: String,
    pub input: CXTemplateInput
}

impl CXTypeMap {
    pub fn get_template(&self, name: &str, template_input: &CXTemplateInput) -> CXResult<CXType> {
        let template = self.templated_types.get(name)?;
        let output = template.generate(name, template_input)?;

        match output {
            CXTemplateOutput::Type(ty) => Some(ty),

            _ => unreachable!("Expected a type output from template generation, found a different type"),
        }
    }
    
    pub fn get_existing_template(&self, name: &str, template_input: &CXTemplateInput) -> Option<&CXType> {
        let mangled_name = mangle_templated_type(name, &template_input.params);
        
        self.get(&mangled_name)
    }
}

impl CXFunctionMap {
    pub fn get_template(&mut self, name: &str, template_input: &CXTemplateInput) -> CXResult<CXFunctionPrototype> {
        self.templated_types.get(name)
            .and_then(|template| template.generate(name, template_input))
            .and_then(|output| match output {
                CXTemplateOutput::FunctionPrototype(fn_proto) => Some(fn_proto),
                _ => None,
            })
    }
}