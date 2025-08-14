use crate::parse::ast::CXFunctionPrototype;
use crate::parse::maps::CXTypeMap;
use crate::parse::type_mapping::{contextualize_fn_prototype, contextualize_type};
use crate::parse::value_type::CXType;
use crate::preparse::pp_type::{CXFunctionTemplate, CXNaiveType, CXTypeTemplate};
use speedy::{Readable, Writable};
use std::collections::HashMap;
use cx_util::mangling::mangle_templated_fn;
use cx_util::rwlockser::RwLockSer;

#[derive(Debug, Clone, Readable, Writable)]
pub struct CXTemplateTypeGen<Generator: TemplateGenerator> {
    pub module_origin: Option<String>,
    pub generator: Generator
}

pub trait TemplateGenerator {
    type Output;

    fn args(&self) -> &[String];
    fn get_template(&self, map: &CXTypeMap, input: CXTemplateInput) -> Option<Self::Output>;
}

#[derive(Debug, Hash, PartialEq, Eq, Clone, Readable, Writable)]
pub struct CXTemplateInput {
    pub params: Vec<CXType>
}

#[derive(Debug, Clone, Readable, Writable)]
pub struct CXTypeGenerator {
    template: CXTypeTemplate,
    generated: RwLockSer<HashMap<CXTemplateInput, CXType>>
}

impl From<CXTypeTemplate> for CXTemplateTypeGen<CXTypeGenerator> {
    fn from(template: CXTypeTemplate) -> Self {
        CXTemplateTypeGen {
            module_origin: None,
            generator: CXTypeGenerator {
                template,
                generated: RwLockSer::new(HashMap::new())
            }
        }
    }
}

#[derive(Debug, Clone, Readable, Writable)]
pub struct CXFunctionGenerator {
    template: CXFunctionTemplate,
    generated: RwLockSer<HashMap<CXTemplateInput, CXFunctionPrototype>>
}

impl From<CXFunctionTemplate> for CXTemplateTypeGen<CXFunctionGenerator> {
    fn from(template: CXFunctionTemplate) -> Self {
        CXTemplateTypeGen {
            module_origin: None,
            generator: CXFunctionGenerator {
                template,
                generated: RwLockSer::new(HashMap::new())
            }
        }
    }
}


impl TemplateGenerator for CXTypeGenerator {
    type Output = CXType;

    fn args(&self) -> &[String] {
        &self.template.inputs
    }
    
    fn get_template(&self, map: &CXTypeMap, input: CXTemplateInput) -> Option<CXType> {
        if let Some(output) = self.generated.read().unwrap().get(&input) {
            return Some(output.clone());
        }

        let mut temp_map = map.clone();

        for (name, ty) in self.template.inputs.iter().zip(&input.params) {
            temp_map.insert(name.clone(), ty.clone());
        }

        let resolved_type = contextualize_type(&temp_map, &self.template.shell)?;

        self.generated.write()
            .unwrap()
            .insert(input.clone(), resolved_type.clone());

        Some(resolved_type)
    }
}

impl TemplateGenerator for CXFunctionGenerator {
    type Output = CXFunctionPrototype;
    
    fn args(&self) -> &[String] {
        &self.template.inputs
    }

    fn get_template(&self, map: &CXTypeMap, input: CXTemplateInput) -> Option<CXFunctionPrototype> {
        if let Some(output) = self.generated.read().unwrap().get(&input) {
            return Some(output.clone());
        }

        let mut temp_map = map.clone();

        for (name, ty) in self.template.inputs.iter().zip(&input.params) {
            temp_map.insert(name.clone(), ty.clone());
        }

        let resolved_prototype = contextualize_fn_prototype(&temp_map, &self.template.shell)?;
        
        self.generated.write()
            .unwrap()
            .insert(input.clone(), resolved_prototype.clone());

        Some(resolved_prototype)
    }
}

impl CXTypeGenerator {
    pub fn get_shell(&self) -> &CXNaiveType {
        &self.template.shell
    }
}

impl CXFunctionGenerator {
    pub fn get_generated(&self) -> Vec<(CXTemplateInput, CXFunctionPrototype)> {
        self.generated.read()
            .unwrap()
            .iter()
            .map(|(input, prototype)| (input.clone(), prototype.clone()))
            .collect()
    }
}
