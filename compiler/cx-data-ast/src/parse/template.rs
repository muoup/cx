use crate::parse::ast::CXFunctionPrototype;
use crate::parse::value_type::CXType;
use crate::preparse::pp_type::{CXFunctionTemplate, CXNaiveType, CXNaiveTypeKind, CXTypeTemplate};
use cx_util::rwlockser::RwLockSer;
use cx_util::CXResult;
use speedy::{Readable, Writable};
use std::collections::HashMap;

#[derive(Debug, Clone, Readable, Writable)]
pub struct CXTemplateTypeGen<Generator> {
    pub module_origin: Option<String>,
    pub generic_types: Vec<String>,
    pub generator: Generator
}

#[derive(Debug, Clone, Readable, Writable)]
pub struct CXTypeGenerator {
    template: CXTypeTemplate,
    generated: RwLockSer<HashMap<CXTemplateInput, CXTemplateOutput>>
}

impl From<CXTypeTemplate> for CXTemplateTypeGen<CXTypeGenerator> {
    fn from(template: CXTypeTemplate) -> Self {
        CXTemplateTypeGen {
            module_origin: None,
            generic_types,
            generator: CXTypeGenerator {
                template,

                ..Default::default()
            }
        }
    }
}

#[derive(Debug, Clone, Readable, Writable)]
pub struct CXFunctionGenerator {
    template: CXFunctionTemplate,
    generated: RwLockSer<HashMap<CXTemplateInput, CXTemplateOutput>>
}

#[derive(Debug, Hash, PartialEq, Eq, Clone, Readable, Writable)]
pub struct CXTemplateInput {
    pub params: Vec<CXType>
}

#[derive(Debug, Clone, Readable, Writable)]
pub enum CXTemplateOutput {
    Type(CXType),
    FunctionPrototype(CXFunctionPrototype)
}

impl CXTemplateTypeGen {
    pub fn generate(&self, name: &str, input: &CXTemplateInput) -> CXResult<CXTemplateOutput> {
        let read_lock = self.generated.read()
            .unwrap_or_else(|_| panic!("Dead lock occurred while reading template data"));

        if let Some(output) = read_lock.get(input) {
            return Some(output.clone());
        }

        drop(read_lock);

        let mut write_lock = self.generated.write()
            .unwrap_or_else(|_| panic!("Dead lock occurred while writing template data"));

        let output = self.generate_unresolved(input)?;
        write_lock.insert(input.clone(), output);

        Some(write_lock.get(input).expect("Failed to retrieve generated template output").clone())
    }
    
    fn generate_unresolved(&self, input: &CXTemplateInput) -> CXResult<CXTemplateOutput> {
        match &self.generator {
            CXTemplateGenerator::TypeGen(ty) => {
                let mut generated_type = ty.clone();
                self.resolve_type_generics(&mut generated_type, input)?;
                
                Some(
                    CXTemplateOutput::Type(generated_type)
                )
            },
            CXTemplateGenerator::FunctionGen(prototype) => {
                let mut generated_prototype = prototype.clone();
                self.resolve_prototype_generics(&mut generated_prototype, input)?;
             
                Some(
                    CXTemplateOutput::FunctionPrototype(generated_prototype)
                )
            }
        }
    }
    
    fn resolve_prototype_generics(&self, prototype: &mut CXFunctionPrototype, input: &CXTemplateInput) -> CXResult<()> {
        for param in &mut prototype.params {
            self.resolve_type_generics(&mut param._type, input)?;
        }
        
        self.resolve_type_generics(&mut prototype.return_type, input)?;
        
        Some(())
    }
    
    fn resolve_type_generics(&self, ty: &mut CXNaiveType, input: &CXTemplateInput) -> CXResult<()> {
        let generic_map = input.params.iter()
            .zip(self.generic_types.iter())
            .map(|(ty, name)| (name.clone(), ty.clone()))
            .collect::<HashMap<String, CXType>>();
    
        Self::rtg_recursive(ty, &generic_map)
    }
    
    fn rtg_recursive(ty: &mut CXNaiveType, generic_map: &HashMap<String, CXType>) -> CXResult<()> {
        match &mut ty.kind {
            CXNaiveTypeKind::ExplicitSizedArray(inner, ..) |
            CXNaiveTypeKind::ImplicitSizedArray(inner) |
            CXNaiveTypeKind::PointerTo { inner_type: ref mut inner, .. } =>
                Self::rtg_recursive(inner, generic_map)?,

            _ => todo!()
        }
        
        Some(())
    }
}