use std::collections::HashMap;
use cx_util::CXResult;
use crate::parse::ast::CXFunctionPrototype;
use crate::parse::value_type::{CXType, CXTypeKind};

#[derive(Debug)]
pub struct CXTemplate {
    pub generic_types: Vec<String>,
    pub generator: CXTemplateGenerator,
    pub generated: HashMap<CXTemplateInput, CXTemplateOutput>
}

#[derive(Debug)]
pub enum CXTemplateGenerator {
    TypeGen(CXType),
    FunctionGen(CXFunctionPrototype)
}

#[derive(Debug, Hash, PartialEq, Eq)]
pub struct CXTemplateInput {
    pub types: Vec<CXType>
}

#[derive(Debug, Clone)]
pub enum CXTemplateOutput {
    Type(CXType),
    FunctionPrototype(CXFunctionPrototype)
}

impl CXTemplate {
    pub fn generate(&mut self, input: CXTemplateInput) -> CXResult<CXTemplateOutput> {
        if let Some(output) = self.generated.get(&input) {
            return Some(output.clone());
        }
        
        let output = self.generate_unresolved(&input)?;
        self.generated.insert(input, output.clone());
        
        return Some(output);
    }
    
    fn generate_unresolved(&mut self, input: &CXTemplateInput) -> CXResult<CXTemplateOutput> {
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
            self.resolve_type_generics(&mut param.type_, input)?;
        }
        
        self.resolve_type_generics(&mut prototype.return_type, input)?;
        
        Some(())
    }
    
    fn resolve_type_generics(&self, ty: &mut CXType, input: &CXTemplateInput) -> CXResult<()> {
        let generic_map = input.types.iter()
            .zip(self.generic_types.iter())
            .map(|(ty, name)| (name.clone(), ty.clone()))
            .collect::<HashMap<String, CXType>>();
    
        Self::rtg_recursive(ty, &generic_map)
    }
    
    fn rtg_recursive(ty: &mut CXType, generic_map: &HashMap<String, CXType>) -> CXResult<()> {
        match ty.kind {
            CXTypeKind::Identifier { ref name, .. } =>
                if let Some(replacement) = generic_map.get(name.as_str()) {
                    *ty = replacement.clone();
                },
            
            CXTypeKind::Array { _type: ref mut inner, .. } |
            CXTypeKind::PointerTo { ref mut inner, .. } =>
                Self::rtg_recursive(inner, generic_map)?,

            _ => todo!()
        }
        
        Some(())
    }
}