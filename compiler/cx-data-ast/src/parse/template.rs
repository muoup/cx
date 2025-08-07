use std::collections::HashMap;
use speedy::{Readable, Writable};
use cx_util::CXResult;
use cx_util::mangling::mangle_templated_fn;
use crate::parse::ast::{CXFunctionPrototype, CXAST};
use crate::parse::maps::CXTemplateRequest;
use crate::parse::value_type::{CXType, CXTypeKind};

#[derive(Debug, Clone, Readable, Writable)]
pub struct CXTemplateTypeGen {
    pub module_origin: Option<String>,
    
    pub generic_types: Vec<String>,
    pub generator: CXTemplateGenerator,
    pub generated: HashMap<CXTemplateInput, CXTemplateOutput>
}

impl CXTemplateTypeGen {
    pub fn function_template( 
        generic_types: Vec<String>,
        prototype: CXFunctionPrototype
    ) -> Self {
        CXTemplateTypeGen {
            generic_types,
            
            module_origin: None,
            generator: CXTemplateGenerator::FunctionGen(prototype),
            generated: HashMap::new()
        }
    }
    
    pub fn type_template(
        generic_types: Vec<String>,
        ty: CXType
    ) -> Self {
        CXTemplateTypeGen {
            generic_types,
            
            module_origin: None,
            generator: CXTemplateGenerator::TypeGen(ty),
            generated: HashMap::new()
        }
    }
}

#[derive(Debug, Clone, Readable, Writable)]
pub enum CXTemplateGenerator {
    TypeGen(CXType),
    FunctionGen(CXFunctionPrototype)
}

#[derive(Debug, Hash, PartialEq, Eq, Clone, Readable, Writable)]
pub struct CXTemplateInput {
    pub types: Vec<CXType>
}

#[derive(Debug, Clone, Readable, Writable)]
pub enum CXTemplateOutput {
    Type(CXType),
    FunctionPrototype(CXFunctionPrototype)
}

impl CXTemplateTypeGen {
    pub fn generate(&mut self, name: &str, input: &CXTemplateInput) -> CXResult<(Option<CXTemplateRequest>, &CXTemplateOutput)> {
        if !self.generated.contains_key(&input) {
            let output = self.generate_unresolved(input)?;
            
            self.generated.insert(input.clone(), output);
            
            let request = CXTemplateRequest {
                template_name: name.to_string(),
                input: input.clone()
            };
            
            return Some((Some(request), self.generated.get(input).unwrap()));
        }

        self.get_existing(input)
            .map(|output| (None, output))
    }
    
    pub fn get_existing(&self, input: &CXTemplateInput) -> Option<&CXTemplateOutput> {
        self.generated.get(input)
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
            self.resolve_type_generics(&mut param._type, input)?;
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