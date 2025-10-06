use crate::preparse::naive_types::{CXNaivePrototype, CXNaiveTemplateInput, CXNaiveType, CXNaiveTypeKind, ModuleResource, PredeclarationType};
use crate::preparse::templates::{CXFunctionTemplate, CXTypeTemplate};
use cx_util::identifier::CXIdent;
use speedy::{Readable, Writable};
use std::collections::HashMap;

mod format;
pub mod naive_types;
pub mod templates;

pub type NaiveTypeIdent = String;

#[derive(Debug, Clone, Hash, PartialEq, Eq, Readable, Writable)]
pub enum FunctionTypeIdent {
    Standard(CXIdent),
    Templated(CXIdent, CXNaiveTemplateInput)
}

#[derive(Debug, Clone, Hash, PartialEq, Eq, Readable, Writable)]
pub enum NaiveFnIdent {
    Standard(CXIdent),
    MemberFunction {
        _type: FunctionTypeIdent,
        function_name: CXIdent,
    },
    Destructor(FunctionTypeIdent),
}

#[derive(Debug, Clone, Readable, Writable)]
pub struct CXNaiveMap<Identifier: Eq + std::hash::Hash, Standard, Template> {
    standard: HashMap<Identifier, ModuleResource<Standard>>,
    templates: HashMap<Identifier, ModuleResource<Template>>
}

pub type CXNaiveTypeMap = CXNaiveMap<NaiveTypeIdent, CXNaiveType, CXTypeTemplate>;
pub type CXNaiveFnMap = CXNaiveMap<NaiveFnIdent, CXNaivePrototype, CXFunctionTemplate>;

impl<Identifier, Standard, Template> Default for CXNaiveMap<Identifier, Standard, Template> 
    where Identifier: Eq + std::hash::Hash {
    fn default() -> Self {
        Self::new()
    }
}

impl<Identifier, Standard, Template> CXNaiveMap<Identifier, Standard, Template> 
    where Identifier: Eq + std::hash::Hash {
    pub fn new() -> Self {
        Self {
            standard: HashMap::new(),
            templates: HashMap::new(),
        }
    }

    pub fn insert_standard(&mut self, name: Identifier, item: ModuleResource<Standard>) {
        self.standard.insert(name, item);
    }

    pub fn insert_template(&mut self, name: Identifier, item: ModuleResource<Template>) {
        self.templates.insert(name, item);
    }
    
    pub fn remove_standard(&mut self, name: &Identifier) -> Option<(Identifier, ModuleResource<Standard>)> 
        where Identifier: Clone {
        self.standard.remove_entry(name)
    }
    
    pub fn remove_template(&mut self, name: &Identifier) -> Option<(Identifier, ModuleResource<Template>)> 
        where Identifier: Clone {
        self.templates.remove_entry(name)
    }
    
    pub fn standard_iter(&self) -> impl Iterator<Item = (&Identifier, &ModuleResource<Standard>)> {
        self.standard.iter()
    }
    
    pub fn template_iter(&self) -> impl Iterator<Item = (&Identifier, &ModuleResource<Template>)> {
        self.templates.iter()
    }
 
    pub fn get(&self, ident: &Identifier) -> Option<&ModuleResource<Standard>> {
        self.standard.get(ident)
    }

    pub fn get_template(&self, ident: &Identifier) -> Option<&ModuleResource<Template>> {
        self.templates.get(ident)
    }
    
    pub fn is_key_std(&self, ident: &Identifier) -> bool {
        self.standard.contains_key(ident)
    }
    
    pub fn is_key_template(&self, ident: &Identifier) -> bool {
        self.templates.contains_key(ident)
    }
    
    pub fn is_key_any(&self, ident: &Identifier) -> bool {
        self.is_key_std(ident) || self.is_key_template(ident)
    }
}

impl NaiveFnIdent {
    pub fn implicit_member(&self) -> Option<&FunctionTypeIdent> {
        match self {
            NaiveFnIdent::MemberFunction { _type, .. } => Some(_type),
            NaiveFnIdent::Destructor(name) => Some(name),
            NaiveFnIdent::Standard(_) => None,
        }
    }
}

impl FunctionTypeIdent {
    pub fn as_type(&self) -> CXNaiveType {
        match self {
            FunctionTypeIdent::Standard(name) => {
                CXNaiveType::new(0, CXNaiveTypeKind::Identifier { name: name.clone(), predeclaration: PredeclarationType::None })
            }
            FunctionTypeIdent::Templated(name, input) => {
                CXNaiveType::new(0, CXNaiveTypeKind::TemplatedIdentifier { name: name.clone(), input: input.clone() })
            }
        }
    }
    
    pub fn from_type(naive_type: &CXNaiveType) -> Option<Self> {
        match &naive_type.kind {
            CXNaiveTypeKind::Identifier { name, .. } => Some(FunctionTypeIdent::Standard(name.clone())),
            CXNaiveTypeKind::TemplatedIdentifier { name, input } => Some(FunctionTypeIdent::Templated(name.clone(), input.clone())),
            _ => None,
        }
    }
}