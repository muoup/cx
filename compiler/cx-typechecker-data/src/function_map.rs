use std::collections::HashMap;

use cx_parsing_data::data::{CXFunctionTemplate, ModuleResource};
use cx_util::identifier::CXIdent;
use speedy::{Readable, Writable};

use crate::cx_types::{CXFunctionPrototype, CXType};

pub type CXFnMap = HashMap<CXFunctionIdentifier, CXFunctionPrototype>;

#[derive(Debug, Default, Clone, Readable, Writable)]
pub struct CXFnData {
    map: CXFnMap,
    templates: HashMap<CXFunctionIdentifier, ModuleResource<CXFunctionTemplate>>,
}

impl CXFnData {
    pub fn new() -> Self {
        CXFnData {
            map: HashMap::new(),
            templates: HashMap::new(),
        }
    }

    pub fn insert_standard(&mut self, prototype: CXFunctionPrototype) {
        self.map.insert(prototype.name.clone(), prototype);
    }

    pub fn insert_template(
        &mut self,
        name: CXFunctionIdentifier,
        template: ModuleResource<CXFunctionTemplate>,
    ) {
        self.templates.insert(name, template);
    }
    
    pub fn iter(&self) -> impl Iterator<Item = (&CXFunctionIdentifier, &CXFunctionPrototype)> {
        self.map.iter()
    }
    
    pub fn standard_fns(&self) -> impl Iterator<Item = &CXFunctionPrototype> {
        self.map.values()
    }
    
    pub fn contains_generated(&self, name: &CXFunctionIdentifier) -> bool {
        self.map.contains_key(name)
    }

    pub fn get(&self, name: &CXFunctionIdentifier) -> Option<&CXFunctionPrototype> {
        self.map.get(name)
    }

    pub fn get_template(
        &self,
        name: &CXFunctionIdentifier,
    ) -> Option<&ModuleResource<CXFunctionTemplate>> {
        self.templates.get(name)
    }
}

#[derive(Debug, Clone, PartialEq, Eq, Hash, Readable, Writable)]
pub enum CXFunctionIdentifier {
    Standard { kind: CXFunctionKind },
    Templated { kind: CXFunctionKind, template_prefix: String }
}

impl Default for CXFunctionIdentifier {
    fn default() -> Self {
        CXFunctionIdentifier::Standard {
            kind: CXFunctionKind::Standard {
                name: CXIdent::from(""),
            }
        }
    }
}

impl From<CXFunctionKind> for CXFunctionIdentifier {
    fn from(kind: CXFunctionKind) -> Self {
        CXFunctionIdentifier::Standard {
            kind,
        }
    }
}

#[derive(Debug, Clone, PartialEq, Eq, Hash, Readable, Writable)]
pub enum CXFunctionKind {
    Standard { name: CXIdent },
    Member { base_type: CXIdent, name: CXIdent },
    Destructor { base_type: CXIdent },
    Deconstructor { base_type: CXIdent },
}

impl CXFunctionKind {
    pub fn member_mangle(base_type: &str, name: &str) -> String {
        format!("_M{base_type}_{name}")
    }

    pub fn destructor_mangle(base_type: &str) -> String {
        format!("_D{base_type}")
    }
    
    pub fn destructor_mangle_ty(base_type: &CXType) -> Option<String> {
        let name = Self::destructor_mangle(base_type.get_identifier()?.as_str());
        
        if !base_type.was_template_instantiated() { return Some(name); }
    
        Self::template_mangle(&name, &CXType::unit(), std::iter::once(&base_type.clone().pointer_to()))
            .into()
    }
    
    pub fn deconstructor_mangle(base_type: &str) -> String {
        format!("_DC{base_type}")
    }
    
    pub fn deconstructor_mangle_ty(base_type: &CXType) -> Option<String> {
        let name = Self::deconstructor_mangle(base_type.get_identifier()?.as_str());
        
        if !base_type.was_template_instantiated() { return Some(name); }
        
        Self::template_mangle(&name, &CXType::unit(), std::iter::once(&base_type.clone().pointer_to()))
            .into()
    }
    
    pub fn template_prefix<'a>(return_type: &CXType, params: impl Iterator<Item=&'a CXType>) -> String {
        let mut mangled = String::from("_t");
        
        mangled.push_str(return_type.mangle().as_str());

        for param in params {
            mangled.push_str(param.mangle().as_str());
        }

        mangled
    }
    
    fn template_mangle<'a>(name: &str, return_type: &CXType, params: impl Iterator<Item=&'a CXType>) -> String {
        format!("{}{}", Self::template_prefix(return_type, params), name)
    }
    
    pub fn standard_template_mangle(name: &str, prototype: &CXFunctionPrototype) -> String {
        Self::template_mangle(
            name,
            &prototype.return_type,
            prototype.params.iter().map(|p| &p._type),
        )
    }
}

impl CXFunctionIdentifier {
    fn mangle_kind(kind: &CXFunctionKind) -> String {
        match kind {
            CXFunctionKind::Standard { name } => 
                name.as_string(),
            CXFunctionKind::Member { base_type, name } =>
                CXFunctionKind::member_mangle(base_type.as_str(), name.as_str()),
            CXFunctionKind::Destructor { base_type } =>
                CXFunctionKind::destructor_mangle(base_type.as_str()),
            CXFunctionKind::Deconstructor { base_type } =>
                CXFunctionKind::deconstructor_mangle(base_type.as_str()),
        }
    }
    
    pub fn standardized(&self) -> CXFunctionIdentifier {
        match self {
            CXFunctionIdentifier::Templated { kind, .. } => {
                CXFunctionIdentifier::Standard { kind: kind.clone() }
            },
            CXFunctionIdentifier::Standard { .. } => self.clone()
        }
    }
    
    pub fn template_mangle2<'a>(&mut self, return_type: &CXType, params: impl Iterator<Item=&'a CXType>) {
        if let CXFunctionIdentifier::Standard { kind } = self {
            let mangled_name = CXFunctionKind::template_prefix(return_type, params);
            
            *self = CXFunctionIdentifier::Templated {
                kind: kind.clone(),
                template_prefix: mangled_name,
            };
        }
    }
    
    pub fn template_mangle(&mut self, prototype: &CXFunctionPrototype) {
        self.template_mangle2(&prototype.return_type, prototype.params.iter().map(|p| &p._type));
    }
    
    pub fn mangle(&self) -> String {
        match self {
            CXFunctionIdentifier::Standard { kind } =>
                Self::mangle_kind(kind),
            CXFunctionIdentifier::Templated { kind, template_prefix } =>
                format!("{}{}", template_prefix, Self::mangle_kind(kind)),
        }
    }
}
