use std::collections::HashMap;

use cx_parsing_data::preparse::{naive_types::ModuleResource, templates::CXFunctionTemplate};
use cx_util::identifier::CXIdent;
use speedy::{Readable, Writable};

use crate::cx_types::{CXFunctionPrototype, CXType};

pub type CXFnBaseMap = HashMap<String, CXFunctionPrototype>;

#[derive(Debug, Default, Clone, Readable, Writable)]
pub struct CXFnMap {
    map: CXFnBaseMap,
    templates: HashMap<String, ModuleResource<CXFunctionTemplate>>,
}

impl CXFnMap {
    pub fn new() -> Self {
        CXFnMap {
            map: HashMap::new(),
            templates: HashMap::new(),
        }
    }

    pub fn insert_standard(&mut self, name: String, prototype: CXFunctionPrototype) {
        self.map.insert(name, prototype);
    }
    
    pub fn insert_template(&mut self, name: String, template: ModuleResource<CXFunctionTemplate>) {
        self.templates.insert(name, template);
    }
    
    pub fn contains_generated(&self, name: &str) -> bool {
        self.map.contains_key(name)
    }
    
    pub fn get(&self, name: &str) -> Option<&CXFunctionPrototype> {
        self.map.get(name)
    }

    pub fn get_template(&self, name: &str) -> Option<&ModuleResource<CXFunctionTemplate>> {
        self.templates.get(name)
    }
}

#[derive(Debug, Clone, Readable, Writable)]
pub struct CXFunctionIdentifier {
    is_templated: bool,
    kind: CXFunctionKind,
}

impl Default for CXFunctionIdentifier {
    fn default() -> Self {
        CXFunctionIdentifier {
            is_templated: false,
            kind: CXFunctionKind::Standard {
                name: CXIdent::from(""),
            },
        }
    }
}

impl From<CXFunctionKind> for CXFunctionIdentifier {
    fn from(kind: CXFunctionKind) -> Self {
        CXFunctionIdentifier {
            is_templated: false,
            kind,
        }
    }
}

#[derive(Debug, Clone, Readable, Writable)]
pub enum CXFunctionKind {
    Standard { name: CXIdent },
    Member { base_type: CXType, name: CXIdent },
    Destructor { base_type: CXType },
    Deconstructor { base_type: CXType }
}

impl CXFunctionIdentifier {
    fn base_mangle(&self) -> String {
        match &self.kind {
            CXFunctionKind::Standard { name } => name.as_string(),
            CXFunctionKind::Member { base_type, name } => {
                let base_name = base_type.get_identifier()
                    .unwrap()
                    .as_str();
                format!("_M{}_{}", base_name, name.as_str())
            }
            CXFunctionKind::Destructor { base_type } => {
                let base_name = base_type.get_identifier()
                    .unwrap()
                    .as_str();
                format!("_D{}", base_name)
            }
            CXFunctionKind::Deconstructor { base_type } => {
                let base_name = base_type.get_identifier()
                    .unwrap()
                    .as_str();
                format!("_DC{}", base_name)
            }
        }
    }
    
    fn template_mangle(&self, prototype: &CXFunctionPrototype) -> String {
        if !self.is_templated {
            return String::new();
        }
        
        let mut mangled = String::from("_t");
        mangled.push_str(prototype.return_type.mangle().as_str());
        
        for param in &prototype.params {
            mangled.push_str(param._type.mangle().as_str());
        }
        
        mangled
    }
    
    pub fn mangle(&self, prototype: &CXFunctionPrototype) -> String {
        format!("{}{}", self.template_mangle(prototype), self.base_mangle())
    }
    
    pub fn set_templated(&mut self) {
        self.is_templated = true;
    }
    
    pub fn implicit_member(&self) -> Option<&CXType> {
        match &self.kind {
            CXFunctionKind::Member { base_type, .. } => Some(base_type),
            CXFunctionKind::Destructor { base_type } => Some(base_type),
            CXFunctionKind::Deconstructor { base_type } => Some(base_type),
            _ => None,
        }
    }
}