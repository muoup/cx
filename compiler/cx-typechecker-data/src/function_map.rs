use std::collections::HashMap;

use cx_parsing_data::preparse::{naive_types::ModuleResource, templates::CXFunctionTemplate};
use cx_util::identifier::CXIdent;
use speedy::{Readable, Writable};

use crate::cx_types::CXFunctionPrototype;

pub type CXFnMap = HashMap<CXFunctionKind, CXFunctionPrototype>;

#[derive(Debug, Default, Clone, Readable, Writable)]
pub struct CXFnData {
    map: CXFnMap,
    templates: HashMap<CXFunctionKind, ModuleResource<CXFunctionTemplate>>,
}

impl CXFnData {
    pub fn new() -> Self {
        CXFnData {
            map: HashMap::new(),
            templates: HashMap::new(),
        }
    }

    pub fn insert_standard(&mut self, prototype: CXFunctionPrototype) {
        self.map.insert(prototype.name.kind.clone(), prototype);
    }

    pub fn insert_template(
        &mut self,
        name: CXFunctionKind,
        template: ModuleResource<CXFunctionTemplate>,
    ) {
        self.templates.insert(name, template);
    }
    
    pub fn iter(&self) -> impl Iterator<Item = (&CXFunctionKind, &CXFunctionPrototype)> {
        self.map.iter()
    }
    
    pub fn standard_fns(&self) -> impl Iterator<Item = &CXFunctionPrototype> {
        self.map.values()
    }
    
    pub fn contains_generated(&self, name: &CXFunctionKind) -> bool {
        self.map.contains_key(name)
    }

    pub fn get(&self, name: &CXFunctionKind) -> Option<&CXFunctionPrototype> {
        self.map.get(name)
    }

    pub fn get_template(
        &self,
        name: &CXFunctionKind,
    ) -> Option<&ModuleResource<CXFunctionTemplate>> {
        self.templates.get(name)
    }
}

#[derive(Debug, Clone, PartialEq, Eq, Hash, Readable, Writable)]
pub struct CXFunctionIdentifier {
    is_templated: bool,
    pub kind: CXFunctionKind,
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

#[derive(Debug, Clone, PartialEq, Eq, Hash, Readable, Writable)]
pub enum CXFunctionKind {
    Standard { name: CXIdent },
    Member { base_type: CXIdent, name: CXIdent },
    Destructor { base_type: CXIdent },
    Deconstructor { base_type: CXIdent },
}

impl CXFunctionKind {
    pub fn member_mangle(base_type: &str, name: &str) -> String {
        format!("_M{}_{}", base_type, name)
    }

    pub fn destructor_mangle(base_type: &str) -> String {
        format!("_D{}", base_type)
    }
    
    pub fn deconstructor_mangle(base_type: &str) -> String {
        format!("_DC{}", base_type)
    }
}

impl CXFunctionIdentifier {
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
        match &self.kind {
            CXFunctionKind::Standard { name } => 
                format!("{}{}", name.as_str(), self.template_mangle(prototype)),
            CXFunctionKind::Member { base_type, name } =>
                CXFunctionKind::member_mangle(base_type.as_str(), name.as_str()),
            CXFunctionKind::Destructor { base_type } =>
                CXFunctionKind::destructor_mangle(base_type.as_str()),
            CXFunctionKind::Deconstructor { base_type } =>
                CXFunctionKind::deconstructor_mangle(base_type.as_str()),
        }
    }

    pub fn set_templated(&mut self) {
        self.is_templated = true;
    }

    pub fn implicit_member(&self) -> Option<&CXIdent> {
        match &self.kind {
            CXFunctionKind::Member { base_type, .. } => Some(base_type),
            CXFunctionKind::Destructor { base_type } => Some(base_type),
            CXFunctionKind::Deconstructor { base_type } => Some(base_type),
            _ => None,
        }
    }
}
