use crate::preparse::naive_types::{CXNaivePrototype, CXNaiveType, ModuleResource};
use crate::preparse::templates::{CXFunctionTemplate, CXTypeTemplate};
use cx_util::identifier::CXIdent;
use cx_util::mangling::{mangle_destructor, mangle_member_function};
use speedy::{Readable, Writable};
use std::collections::HashMap;

mod format;
pub mod naive_types;
pub mod templates;

#[derive(Debug, Clone, Readable, Writable)]
pub struct GenericNaiveMap<Standard, Template> {
    pub standard: HashMap<String, ModuleResource<Standard>>,
    pub templates: HashMap<String, ModuleResource<Template>>,
}

pub type CXNaiveTypeMap = GenericNaiveMap<CXNaiveType, CXTypeTemplate>;
pub type CXNaiveFnMap = GenericNaiveMap<CXNaivePrototype, CXFunctionTemplate>;

impl<Standard, Template> Default for GenericNaiveMap<Standard, Template> {
    fn default() -> Self {
        Self::new()
    }
}

impl<Standard, Template> GenericNaiveMap<Standard, Template> {
    pub fn new() -> Self {
        GenericNaiveMap {
            standard: HashMap::new(),
            templates: HashMap::new(),
        }
    }

    pub fn insert_standard(&mut self, name: String, item: ModuleResource<Standard>) {
        self.standard.insert(name, item);
    }

    pub fn insert_template(&mut self, name: String, item: ModuleResource<Template>) {
        self.templates.insert(name, item);
    }

    pub fn get(&self, name: &str) -> Option<&ModuleResource<Standard>> {
        self.standard.get(name)
    }

    pub fn get_template(&self, name: &str) -> Option<&ModuleResource<Template>> {
        self.templates.get(name)
    }
}

#[derive(Debug, Clone, PartialEq, Eq, Readable, Writable)]
pub enum CXNaiveFnIdent {
    Standard(CXIdent),
    MemberFunction {
        _type: CXNaiveType,
        function_name: CXIdent,
    },
    Destructor(CXIdent),
}

impl CXNaiveFnIdent {
    pub fn mangle(&self) -> String {
        match self {
            CXNaiveFnIdent::Standard(name) => name.to_string(),
            CXNaiveFnIdent::MemberFunction {
                _type,
                function_name,
            } => {
                let Some(name) = _type.get_name() else {
                    unreachable!("Member function's type must have a name");
                };

                mangle_member_function(name.to_string(), function_name.as_str())
            }
            CXNaiveFnIdent::Destructor(name) => mangle_destructor(name.as_str()),
        }
    }
}
