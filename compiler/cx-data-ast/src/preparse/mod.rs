use std::collections::HashMap;
use std::hash::DefaultHasher;
use speedy::{Readable, Writable};
use crate::parse::identifier::CXIdent;
use crate::preparse::pp_type::{CXFunctionTemplate, CXNaivePrototype, CXNaiveTemplateInput, CXNaiveType, CXTypeTemplate, ModuleResource};

pub mod pp_type;
mod format;

pub type CXNaiveTypeMap = HashMap<String, ModuleResource<CXNaiveType>>;
pub type CXNaiveFunctionMap = Vec<ModuleResource<CXNaivePrototype>>;

pub type CXNaiveTypeTemplates = HashMap<String, ModuleResource<CXTypeTemplate>>;
pub type CXNaiveFunctionTemplates = Vec<ModuleResource<CXFunctionTemplate>>;

#[derive(Debug, Clone, PartialEq, Eq, Readable, Writable)]
pub enum CXNaiveFnIdent {
    Standard(CXIdent),
    MemberFunction {
        _type: CXNaiveType,
        function_name: CXIdent
    },
    Destructor(CXIdent)
}

impl CXNaiveFnIdent {
    pub fn as_string(&self) -> String {
        match self {
            CXNaiveFnIdent::Standard(name) => name.to_string(),
            CXNaiveFnIdent::MemberFunction { _type, function_name } => {
                format!("_{}_{}", _type, function_name.to_string())
            }
            CXNaiveFnIdent::Destructor(name) => {
                format!("~{}", name.to_string())
            }
        }
    }
}