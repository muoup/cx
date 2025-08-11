use std::collections::HashMap;
use crate::preparse::pp_type::{CXFunctionTemplate, CXNaivePrototype, CXNaiveType, CXTypeTemplate, ModuleResource};

pub mod pp_type;
mod format;

pub type CXNaiveTypeMap = HashMap<String, ModuleResource<CXNaiveType>>;
pub type CXNaiveFunctionMap = Vec<(String, ModuleResource<CXNaivePrototype>)>;

pub type CXNaiveTypeTemplates = HashMap<String, ModuleResource<CXTypeTemplate>>;
pub type CXNaiveFunctionTemplates = Vec<ModuleResource<CXFunctionTemplate>>;