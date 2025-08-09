use std::collections::HashMap;
use crate::parse::parser::VisibilityMode;
use crate::preparse::pp_type::{CXFunctionTemplate, CXNaivePrototype, CXNaiveType, CXTypeTemplate};

pub mod pp_type;
mod format;

pub type CXNaiveTypeMap = HashMap<String, CXNaiveType>;
pub type CXNaiveFunctionMap = Vec<(String, VisibilityMode, CXNaivePrototype)>;

pub type CXNaiveTypeTemplates = Vec<CXTypeTemplate>;
pub type CXNaiveFunctionTemplates = Vec<CXFunctionTemplate>;