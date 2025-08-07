use std::collections::HashMap;
use cx_data_lexer::token::Token;
use crate::parse::ast::CXAST;
use crate::parse::maps::{CXFunctionMap, CXTypeMap};
use crate::preparse::{CXNaiveFunctionMap, CXNaiveFunctionTemplates, CXNaiveTypeMap, CXNaiveTypeTemplates};
use crate::preparse::pp_type::{CXFunctionTemplate, CXNaivePrototype, CXNaiveType};

pub mod parse;
pub mod preparse;

pub type PreprocessContents = String;
pub type LexContents = Vec<Token>;

#[derive(Debug, Default, Clone)]
pub struct PreparseContents {
    pub destructor_definitions: Vec<String>,
    pub imports: Vec<String>,

    pub type_definitions: CXNaiveTypeMap,
    pub function_definitions: CXNaiveFunctionMap,

    pub type_templates: CXNaiveTypeTemplates,
    pub function_templates: CXNaiveFunctionTemplates
}

pub type ParseContents = CXAST;
