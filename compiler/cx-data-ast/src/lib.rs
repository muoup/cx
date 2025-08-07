use cx_data_lexer::token::Token;
use crate::parse::ast::CXAST;
use crate::parse::maps::{CXFunctionMap, CXTypeMap};

pub mod parse;
pub mod preparse;

pub type PreprocessContents = String;
pub type LexContents = Vec<Token>;

#[derive(Debug, Default)]
pub struct PreparseContents {
    pub type_definitions: CXTypeMap,
    pub function_definitions: CXFunctionMap,
    pub destructor_definitions: Vec<String>,
    pub imports: Vec<String>,
}

pub type ParseContents = CXAST;
