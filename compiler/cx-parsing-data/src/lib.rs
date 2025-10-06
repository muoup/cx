use crate::parse::ast::CXAST;
use crate::preparse::{CXNaiveFnMap, CXNaiveTypeMap};
use speedy::{Readable, Writable};

pub mod parse;
pub mod preparse;

#[derive(Debug, Default, Clone, Readable, Writable)]
pub struct PreparseContents {
    pub module: String,
    pub imports: Vec<String>,

    pub type_definitions: CXNaiveTypeMap,
    pub function_definitions: CXNaiveFnMap,
}

pub type ParseContents = CXAST;
