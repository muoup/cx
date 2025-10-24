use crate::parse::ast::CXAST;
use crate::preparse::naive_types::ModuleResource;
use cx_util::identifier::CXIdent;
use speedy::{Readable, Writable};

pub mod parse;
pub mod preparse;

#[derive(Debug, Default, Clone, Readable, Writable)]
pub struct PreparseContents {
    pub module: String,
    pub imports: Vec<String>,
    pub type_idents: Vec<ModuleResource<CXIdent>>,
}

pub type ParseContents = CXAST;
