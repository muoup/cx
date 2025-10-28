use cx_util::identifier::CXIdent;
use speedy::{Readable, Writable};

use crate::{ast::CXAST, data::ModuleResource};

pub mod ast;
pub mod data;

pub mod naive_map;
pub mod macros;
pub mod parser;

mod format;

#[derive(Debug, Default, Clone, Readable, Writable)]
pub struct PreparseContents {
    pub module: String,
    pub imports: Vec<String>,
    pub type_idents: Vec<ModuleResource<CXIdent>>,
}

pub type ParseContents = CXAST;
