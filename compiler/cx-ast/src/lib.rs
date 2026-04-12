use cx_util::{identifier::CXIdent, module_path::ModulePath};
use speedy::{Readable, Writable};

use crate::{ast::CXAST, data::ModuleResource};

pub mod ast;
pub mod data;

pub mod type_map;
pub mod macros;

mod format;

#[derive(Debug, Default, Clone, Readable, Writable)]
pub struct PreparseContents {
    pub module: String,
    pub imports: Vec<ModulePath>,
    pub type_idents: Vec<ModuleResource<CXIdent>>,
}

pub type ParseContents = CXAST;
