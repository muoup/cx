use speedy::{Readable, Writable};
use crate::parse::identifier::CXIdent;
use crate::preparse::naive_types::{CXNaivePrototype, CXNaiveType};

#[derive(Debug, Default, Clone, Hash, Readable, Writable)]
pub struct CXTemplatePrototype {
    pub types: Vec<String>
}

#[derive(Debug, Default, Clone, Readable, Writable)]
pub struct CXTemplate<Shell> {
    pub name: CXIdent,
    pub prototype: CXTemplatePrototype,
    pub shell: Shell,
}

pub type CXTypeTemplate = CXTemplate<CXNaiveType>;
pub type CXFunctionTemplate = CXTemplate<CXNaivePrototype>;
pub type CXDestructorTemplate = CXTemplate<String>;