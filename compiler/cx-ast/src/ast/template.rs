use cx_util::identifier::CXIdent;

use crate::ast::types::CXType;

#[derive(Debug, Clone, Hash, PartialEq, Eq)]
pub struct CXTemplateInput {
    pub params: Vec<CXType>,
}

#[derive(Debug, Default, Clone, PartialEq, Eq, Hash)]
pub struct CXTemplatePrototype {
    pub types: Vec<CXIdent>,
}

