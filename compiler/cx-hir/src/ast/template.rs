use cx_util::identifier::CXIdent;

use crate::ast::types::HIRType;

#[derive(Debug, Clone, Hash, PartialEq, Eq)]
pub struct HIRTemplateInput {
    pub params: Vec<HIRType>,
}

#[derive(Debug, Default, Clone, PartialEq, Eq, Hash)]
pub struct HIRTemplatePrototype {
    pub types: Vec<CXIdent>,
}
