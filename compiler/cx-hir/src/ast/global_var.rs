use cx_util::identifier::CXIdent;

use crate::ast::{
    expression::HIRExpression,
    modifiers::{HIRSymbolNameScheme, LinkageMode},
    types::HIRType,
};

#[derive(Debug, Clone, PartialEq, Eq, Hash)]
pub enum HIRGlobalVariable {
    EnumDefinition(HIREnumDefinition),

    Standard {
        name: CXIdent,
        _type: HIRType,
        is_mutable: bool,
        initializer: Option<HIRExpression>,

        linkage: LinkageMode,
        symbol_name_scheme: HIRSymbolNameScheme,
    },
}

#[derive(Debug, Clone, PartialEq, Eq, Hash)]
pub struct HIREnumDefinition {
    pub name: Option<CXIdent>,
    pub variants: Vec<HIREnumVariant>,
}

#[derive(Debug, Clone, PartialEq, Eq, Hash)]
pub struct HIREnumVariant {
    pub name: CXIdent,
    pub value: Option<HIRExpression>,
}
