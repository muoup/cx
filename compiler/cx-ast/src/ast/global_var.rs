use cx_util::identifier::CXIdent;

use crate::ast::{
    expression::CXExpression,
    modifiers::{CXLinkageMode, CXSymbolNameScheme},
    types::CXType,
};

#[derive(Debug, Clone, PartialEq, Eq, Hash)]
pub enum CXGlobalVariable {
    EnumDefinition(CXEnumDefinition),

    Standard {
        name: CXIdent,
        _type: CXType,
        is_mutable: bool,
        initializer: Option<CXExpression>,

        linkage: CXLinkageMode,
        symbol_name_scheme: CXSymbolNameScheme,
    },
}

#[derive(Debug, Clone, PartialEq, Eq, Hash)]
pub struct CXEnumDefinition {
    pub name: Option<CXIdent>,
    pub variants: Vec<CXEnumVariant>,
}

#[derive(Debug, Clone, PartialEq, Eq, Hash)]
pub struct CXEnumVariant {
    pub name: CXIdent,
    pub value: Option<CXExpression>,
}
