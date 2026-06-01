use cx_util::identifier::CXIdent;

use crate::ast::{expression::CXExpression, modifiers::CXLinkageMode, types::CXType};

#[derive(Debug, Clone, PartialEq, Eq, Hash)]
pub enum CXGlobalVariable {
    EnumDefinition {
        variants: Vec<CXEnumVariant>,
    },

    Standard {
        _type: CXType,
        is_mutable: bool,
        linkage: CXLinkageMode,
        initializer: Option<CXExpression>,
    },
}

#[derive(Debug, Clone, PartialEq, Eq, Hash)]
pub struct CXEnumVariant {
    pub name: CXIdent,
    pub value: Option<CXExpression>,
}
