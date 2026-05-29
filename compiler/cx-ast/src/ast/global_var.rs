use cx_util::identifier::CXIdent;

use crate::ast::{expression::CXExpression, types::CXType};

#[derive(Debug, Clone, PartialEq, Eq, Hash)]
pub enum CXGlobalVariable {
    EnumDefinition {
        variants: Vec<CXEnumVariant>,
    },

    Standard {
        _type: CXType,
        is_mutable: bool,
        initializer: Option<CXExpression>,
    },
}

#[derive(Debug, Clone, PartialEq, Eq, Hash)]
pub struct CXEnumVariant {
    pub name: CXIdent,
    pub value: Option<CXExpression>,
}
