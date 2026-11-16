use chl_tokens::TokenRange;
use chl_util::identifier::CHLIdent;

use crate::ast::{
    expression::HIRExpression,
    modifiers::{HIRSymbolNameScheme, LinkageMode},
    types::HIRType,
};

#[derive(Debug, Clone, PartialEq, Eq, Hash)]
pub enum HIRGlobalVariable {
    EnumDefinition(HIREnumDefinition),

    Standard {
        comptime: bool,
        name: CHLIdent,
        ty: HIRType,
        is_mutable: bool,
        initializer: Option<HIRExpression>,

        linkage: LinkageMode,
        symbol_name_scheme: HIRSymbolNameScheme,
        range: TokenRange,
    },
}

#[derive(Debug, Clone, PartialEq, Eq, Hash)]
pub struct HIREnumDefinition {
    pub name: Option<CHLIdent>,
    pub variants: Vec<HIREnumVariant>,
    pub symbol_naming: HIRSymbolNameScheme,
}

#[derive(Debug, Clone, PartialEq, Eq, Hash)]
pub struct HIREnumVariant {
    pub name: CHLIdent,
    pub value: Option<HIRExpression>,
    pub range: TokenRange,
}
