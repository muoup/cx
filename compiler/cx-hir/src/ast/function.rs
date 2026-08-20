use cx_tokens::TokenRange;
use cx_util::{identifier::CXIdent, namespace::QualifiedName};

use crate::ast::{
    expression::HIRExpression,
    modifiers::{HIRSymbolNameScheme, LinkageMode},
    types::HIRType,
};

#[derive(Debug, Clone, Default, Hash, PartialEq, Eq)]
pub struct HIRFunctionContract {
    pub safe: bool,
    pub noreturn: bool,

    pub precondition: Option<HIRExpression>,
    pub postcondition: Option<(Option<CXIdent>, HIRExpression)>,
}

#[derive(Debug, Clone, Hash, PartialEq, Eq)]
pub struct HIRFunctionPrototype {
    pub kind: HIRFunctionKind,

    pub return_type: HIRType,
    pub params: Vec<HIRParameter>,
    pub var_args: bool,
    pub contract: HIRFunctionContract,

    pub linkage: LinkageMode,
    pub symbol_naming: HIRSymbolNameScheme,
    pub range: TokenRange,
}

#[derive(Debug, Clone, Hash, PartialEq, Eq)]
pub struct HIRParameter {
    pub name: Option<CXIdent>,
    pub _type: HIRType,
}

#[derive(Debug, Clone, Hash, PartialEq, Eq)]
pub struct HIRComptimeFnPrototype {
    pub kind: HIRFunctionKind,
    pub params: Vec<HIRComptimeParameter>,
    pub return_type: HIRComptimeValueType,
    pub range: TokenRange,
}

#[derive(Debug, Clone, Hash, PartialEq, Eq)]
pub struct HIRComptimeParameter {
    pub name: Option<CXIdent>,
    pub value_type: HIRComptimeValueType,
}

#[derive(Debug, Clone, Hash, PartialEq, Eq)]
pub struct HIRComptimeValueType {
    pub expr: bool,
    pub params: Vec<HIRType>,
    pub _type: HIRType,
}

#[derive(Debug, Clone, Hash, PartialEq, Eq)]
pub enum HIRFunctionKind {
    Standard(CXIdent),
    AssociatedFunction { namespace: CXIdent, name: CXIdent },
}

impl HIRFunctionKind {
    pub fn into_key(&self) -> QualifiedName {
        match self {
            HIRFunctionKind::Standard(name) => QualifiedName::new_raw(name.clone()),
            HIRFunctionKind::AssociatedFunction { namespace, name } => {
                QualifiedName::new_raw(namespace.clone()).child(name.clone())
            }
        }
    }
}
