use cx_tokens::TokenRange;
use cx_util::{identifier::CXIdent, namespace::QualifiedName};

use crate::ast::{
    expression::CXExpression,
    modifiers::{CXLinkageMode, CXSymbolNameScheme},
    types::CXType,
};

#[derive(Debug, Clone, Default, Hash, PartialEq, Eq)]
pub struct CXFunctionContract {
    pub safe: bool,

    pub precondition: Option<CXExpression>,
    pub postcondition: Option<(Option<CXIdent>, CXExpression)>,
}

#[derive(Debug, Clone, Hash, PartialEq, Eq)]
pub struct CXFunctionPrototype {
    pub kind: CXFunctionKind,

    pub return_type: CXType,
    pub params: Vec<CXParameter>,
    pub var_args: bool,
    pub contract: CXFunctionContract,

    pub linkage: CXLinkageMode,
    pub symbol_naming: CXSymbolNameScheme,
    pub range: TokenRange,
}

#[derive(Debug, Clone, Hash, PartialEq, Eq)]
pub struct CXParameter {
    pub name: Option<CXIdent>,
    pub _type: CXType,
}

#[derive(Debug, Clone, Hash, PartialEq, Eq)]
pub struct CXComptimeFnPrototype {
    pub kind: CXFunctionKind,
    pub params: Vec<CXComptimeParameter>,
    pub return_type: CXComptimeValueType,
    pub range: TokenRange,
}

#[derive(Debug, Clone, Hash, PartialEq, Eq)]
pub struct CXComptimeParameter {
    pub name: Option<CXIdent>,
    pub value_type: CXComptimeValueType,
}

#[derive(Debug, Clone, Hash, PartialEq, Eq)]
pub struct CXComptimeValueType {
    pub expr: bool,
    pub _type: CXType,
}

#[derive(Debug, Clone, Hash, PartialEq, Eq)]
pub enum CXFunctionKind {
    Standard(CXIdent),
    AssociatedFunction { namespace: CXIdent, name: CXIdent },
}

impl CXFunctionKind {
    pub fn into_key(&self) -> QualifiedName {
        match self {
            CXFunctionKind::Standard(name) => QualifiedName::new_raw(name.clone()),
            CXFunctionKind::AssociatedFunction { namespace, name } => {
                QualifiedName::new_raw(namespace.clone()).child(name.clone())
            }
        }
    }
}
