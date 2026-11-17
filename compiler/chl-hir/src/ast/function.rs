use chl_namespace::module::QualifiedName;
use chl_tokens::TokenRange;
use chl_util::{identifier::CHLIdent};

use crate::ast::{
    expression::HIRExpression,
    modifiers::{HIRSymbolNameScheme, LinkageMode},
    types::HIRType,
};

#[derive(Debug, Clone, Hash, PartialEq, Eq)]
pub enum HIRFunctionBody {
    Block {
        statements: Box<[HIRExpression]>,
        range: TokenRange,
    },
    Expression(HIRExpression),
    TypeAlias(HIRType),
}

impl HIRFunctionBody {
    pub fn token_range(&self) -> &TokenRange {
        match self {
            Self::Block { range, .. } => range,
            Self::Expression(expression) => &expression.range,
            Self::TypeAlias(ty) => &ty.range,
        }
    }
}

#[derive(Debug, Clone, Default, Hash, PartialEq, Eq)]
pub struct HIRFunctionContract {
    pub safe: bool,

    pub precondition: Option<HIRExpression>,
    pub postcondition: Option<(Option<CHLIdent>, HIRExpression)>,
}

#[derive(Debug, Clone, Hash, PartialEq, Eq)]
pub struct HIRFunctionPrototype {
    pub kind: HIRFunctionKind,
    pub comptime: bool,

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
    pub name: Option<CHLIdent>,
    pub ty: HIRType,
    pub comptime: bool,
    pub default: Option<HIRExpression>,
    pub range: TokenRange,
}

#[derive(Debug, Clone, Hash, PartialEq, Eq)]
pub enum HIRFunctionKind {
    Standard(CHLIdent),
    AssociatedFunction { namespace: CHLIdent, name: CHLIdent },
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
