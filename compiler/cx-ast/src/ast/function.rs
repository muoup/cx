use cx_tokens::TokenRange;
use cx_util::{identifier::CXIdent, namespace::QualifiedName};

use crate::ast::{expression::CXExpression, modifiers::CXLinkageMode, types::CXType};

#[derive(Debug, Clone, Default, Hash, PartialEq, Eq)]
pub struct CXFunctionContract {
    pub safe: bool,

    pub precondition: Option<CXExpression>,
    pub postcondition: Option<(Option<CXIdent>, CXExpression)>,
}

#[derive(Debug, Clone, Hash, PartialEq, Eq)]
pub struct CXFunctionPrototype {
    pub kind: CXFunctionKind,
    pub params: Vec<CXParameter>,
    pub return_type: CXType,
    pub var_args: bool,
    pub contract: CXFunctionContract,
    pub linkage: CXLinkageMode,
    pub range: TokenRange,
}

#[derive(Debug, Clone, Hash, PartialEq, Eq)]
pub struct CXParameter {
    pub name: Option<CXIdent>,
    pub _type: CXType,
}

#[derive(Debug, Clone, Hash, PartialEq, Eq)]
pub enum CXFunctionKind {
    Standard(CXIdent),
    MemberFunction {
        member_type: QualifiedName,
        name: CXIdent,
    },
}

impl CXFunctionKind {
    pub fn into_key(&self) -> QualifiedName {
        match self {
            CXFunctionKind::Standard(name) => QualifiedName::new_raw(name.clone()),
            CXFunctionKind::MemberFunction {
                member_type, name, ..
            } => member_type.clone().child(name.clone()),
        }
    }
}
