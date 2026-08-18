use cx_util::{identifier::CXIdent, linkage::LinkageMode};

use crate::thir::{expression::THIRExpression, r#type::THIRType};

#[derive(Debug, Clone)]
pub struct THIRGlobalVariable {
    pub kind: THIRGlobalVarKind,
    pub is_mutable: bool,
    pub linkage: LinkageMode,
}

#[derive(Debug, Clone)]
pub enum THIRGlobalVarKind {
    StringLiteral {
        name: CXIdent,
        value: String,
    },
    Variable {
        name: CXIdent,
        _type: THIRType,
        initializer: Option<THIRExpression>,
    },
}
