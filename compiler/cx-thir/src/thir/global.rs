use cx_util::{identifier::CXIdent, linkage::LinkageMode};

use crate::thir::r#type::THIRType;

#[derive(Debug, Clone)]
pub struct MIRGlobalVariable {
    pub kind: MIRGlobalVarKind,
    pub is_mutable: bool,
    pub linkage: LinkageMode,
}

#[derive(Debug, Clone)]
pub enum MIRGlobalVarKind {
    StringLiteral {
        name: CXIdent,
        value: String,
    },
    Variable {
        name: CXIdent,
        _type: THIRType,
        initializer: Option<i64>,
    },
}
