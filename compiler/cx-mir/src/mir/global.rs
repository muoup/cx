use cx_ast::ast::modifiers::CXLinkageMode;
use cx_util::identifier::CXIdent;

use crate::mir::data::MIRType;

#[derive(Debug, Clone)]
pub struct MIRGlobalVariable {
    pub kind: MIRGlobalVarKind,
    pub is_mutable: bool,
    pub linkage: CXLinkageMode,
}

#[derive(Debug, Clone)]
pub enum MIRGlobalVarKind {
    StringLiteral {
        name: CXIdent,
        value: String,
    },
    Variable {
        name: CXIdent,
        _type: MIRType,
        initializer: Option<i64>,
    },
}