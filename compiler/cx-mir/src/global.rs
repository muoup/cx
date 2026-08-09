use cx_ast::ast::modifiers::CXLinkageMode;
use cx_util::identifier::CXIdent;

use crate::expr::{MIRInstr};

pub struct MIRFnSignature {
    name: CXIdent,
}

pub struct MIRFnPrototype {
    signature: MIRFnSignature,
    linkage: CXLinkageMode,
}

pub struct MIRFunction {
    prototype: MIRFnPrototype,
    instrs: Vec<MIRInstr>,
}
