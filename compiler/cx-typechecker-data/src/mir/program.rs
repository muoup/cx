use cx_util::identifier::CXIdent;

use crate::mir::{expression::MIRInstruction, types::CXFunctionPrototype};


pub struct MIRUnit {
    pub prototypes: Vec<CXFunctionPrototype>,
    pub functions: Vec<MIRFunction>
}
    
pub struct MIRFunction {
    pub prototype: CXFunctionPrototype,
    pub basic_blocks: Vec<MIRBasicBlock>,
}

pub struct MIRBasicBlock {
    pub id: CXIdent,
    pub expressions: Vec<MIRInstruction>,
}