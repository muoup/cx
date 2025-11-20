use cx_typechecker_data::cx_types::CXFunctionPrototype;
use cx_util::identifier::CXIdent;

use crate::expression::MIRInstruction;

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