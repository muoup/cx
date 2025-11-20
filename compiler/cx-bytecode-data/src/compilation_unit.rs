use cx_util::identifier::CXIdent;

use crate::{bc_type::BCFunctionPrototype, instruction::BCInstruction};

#[derive(Debug, Clone, PartialEq, Eq)]
pub struct BCUnit {
    pub function_prototypes: Vec<BCFunctionPrototype>,
    pub functions: Vec<BCFunction>,
}

#[derive(Debug, Clone, PartialEq, Eq)]
pub struct BCFunction {
    pub prototype: BCFunctionPrototype,
    pub params: Vec<CXIdent>,
    pub blocks: Vec<BCBlock>,
}

#[derive(Debug, Clone, PartialEq, Eq)]
pub struct BCBlock {
    pub name: CXIdent,
    pub instructions: Vec<BCInstruction>,
}
