use cx_util::identifier::CXIdent;

use crate::{MIRFunctionID, MIRGlobalID, MIRTypeID};

#[derive(Debug, Clone, Default)]
pub struct MIRCInterface {
    pub functions: Vec<MIRFunctionID>,
    pub globals: Vec<MIRGlobalID>,
    pub types: Vec<MIRCType>,
    pub constants: Vec<(CXIdent, i128)>,
}

#[derive(Debug, Clone)]
pub struct MIRCType {
    pub name: CXIdent,
    pub ty: MIRTypeID,
    pub alias: bool,
}
