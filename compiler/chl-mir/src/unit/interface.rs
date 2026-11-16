use chl_util::identifier::CHLIdent;

use crate::{MIRFunctionID, MIRGlobalID, MIRTypeID};

#[derive(Debug, Clone, Default)]
pub struct MIRCInterface {
    pub functions: Vec<MIRFunctionID>,
    pub globals: Vec<MIRGlobalID>,
    pub types: Vec<MIRCType>,
    pub constants: Vec<(CHLIdent, i128)>,
}

#[derive(Debug, Clone)]
pub struct MIRCType {
    pub name: CHLIdent,
    pub ty: MIRTypeID,
    pub alias: bool,
}
