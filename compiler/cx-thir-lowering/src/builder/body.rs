#[derive(Debug)]
pub(crate) struct MIRBodyBuilder {
    current_block: MIRBasicBlockID,

    entry: Option<MIRBasicBlockID>,
    blocks: Vec<MIRBasicBlock>,
    places: Vec<MIRPlaceDecl>,
    registers: Vec<MIRRegisterDecl>,
    scopes: Vec<MIRScopeDecl>,
}

impl MIRBodyBuilder {
    pub fn finish(self) -> MIRBody {
        MIRBody {
            entry: self.entry,
            blocks: self.blocks,
            places: self.places,
            registers: self.registers,
            scopes: self.scopes,
        }
    }
}
