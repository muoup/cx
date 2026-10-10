use cx_mir::{
    MIRBasicBlock, MIRBasicBlockID, MIRBody, MIRFnPrototype, MIRFunction, MIRInstruction,
    MIRInstructionKind, MIRPlaceDecl, MIRPlaceID, MIRRegister, MIRRegisterDecl, MIRScopeDecl,
    MIRScopeID,
};
use cx_tokens::TokenRange;
use cx_util::identifier::CXIdent;

#[derive(Debug)]
pub struct MIRFunctionBuilder {
    prototype: MIRFnPrototype,

    parameters: Vec<MIRPlaceID>,
    blocks: Vec<MIRBasicBlock>,
    registers: Vec<MIRRegisterDecl>,
    places: Vec<MIRPlaceDecl>,
    scopes: Vec<MIRScopeDecl>,
    address_labels: Vec<(String, MIRBasicBlockID)>,

    current_block: MIRBasicBlockID,
}

impl MIRFunctionBuilder {
    pub fn new(prototype: MIRFnPrototype) -> Self {
        let entry = MIRBasicBlockID::new(0);

        Self {
            prototype,

            parameters: Vec::new(),
            blocks: vec![MIRBasicBlock::new(entry, None)],
            registers: Vec::new(),
            places: Vec::new(),
            scopes: Vec::new(),
            address_labels: Vec::new(),

            current_block: entry,
        }
    }

    pub fn prototype(&self) -> &MIRFnPrototype {
        &self.prototype
    }

    pub fn parameters(&self) -> &[MIRPlaceID] {
        &self.parameters
    }

    pub fn parameter(&self, index: usize) -> Option<MIRPlaceID> {
        self.parameters.get(index).copied()
    }

    pub fn add_parameter(&mut self, param: MIRPlaceID) {
        self.parameters.push(param);
    }

    pub fn blocks(&self) -> &[MIRBasicBlock] {
        &self.blocks
    }

    pub fn block(&self, id: MIRBasicBlockID) -> Option<&MIRBasicBlock> {
        self.blocks.get(id.index())
    }

    pub fn block_mut(&mut self, id: MIRBasicBlockID) -> Option<&mut MIRBasicBlock> {
        self.blocks.get_mut(id.index())
    }

    pub fn add_block(&mut self, debug_name: Option<CXIdent>) -> MIRBasicBlockID {
        let id = MIRBasicBlockID::new(self.blocks.len());
        self.blocks.push(MIRBasicBlock::new(id, debug_name));
        id
    }

    pub fn add_block_param(&mut self, block: MIRBasicBlockID, param: MIRRegister) {
        self.blocks[block.index()].push_param(param);
    }

    pub fn current_block(&self) -> MIRBasicBlockID {
        self.current_block
    }

    pub fn switch_to(&mut self, block: MIRBasicBlockID) {
        assert!(
            block.index() < self.blocks.len(),
            "Attempt to switch to unknown block {block}"
        );

        self.current_block = block;
    }

    pub fn emit(&mut self, kind: MIRInstructionKind, span: TokenRange) {
        let block = &mut self.blocks[self.current_block.index()];
        if block
            .last_instruction()
            .is_some_and(MIRInstruction::is_terminator)
        {
            return;
        }

        block.push_instruction(MIRInstruction::new(kind, span));
    }

    pub fn add_scope(&mut self, span: TokenRange) -> MIRScopeID {
        let id = MIRScopeID::new(self.scopes.len());
        self.scopes.push(MIRScopeDecl::new(span));
        id
    }

    pub fn places(&self) -> &[MIRPlaceDecl] {
        &self.places
    }

    pub fn place(&self, id: MIRPlaceID) -> Option<&MIRPlaceDecl> {
        self.places.get(id.index())
    }

    pub fn allocate_place(&mut self, place: MIRPlaceDecl) -> MIRPlaceID {
        let id = MIRPlaceID::new(self.places.len());
        self.places.push(place);
        id
    }

    pub fn scopes(&self) -> &[MIRScopeDecl] {
        &self.scopes
    }

    pub fn register(&self, id: MIRRegister) -> Option<&MIRRegisterDecl> {
        self.registers.get(id.index())
    }

    pub fn allocate_register(&mut self, register: MIRRegisterDecl) -> MIRRegister {
        let id = MIRRegister::new(self.registers.len());
        self.registers.push(register);
        id
    }

    pub fn finish(self) -> MIRFunction {
        MIRFunction::new(
            self.prototype,
            Some(MIRBody::new(
                MIRBasicBlockID::new(0),
                self.blocks,
                self.places,
                self.parameters,
                self.registers,
                self.scopes,
                self.address_labels,
            )),
        )
    }
}
