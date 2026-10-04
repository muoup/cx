use cx_tokens::TokenRange;
use cx_util::identifier::CXIdent;

use crate::{
    expr::instruction::{MIRBasicBlock, MIRInstruction, MIRScopeID},
    ty::MIRTypeID,
    unit::{MIRBasicBlockID, MIRPlaceDecl, MIRRegisterDecl, MIRScopeDecl, function::MIRFnParam},
    value::{MIRBindable, MIRPlaceID, MIRRegisterID as MIRRegister},
};

#[derive(Debug, Clone)]
pub struct MIRBody {
    entry: MIRBasicBlockID,

    blocks: Vec<MIRBasicBlock>,
    places: Vec<MIRPlaceDecl>,

    parameters: Vec<MIRPlaceID>,

    registers: Vec<MIRRegisterDecl>,

    scopes: Vec<MIRScopeDecl>,
}

impl MIRBody {
    pub fn new() -> Self {
        Self {
            entry: MIRBasicBlockID::new(0),
            blocks: Vec::new(),
            places: Vec::new(),
            parameters: Vec::new(),
            registers: Vec::new(),
            scopes: Vec::new(),
        }
    }

    pub fn entry(&self) -> MIRBasicBlockID {
        self.entry
    }

    pub fn add_block(&mut self) -> MIRBasicBlockID {
        let id = MIRBasicBlockID::new(self.blocks.len());

        self.blocks.push(MIRBasicBlock::new(id, None));
        id
    }

    pub fn add_block_named(&mut self, debug_name: impl Into<CXIdent>) -> MIRBasicBlockID {
        let id = MIRBasicBlockID::new(self.blocks.len());
        let block = MIRBasicBlock::new(id, Some(debug_name.into()));

        self.blocks.push(block);
        id
    }

    pub fn add_block_param(
        &mut self,
        block: MIRBasicBlockID,
        ty: MIRTypeID,
        debug_name: Option<CXIdent>,
    ) -> MIRRegister {
        let register = self.add_register(ty, debug_name);
        self.block_mut(block)
            .expect("block param added to unknown block")
            .push_param(register);
        register
    }

    pub fn push_instr_at(&mut self, block: MIRBasicBlockID, instr: MIRInstruction) {
        self.block_mut(block)
            .expect("instruction pushed to unknown block")
            .push_instruction(instr);
    }

    pub fn blocks(&self) -> &[MIRBasicBlock] {
        &self.blocks
    }

    pub fn block(&self, id: MIRBasicBlockID) -> Option<&MIRBasicBlock> {
        self.blocks().get(id.index())
    }

    pub fn block_mut(&mut self, id: MIRBasicBlockID) -> Option<&mut MIRBasicBlock> {
        self.blocks.get_mut(id.index())
    }

    pub fn places(&self) -> &[MIRPlaceDecl] {
        &self.places
    }

    pub fn place(&self, id: MIRPlaceID) -> Option<&MIRPlaceDecl> {
        self.places().get(id.index())
    }

    pub fn place_mut(&mut self, id: MIRPlaceID) -> Option<&mut MIRPlaceDecl> {
        self.places.get_mut(id.index())
    }

    pub fn mark_adopted(&mut self, place: MIRPlaceID) {
        self.place_mut(place)
            .expect("adopted unknown MIR place")
            .adopted = true;
    }

    pub fn scopes(&self) -> &[MIRScopeDecl] {
        &self.scopes
    }

    pub fn scope(&self, id: MIRScopeID) -> Option<&MIRScopeDecl> {
        self.scopes().get(id.index())
    }

    pub fn scope_mut(&mut self, id: MIRScopeID) -> Option<&mut MIRScopeDecl> {
        self.scopes.get_mut(id.index())
    }

    pub fn add_scope(&mut self, token_range: TokenRange) -> MIRScopeID {
        let id = MIRScopeID::new(self.scopes.len());
        self.scopes.push(MIRScopeDecl { id, token_range });
        id
    }

    pub fn add_register(&mut self, ty: MIRTypeID, debug_name: Option<CXIdent>) -> MIRRegister {
        let id = MIRRegister::new(self.registers.len());
        self.registers.push(MIRRegisterDecl { id, ty, debug_name });
        id
    }

    pub fn add_place(
        &mut self,
        ty: MIRTypeID,
        debug_name: Option<CXIdent>,
        nodrop: bool,
        scope: MIRScopeID,
    ) -> MIRPlaceID {
        let id = MIRPlaceID::new(self.places.len());
        self.places.push(MIRPlaceDecl {
            id,
            ty,
            debug_name,
            nodrop,
            scope,
            adopted: false,
        });
        id
    }

    pub fn parameters(&self) -> &[MIRPlaceID] {
        &self.parameters
    }

    pub fn add_parameter(&mut self, parameter: &MIRFnParam, scope: MIRScopeID) -> MIRPlaceID {
        let place = self.add_place(
            parameter.ty(),
            parameter.name().cloned(),
            parameter.nodrop(),
            scope,
        );
        self.parameters.push(place);
        place
    }

    pub fn registers(&self) -> &[MIRRegisterDecl] {
        &self.registers
    }

    pub fn register(&self, id: MIRRegister) -> Option<&MIRRegisterDecl> {
        self.registers().get(id.index())
    }

    pub fn bindable_debug_name(&self, bindable: &MIRBindable) -> (String, bool) {
        let debug_name = match bindable {
            MIRBindable::Place(id) => self.place(*id).and_then(|place| place.debug_name.as_ref()),
            MIRBindable::Register(id) => self
                .register(*id)
                .and_then(|register| register.debug_name.as_ref()),
        };
        let name = debug_name
            .map(ToString::to_string)
            .unwrap_or_else(|| format!("{bindable:?}"));
        let discarded = debug_name.is_some_and(|name| name.as_str() == "_");
        (name, discarded)
    }
}
