use cx_tokens::TokenRange;
use cx_util::identifier::CXIdent;

use crate::{MIRBasicBlock, MIRBasicBlockID, MIRPlaceID, MIRRegister, MIRScopeID, MIRTypeID};

#[derive(Debug, Clone)]
pub struct MIRBody {
    entry: MIRBasicBlockID,

    blocks: Vec<MIRBasicBlock>,
    places: Vec<MIRPlaceDecl>,
    parameters: Vec<MIRPlaceID>,
    registers: Vec<MIRRegisterDecl>,
    scopes: Vec<MIRScopeDecl>,
    address_labels: Vec<(String, MIRBasicBlockID)>,
}

impl MIRBody {
    pub fn new(
        entry: MIRBasicBlockID,
        blocks: Vec<MIRBasicBlock>,
        places: Vec<MIRPlaceDecl>,
        parameters: Vec<MIRPlaceID>,
        registers: Vec<MIRRegisterDecl>,
        scopes: Vec<MIRScopeDecl>,
        address_labels: Vec<(String, MIRBasicBlockID)>,
    ) -> Self {
        Self {
            entry,
            blocks,
            places,
            parameters,
            registers,
            scopes,
            address_labels,
        }
    }

    pub fn entry(&self) -> MIRBasicBlockID {
        self.entry
    }

    pub fn address_labels(&self) -> &[(String, MIRBasicBlockID)] {
        &self.address_labels
    }

    pub fn address_label(&self, label: &str) -> Option<MIRBasicBlockID> {
        self.address_labels
            .iter()
            .find_map(|(name, block)| (name == label).then_some(*block))
    }

    pub fn blocks(&self) -> impl Iterator<Item = (MIRBasicBlockID, &MIRBasicBlock)> {
        self.blocks
            .iter()
            .enumerate()
            .map(|(index, block)| (MIRBasicBlockID::new(index), block))
    }

    pub fn block(&self, id: MIRBasicBlockID) -> &MIRBasicBlock {
        &self.blocks[id.index()]
    }

    pub fn places(&self) -> impl Iterator<Item = (MIRPlaceID, &MIRPlaceDecl)> {
        self.places
            .iter()
            .enumerate()
            .map(|(index, place)| (MIRPlaceID::new(index), place))
    }

    pub fn place(&self, id: MIRPlaceID) -> &MIRPlaceDecl {
        &self.places[id.index()]
    }

    pub fn scopes(&self) -> impl Iterator<Item = (MIRScopeID, &MIRScopeDecl)> {
        self.scopes
            .iter()
            .enumerate()
            .map(|(index, scope)| (MIRScopeID::new(index), scope))
    }

    pub fn scope(&self, id: MIRScopeID) -> &MIRScopeDecl {
        &self.scopes[id.index()]
    }

    pub fn parameters(&self) -> impl Iterator<Item = (MIRPlaceID, &MIRPlaceDecl)> {
        self.parameters
            .iter()
            .map(|&id| (id, self.place(id)))
    }

    pub fn parameter(&self, idx: usize) -> MIRPlaceID {
        self.parameters[idx]
    }

    pub fn registers(&self) -> impl Iterator<Item = (MIRRegister, &MIRRegisterDecl)> {
        self.registers
            .iter()
            .enumerate()
            .map(|(index, register)| (MIRRegister::new(index), register))
    }

    pub fn register(&self, id: MIRRegister) -> &MIRRegisterDecl {
        &self.registers[id.index()]
    }
}

#[derive(Debug, Clone)]
pub struct MIRPlaceDecl {
    ty: MIRTypeID,
    debug_name: Option<CXIdent>,
    scope: MIRScopeID,

    nodrop: bool,
    adopted: bool,
}

#[derive(Debug, Clone)]
pub struct MIRScopeDecl {
    token_range: TokenRange,
}

#[derive(Debug, Clone)]
pub struct MIRRegisterDecl {
    ty: MIRTypeID,
    debug_name: Option<CXIdent>,
}

impl MIRPlaceDecl {
    pub fn new(
        ty: MIRTypeID,
        debug_name: Option<CXIdent>,
        scope: MIRScopeID,
        nodrop: bool,
        adopted: bool,
    ) -> Self {
        Self {
            ty,
            debug_name,
            scope,
            nodrop,
            adopted,
        }
    }

    pub fn ty(&self) -> MIRTypeID {
        self.ty
    }

    pub fn debug_name(&self) -> Option<&CXIdent> {
        self.debug_name.as_ref()
    }

    pub fn scope(&self) -> MIRScopeID {
        self.scope
    }

    pub fn nodrop(&self) -> bool {
        self.nodrop
    }

    pub fn adopted(&self) -> bool {
        self.adopted
    }
}

impl MIRScopeDecl {
    pub fn new(token_range: TokenRange) -> Self {
        Self { token_range }
    }

    pub fn token_range(&self) -> &TokenRange {
        &self.token_range
    }
}

impl MIRRegisterDecl {
    pub fn new(ty: MIRTypeID, debug_name: Option<CXIdent>) -> Self {
        Self { ty, debug_name }
    }

    pub fn ty(&self) -> MIRTypeID {
        self.ty
    }

    pub fn debug_name(&self) -> Option<&CXIdent> {
        self.debug_name.as_ref()
    }
}
