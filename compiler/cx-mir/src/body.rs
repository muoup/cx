use crate::global::{MIRPlaceDecl, MIRRegisterDecl, MIRScopeDecl};
use crate::{
    MIRBasicBlock, MIRBasicBlockID, MIRInstrKind, MIRInstruction, MIRInstructionLike, MIRPlaceID,
    MIRRegister, MIRScopeID, MIRTypeID,
};
use cx_tokens::TokenRange;
use cx_util::identifier::CXIdent;

#[derive(Debug, Clone)]
pub struct MIRBody<K = MIRInstrKind> {
    entry: MIRBasicBlockID,

    blocks: Vec<MIRBasicBlock<K>>,
    places: Vec<MIRPlaceDecl>,
    parameters: Vec<MIRPlaceID>,
    registers: Vec<MIRRegisterDecl>,
    scopes: Vec<MIRScopeDecl>,
}

impl<K: MIRInstructionLike> MIRBody<K> {
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
        self.blocks.push(MIRBasicBlock::new(id));
        id
    }

    pub fn add_block_named(&mut self, debug_name: impl Into<CXIdent>) -> MIRBasicBlockID {
        let id = MIRBasicBlockID::new(self.blocks.len());
        let mut block = MIRBasicBlock::new(id);
        block.debug_name = Some(debug_name.into());
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
            .params
            .push(register);
        register
    }

    pub fn push_instr_at(&mut self, block: MIRBasicBlockID, kind: K, token_range: TokenRange) {
        let instr = crate::MIRInstruction::new(kind, token_range);
        self.block_mut(block)
            .expect("instruction pushed to unknown block")
            .instrs
            .push(instr);
    }

    pub fn blocks(&self) -> &[MIRBasicBlock<K>] {
        &self.blocks
    }

    pub fn block(&self, id: MIRBasicBlockID) -> Option<&MIRBasicBlock<K>> {
        self.blocks().get(id.index())
    }

    pub fn block_mut(&mut self, id: MIRBasicBlockID) -> Option<&mut MIRBasicBlock<K>> {
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
        });
        id
    }

    pub fn parameters(&self) -> &[MIRPlaceID] {
        &self.parameters
    }

    pub fn add_parameter(
        &mut self,
        parameter: &crate::MIRFnParam,
        scope: MIRScopeID,
    ) -> MIRPlaceID {
        let place = self.add_place(
            parameter.ty,
            parameter.name.clone(),
            parameter.nodrop,
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
    pub fn try_map<L: MIRInstructionLike, E>(
        self,
        mut map: impl FnMut(MIRInstruction<K>) -> Result<MIRInstruction<L>, E>,
    ) -> Result<MIRBody<L>, E> {
        let blocks = self
            .blocks
            .into_iter()
            .map(|block| {
                Ok(MIRBasicBlock {
                    id: block.id,
                    debug_name: block.debug_name,
                    params: block.params,
                    instrs: block
                        .instrs
                        .into_iter()
                        .map(&mut map)
                        .collect::<Result<_, E>>()?,
                })
            })
            .collect::<Result<_, E>>()?;
        Ok(MIRBody {
            entry: self.entry,
            blocks,
            places: self.places,
            parameters: self.parameters,
            registers: self.registers,
            scopes: self.scopes,
        })
    }
}
