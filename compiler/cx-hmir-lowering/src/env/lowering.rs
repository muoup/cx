use std::collections::HashMap;

use cx_hmir::{HMIRFunction, body::HMIRLocalID};
use cx_log::CXResult;
use cx_mir::{
    MIRBasicBlock, MIRBasicBlockID, MIRBody, MIRFunctionID, MIRInstructionKind, MIRPlaceDecl,
    MIRPlaceID, MIRRegister, MIRRegisterDecl, MIRScopeDecl, MIRScopeID, MIRValue,
};
use cx_tokens::TokenRange;
use cx_util::identifier::CXIdent;

use crate::{env::HMIREnvironment, lower::HMIRValue};

#[derive(Debug)]
pub struct FnLoweringContext<'env, 'global, 'hmir> {
    env: &'env mut HMIREnvironment<'global, 'hmir>,
    builder: MIRFunctionBuilder,

    locals: HashMap<HMIRLocalID, HMIRValue>,
}

impl<'env, 'global, 'hmir> FnLoweringContext<'env, 'global, 'hmir> {
    pub fn new(env: &'env mut HMIREnvironment<'global, 'hmir>, function: MIRFunctionID) -> Self {
        Self {
            env,
            builder: MIRFunctionBuilder::new(function),

            locals: HashMap::new(),
        }
    }

    pub fn env(&mut self) -> &mut HMIREnvironment<'global, 'hmir> {
        self.env
    }

    pub fn builder(&mut self) -> &mut MIRFunctionBuilder {
        &mut self.builder
    }

    pub fn insert_local(&mut self, id: HMIRLocalID, value: HMIRValue) {
        self.locals.insert(id, value);
    }

    pub fn get_local(&self, id: &HMIRLocalID) -> Option<&HMIRValue> {
        self.locals.get(id)
    }

    pub fn materialize(&mut self, value: &HMIRValue, span: &TokenRange) -> CXResult<MIRValue> {
        todo!()
    }

    pub fn finish(self) -> MIRBody {
        self.builder.finish()
    }
}

#[derive(Debug)]
pub struct MIRFunctionBuilder {
    function: MIRFunctionID,

    parameters: Vec<MIRPlaceID>,
    blocks: Vec<MIRBasicBlock>,
    registers: Vec<MIRRegisterDecl>,
    places: Vec<MIRPlaceDecl>,
    scopes: Vec<MIRScopeDecl>,
    address_labels: Vec<(String, MIRBasicBlockID)>,
}

impl MIRFunctionBuilder {
    pub fn new(function: MIRFunctionID) -> Self {
        Self {
            function,

            parameters: Vec::new(),
            blocks: Vec::new(),
            registers: Vec::new(),
            places: Vec::new(),
            scopes: Vec::new(),
            address_labels: Vec::new(),
        }
    }
    
    pub fn function(&self) -> MIRFunctionID {
        self.function
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

    pub fn current_block(&self) -> MIRBasicBlockID {
        todo!()
    }

    pub fn switch_to(&mut self, block: MIRBasicBlockID) {
        todo!()
    }

    pub fn terminated(&self) -> bool {
        todo!()
    }

    pub fn emit(&mut self, kind: MIRInstructionKind, span: &TokenRange) {
        todo!()
    }

    pub fn push_scope(&mut self, span: &TokenRange) -> MIRScopeID {
        todo!()
    }

    pub fn pop_scope(&mut self) -> MIRScopeID {
        todo!()
    }

    pub fn current_scope(&self) -> MIRScopeID {
        todo!()
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

    pub fn finish(self) -> MIRBody {
        MIRBody::new(
            MIRBasicBlockID::new(0),
            self.blocks,
            self.places,
            self.parameters,
            self.registers,
            self.scopes,
            self.address_labels,
        )
    }
}
