use std::collections::HashMap;

use cx_hmir::{HMIRFunction, body::HMIRLocalID};
use cx_mir::{
    MIRBasicBlock, MIRBasicBlockID, MIRBody, MIRFnPrototype, MIRFunction, MIRPlaceDecl, MIRPlaceID,
    MIRRegisterDecl, MIRScopeDecl, MIRValue,
};

use crate::env::HMIREnvironment;

#[derive(Debug)]
pub struct FnLoweringContext<'global, 'hmir> {
    env: &'hmir mut HMIREnvironment<'global, 'hmir>,
    builder: MIRFunctionBuilder,
    
    locals: HashMap<HMIRLocalID, MIRValue>,
}

impl<'global, 'hmir> FnLoweringContext<'global, 'hmir> {
    pub fn new(env: &'hmir mut HMIREnvironment<'global, 'hmir>, prototype: MIRFnPrototype) -> Self {
        Self {
            env,
            builder: MIRFunctionBuilder::new(prototype),
            
            locals: HashMap::new(),
        }
    }

    pub fn insert_local(&mut self, id: HMIRLocalID, value: MIRValue) {
        self.locals.insert(id, value);
    }

    pub fn get_local(&self, id: &HMIRLocalID) -> Option<&MIRValue> {
        self.locals.get(id)
    }
}

#[derive(Debug)]
pub struct MIRFunctionBuilder {
    prototype: MIRFnPrototype,

    parameters: Vec<MIRPlaceID>,
    blocks: Vec<MIRBasicBlock>,
    registers: Vec<MIRRegisterDecl>,
    places: Vec<MIRPlaceDecl>,
    scopes: Vec<MIRScopeDecl>,
    address_labels: Vec<(String, MIRBasicBlockID)>,
}

impl MIRFunctionBuilder {
    pub fn new(prototype: MIRFnPrototype) -> Self {
        Self {
            prototype,
            
            parameters: Vec::new(),
            blocks: Vec::new(),
            registers: Vec::new(),
            places: Vec::new(),
            scopes: Vec::new(),
            address_labels: Vec::new(),
        }
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

    pub fn add_block(&mut self, block: MIRBasicBlock) -> MIRBasicBlockID {
        let id = MIRBasicBlockID::new(self.blocks.len());
        self.blocks.push(block);
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
