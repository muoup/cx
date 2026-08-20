use std::collections::HashMap;

use cx_lmir::{
    LMIRBasicBlock, LMIRFunction, LMIRFunctionMap, LMIRFunctionPrototype, LMIRGlobalValue,
    LMIRInstruction,
};
use cx_mir::{
    MIRBasicBlockID, MIRFunction, MIRGlobalID, MIRPlace, MIRRegister, MIRTypeID, MIRUnit,
};

#[derive(Clone)]
pub(crate) enum PlaceBinding {
    Address {
        value: cx_lmir::LMIRValue,
        ty: MIRTypeID,
    },
    Bitfield {
        address: cx_lmir::LMIRValue,
        storage_type: MIRTypeID,
        value_type: MIRTypeID,
        bit_offset: usize,
        bit_width: usize,
    },
}

pub(crate) struct FunctionLoweringContext<'a> {
    unit: &'a MIRUnit,
    function: &'a MIRFunction,
    types: &'a cx_mir::MIRTypeRegistryBuilder,
    prototypes: &'a LMIRFunctionMap,
    global_indices: &'a HashMap<MIRGlobalID, u32>,
    globals: &'a mut Vec<LMIRGlobalValue>,
    prototype: LMIRFunctionPrototype,
    blocks: Vec<LMIRBasicBlock>,
    block_indices: HashMap<MIRBasicBlockID, usize>,
    places: HashMap<MIRPlace, PlaceBinding>,
    current: usize,
    temp: usize,
}

impl<'a> FunctionLoweringContext<'a> {
    pub(crate) fn new(
        unit: &'a MIRUnit,
        function: &'a MIRFunction,
        types: &'a cx_mir::MIRTypeRegistryBuilder,
        prototypes: &'a LMIRFunctionMap,
        global_indices: &'a HashMap<MIRGlobalID, u32>,
        globals: &'a mut Vec<LMIRGlobalValue>,
        prototype: LMIRFunctionPrototype,
        blocks: Vec<LMIRBasicBlock>,
        block_indices: HashMap<MIRBasicBlockID, usize>,
    ) -> Self {
        Self {
            unit,
            function,
            types,
            prototypes,
            global_indices,
            globals,
            prototype,
            blocks,
            block_indices,
            places: HashMap::new(),
            current: 0,
            temp: 0,
        }
    }

    pub(crate) fn finish(self) -> LMIRFunction {
        LMIRFunction {
            prototype: self.prototype,
            blocks: self.blocks,
        }
    }

    pub(crate) fn unit(&self) -> &'a MIRUnit {
        self.unit
    }

    pub(crate) fn function(&self) -> &'a MIRFunction {
        self.function
    }

    pub(crate) fn types(&self) -> &'a cx_mir::MIRTypeRegistryBuilder {
        self.types
    }

    pub(crate) fn prototypes(&self) -> &'a LMIRFunctionMap {
        self.prototypes
    }

    pub(crate) fn prototype(&self) -> &LMIRFunctionPrototype {
        &self.prototype
    }

    pub(crate) fn global_indices(&self) -> &'a HashMap<MIRGlobalID, u32> {
        self.global_indices
    }

    pub(crate) fn globals(&self) -> &Vec<LMIRGlobalValue> {
        self.globals
    }

    pub(crate) fn globals_mut(&mut self) -> &mut Vec<LMIRGlobalValue> {
        self.globals
    }

    pub(crate) fn blocks_len(&self) -> usize {
        self.blocks.len()
    }

    pub(crate) fn set_current(&mut self, current: usize) {
        self.current = current;
    }

    pub(crate) fn current_block_body_mut(&mut self) -> &mut Vec<LMIRInstruction> {
        &mut self.blocks[self.current].body
    }

    pub(crate) fn block_index(&self, block: MIRBasicBlockID) -> usize {
        *self
            .block_indices
            .get(&block)
            .expect("MIR block has no LMIR block index")
    }

    pub(crate) fn bind_place(&mut self, place: MIRPlace, binding: PlaceBinding) {
        self.places.insert(place, binding);
    }

    pub(crate) fn next_temp(&mut self) -> usize {
        let temp = self.temp;
        self.temp += 1;
        temp
    }

    pub(crate) fn push_block(&mut self, block: LMIRBasicBlock) {
        self.blocks.push(block);
    }

    pub(crate) fn place_binding(&self, place: MIRPlace) -> Option<PlaceBinding> {
        self.places.get(&place).cloned()
    }

    pub(crate) fn register_type(&self, register: MIRRegister) -> MIRTypeID {
        self.function
            .register(register)
            .expect("invalid register")
            .ty
    }
}
