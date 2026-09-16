use std::collections::HashMap;

use cx_lmir::{
    LMIRBasicBlock, LMIRFunction, LMIRFunctionMap, LMIRFunctionPrototype, LMIRGlobalValue,
    LMIRInstruction, LMIRValue,
};
use cx_mir::{ty::registry::MIRTypeRegistry, MIRBasicBlockID, MIRGlobalID, MIRTypeID, MIRUnit};

#[derive(Clone)]
pub(crate) enum PlaceBinding {
    Reference {
        value: LMIRValue,
        ty: MIRTypeID,
    },
    Address {
        value: LMIRValue,
        ty: MIRTypeID,
    },
    Bitfield {
        address: LMIRValue,
        storage_type: MIRTypeID,
        value_type: MIRTypeID,
        bit_offset: usize,
        bit_width: usize,
    },
}

pub(crate) struct LMIRGlobalContext<'mir> {
    unit: &'mir MIRUnit,

    prototypes: LMIRFunctionMap,

    globals: Vec<LMIRGlobalValue>,
    global_index: HashMap<MIRGlobalID, usize>,
}

impl<'mir> LMIRGlobalContext<'mir> {
    pub fn new(unit: &'mir MIRUnit) -> Self {
        Self {
            unit,
            prototypes: LMIRFunctionMap::new(),

            globals: Vec::new(),
            global_index: HashMap::new(),
        }
    }

    pub fn types(&self) -> &MIRTypeRegistry {
        self.unit.types()
    }

    pub fn add_global(&mut self, global: LMIRGlobalValue, mir: Option<MIRGlobalID>) -> usize {
        let index = self.globals.len();

        self.globals.push(global);
        if let Some(mir) = mir {
            self.global_index.insert(mir, index);
        }

        index
    }

    pub fn prototypes(&self) -> &LMIRFunctionMap {
        &self.prototypes
    }

    pub fn prototypes_mut(&mut self) -> &mut LMIRFunctionMap {
        &mut self.prototypes
    }
}

pub(crate) struct LMIRFunctionContext<'global> {
    global: &'global mut LMIRGlobalContext<'global>,

    prototype: LMIRFunctionPrototype,

    blocks: Vec<LMIRBasicBlock>,
    block_indices: HashMap<MIRBasicBlockID, usize>,

    current_block: usize,
}

impl<'global> LMIRFunctionContext<'global> {
    pub(crate) fn new(
        global: &'global mut LMIRGlobalContext<'global>,
        prototype: LMIRFunctionPrototype,
    ) -> Self {
        Self {
            global,
            prototype,

            blocks: Vec::new(),
            block_indices: HashMap::new(),

            current_block: 0,
        }
    }

    pub(crate) fn finish(self) -> LMIRFunction {
        LMIRFunction {
            prototype: self.prototype,
            blocks: self.blocks,
        }
    }

    pub(crate) fn global(&self) -> &LMIRGlobalContext {
        self.global
    }

    pub(crate) fn prototype(&self) -> &LMIRFunctionPrototype {
        &self.prototype
    }

    pub(crate) fn set_current(&mut self, current: usize) {
        self.current_block = current;
    }

    pub(crate) fn current_block_mut(&mut self) -> &mut LMIRBasicBlock {
        &mut self.blocks[self.current_block]
    }

    pub(crate) fn emit(&mut self, instruction: LMIRInstruction) {
        self.current_block_mut().body.push(instruction);
    }

    pub(crate) fn block_index(&self, block: MIRBasicBlockID) -> usize {
        *self
            .block_indices
            .get(&block)
            .expect("MIR block has no LMIR block index")
    }

    pub(crate) fn push_block(
        &mut self,
        block: LMIRBasicBlock,
        binding: Option<MIRBasicBlockID>,
    ) -> usize {
        self.blocks.push(block);
        let index = self.blocks.len() - 1;

        if let Some(binding) = binding {
            self.block_indices.insert(binding, index);
        }

        index
    }
}
