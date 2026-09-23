use std::collections::HashMap;

use cx_lmir::types::{LMIRIntegerType, LMIRType, LMIRTypeKind};
use cx_lmir::{
    LMIRBasicBlock, LMIRBlockParameter, LMIRFunction, LMIRFunctionMap, LMIRFunctionPrototype, LMIRGlobalValue, LMIRInstruction, LMIRInstructionKind, LMIRRegister, LMIRUnit, LMIRValue,
};
use cx_mir::MIRFunction;
use cx_mir::ty::interface::MTRegistry;
use cx_mir::{
    ty::{layout::calculate_type_layout, registry::MIRTypeRegistry},
    MIRBasicBlockID, MIRBody, MIRGlobalID, MIRPlaceID, MIRRegister, MIRTypeID, MIRUnit,
};
use cx_util::identifier::CXIdent;

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
    unit: &'mir MIRUnit<'mir>,

    prototypes: LMIRFunctionMap,
    functions: Vec<LMIRFunction>,

    globals: Vec<LMIRGlobalValue>,
    global_index: HashMap<MIRGlobalID, usize>,
}

impl<'mir> LMIRGlobalContext<'mir> {
    pub fn new(unit: &'mir MIRUnit) -> Self {
        Self {
            unit,

            prototypes: LMIRFunctionMap::new(),
            functions: Vec::new(),
            
            globals: Vec::new(),
            global_index: HashMap::new(),
        }
    }

    pub fn unit(&self) -> &MIRUnit {
        self.unit
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

    pub fn functions(&self) -> &[LMIRFunction] {
        &self.functions
    }

    pub fn add_function(&mut self, function: LMIRFunction) {
        self.functions.push(function);
    }

    pub fn finish(self) -> LMIRUnit {
        LMIRUnit {
            architecture: *self.types().architecture(),
            fn_map: self.prototypes,
            fn_defs: self.functions,
            global_vars: self.globals,
        }
    }
}

pub(crate) struct LMIRFunctionContext<'global> {
    global: &'global mut LMIRGlobalContext<'global>,
    mir_function: &'global MIRFunction,

    prototype: LMIRFunctionPrototype,

    blocks: Vec<LMIRBasicBlock>,
    block_indices: HashMap<MIRBasicBlockID, usize>,

    current_block: usize,
    current_register: usize,
    place_addresses: HashMap<MIRPlaceID, LMIRValue>,
    lifted: HashMap<MIRRegister, LMIRValue>,
}

impl<'global> LMIRFunctionContext<'global> {
    pub(crate) fn new(
        global: &'global mut LMIRGlobalContext<'global>,
        mir_function: &'global MIRFunction,
        prototype: LMIRFunctionPrototype,
    ) -> Self {
        Self {
            global,
            mir_function,
            prototype,

            blocks: Vec::new(),
            block_indices: HashMap::new(),

            current_block: 0,
            current_register: 0,
            place_addresses: HashMap::new(),
            lifted: HashMap::new(),
        }
    }

    pub(crate) fn finish(self) -> LMIRFunction {
        LMIRFunction {
            prototype: self.prototype,
            blocks: self.blocks,
        }
    }

    pub(crate) fn mir_function(&self) -> &MIRFunction {
        self.mir_function
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

    pub(crate) fn bind_place(&mut self, place: MIRPlaceID, address: LMIRValue) {
        self.place_addresses.insert(place, address);
    }

    pub(crate) fn record_lift(&mut self, out: MIRRegister, place: MIRPlaceID) {
        let address = self
            .place_addresses
            .get(&place)
            .expect("lifted place has no LMIR storage")
            .clone();
        self.lifted.insert(out, address);
    }

    pub(crate) fn forward_lift(&mut self, out: MIRRegister, source: MIRRegister) {
        if let Some(address) = self.lifted.get(&source).cloned() {
            self.lifted.insert(out, address);
        }
    }

    pub(crate) fn lifted_address(&self, register: MIRRegister) -> Option<&LMIRValue> {
        self.lifted.get(&register)
    }

    pub(crate) fn preserve_lift(&mut self, body: &MIRBody, register: MIRRegister) {
        let place = body
            .blocks()
            .iter()
            .flat_map(|block| block.instructions())
            .find_map(|instruction| match instruction.kind {
                cx_mir::MIRInstructionKind::LiftPlace { out, place } if out == register => {
                    Some(place)
                }
                _ => None,
            })
            .expect("preserved register has no lifted source");
        let declaration = body.place(place).expect("lifted place has no declaration");
        let layout = calculate_type_layout(self.global.types(), declaration.ty);
        let ty = super::lowering::typing::convert_type(declaration.ty, self.global.types());
        let pointer = LMIRType::default_pointer(self.global.types().architecture());
        let address = LMIRValue::Register {
            register: self.new_register(),
            _type: pointer.clone(),
        };
        let LMIRValue::Register {
            register: result, ..
        } = &address
        else {
            unreachable!()
        };
        self.emit(LMIRInstruction {
            kind: LMIRInstructionKind::Allocate {
                _type: ty,
                alignment: layout.alignment() as u8,
            },
            value_type: pointer,
            result: Some(result.clone()),
        });
        let source = self
            .lifted
            .get(&register)
            .expect("lift was not lowered")
            .clone();
        let size = LMIRValue::IntImmediate {
            _type: LMIRType::new(LMIRTypeKind::Integer(LMIRIntegerType::I64), 8),
            val: i64::try_from(layout.size()).expect("preserved value size exceeds i64"),
        };
        self.emit(LMIRInstruction {
            kind: LMIRInstructionKind::Memcpy {
                dest: address.clone(),
                src: source,
                size,
                alignment: layout.alignment() as u8,
            },
            value_type: LMIRType::unit(),
            result: None,
        });
        self.lifted.insert(register, address);
    }

    pub(crate) fn block_index(&self, block: MIRBasicBlockID) -> usize {
        *self
            .block_indices
            .get(&block)
            .expect("MIR block has no LMIR block index")
    }

    pub(crate) fn lower_register(&mut self, register: &MIRRegister) -> LMIRRegister {
        LMIRRegister::new(format!("mir.{}", register.0))
    }

    pub(crate) fn new_register(&mut self) -> LMIRRegister {
        let register = LMIRRegister::new(format!("lmir.{}", self.current_register));
        self.current_register += 1;
        register
    }

    pub(crate) fn push_block(
        &mut self,
        args: Vec<LMIRBlockParameter>,
        debug_name: Option<CXIdent>,
        binding: Option<MIRBasicBlockID>,
    ) -> usize {
        let id = CXIdent::from(format!("block.{}", self.blocks.len() - 1));

        self.blocks.push(LMIRBasicBlock {
            id,
            params: args,
            body: Vec::new(),
            debug_name,
        });

        let index = self.blocks.len() - 1;

        if let Some(binding) = binding {
            self.block_indices.insert(binding, index);
        }

        index
    }
}
