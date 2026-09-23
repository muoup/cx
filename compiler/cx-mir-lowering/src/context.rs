use std::collections::HashMap;

use cx_lmir::types::{LMIRIntegerType, LMIRType, LMIRTypeKind, TypeSize};
use cx_lmir::{
    LMIRBasicBlock, LMIRBlockTarget, LMIRFunction, LMIRFunctionMap, LMIRFunctionPrototype,
    LMIRGlobalType, LMIRGlobalValue, LMIRInstruction, LMIRInstructionKind, LMIRPtrBinOp,
    LMIRRegister, LMIRUnit, LMIRValue, LinkageType,
};
use cx_mir::ty::interface::MTRegistry;
use cx_mir::ty::layout::calculate_type_layout;
use cx_mir::{
    MIRBasicBlockID, MIRBody, MIRFunction, MIRGlobalID, MIRPlaceID, MIRRegister, MIRTypeID, MIRUnit,
};
use cx_util::identifier::CXIdent;

use crate::lowering::typing::convert_type;

pub(crate) struct GlobalContext<'mir> {
    pub unit: &'mir MIRUnit<'mir>,
    pub prototypes: LMIRFunctionMap,
    pub functions: Vec<LMIRFunction>,
    pub globals: Vec<LMIRGlobalValue>,
    pub global_indices: HashMap<MIRGlobalID, u32>,
    strings: HashMap<String, u32>,
}

impl<'mir> GlobalContext<'mir> {
    pub fn new(unit: &'mir MIRUnit<'mir>) -> Self {
        Self {
            unit,
            prototypes: LMIRFunctionMap::new(),
            functions: Vec::new(),
            globals: Vec::new(),
            global_indices: HashMap::new(),
            strings: HashMap::new(),
        }
    }

    pub fn string(&mut self, text: &str) -> u32 {
        if let Some(index) = self.strings.get(text) {
            return *index;
        }
        let index = self.globals.len() as u32;
        self.globals.push(LMIRGlobalValue {
            name: CXIdent::new(format!(".str.{index}")),
            _type: LMIRGlobalType::StringLiteral(text.to_owned()),
            linkage: LinkageType::Static,
        });
        self.strings.insert(text.to_owned(), index);
        index
    }

    pub fn finish(self) -> LMIRUnit {
        LMIRUnit {
            architecture: *self.unit.types().architecture(),
            fn_map: self.prototypes,
            fn_defs: self.functions,
            global_vars: self.globals,
        }
    }
}

pub(crate) struct FunctionContext<'a, 'mir> {
    pub global: &'a mut GlobalContext<'mir>,
    pub function: &'mir MIRFunction,
    pub body: &'mir MIRBody,
    pub prototype: LMIRFunctionPrototype,
    pub blocks: Vec<LMIRBasicBlock>,
    pub block_indices: HashMap<MIRBasicBlockID, usize>,
    pub current_block: usize,
    pub next_register: usize,
    pub places: HashMap<MIRPlaceID, LMIRValue>,
}

impl<'a, 'mir> FunctionContext<'a, 'mir> {
    pub fn new(
        global: &'a mut GlobalContext<'mir>,
        function: &'mir MIRFunction,
        body: &'mir MIRBody,
        prototype: LMIRFunctionPrototype,
    ) -> Self {
        Self {
            global,
            function,
            body,
            prototype,
            blocks: Vec::new(),
            block_indices: HashMap::new(),
            current_block: 0,
            next_register: 0,
            places: HashMap::new(),
        }
    }

    pub fn types(&self) -> &cx_mir::ty::registry::MIRTypeRegistry {
        self.global.unit.types()
    }

    pub fn ty(&self, ty: MIRTypeID) -> LMIRType {
        convert_type(ty, self.types())
    }

    pub fn pointer(&self) -> LMIRType {
        LMIRType::default_pointer(self.types().architecture())
    }

    pub fn reg(&self, id: MIRRegister) -> LMIRValue {
        LMIRValue::Register {
            register: LMIRRegister::new(format!("mir.{}", id.index())),
            _type: self.ty(self.body.register(id).expect("unknown MIR register").ty),
        }
    }

    pub fn emit(&mut self, kind: LMIRInstructionKind, ty: LMIRType, result: Option<LMIRRegister>) {
        self.blocks[self.current_block].body.push(LMIRInstruction {
            kind,
            value_type: ty,
            result,
        });
    }

    pub fn void(&mut self, kind: LMIRInstructionKind) {
        self.emit(kind, LMIRType::unit(), None);
    }

    pub fn temp(&mut self, kind: LMIRInstructionKind, ty: LMIRType) -> LMIRValue {
        let register = LMIRRegister::new(format!("tmp.{}", self.next_register));
        self.next_register += 1;
        self.emit(kind, ty.clone(), Some(register.clone()));
        LMIRValue::Register {
            register,
            _type: ty,
        }
    }

    pub fn assign(&mut self, out: MIRRegister, kind: LMIRInstructionKind) {
        let LMIRValue::Register { register, _type } = self.reg(out) else {
            unreachable!()
        };
        self.emit(kind, _type, Some(register));
    }

    pub fn integer(&self, value: i128, ty: LMIRIntegerType) -> LMIRValue {
        LMIRValue::IntImmediate {
            _type: LMIRType::with_implicit_abi(
                self.types().architecture(),
                LMIRTypeKind::Integer(ty),
            ),
            val: value as i64,
        }
    }

    pub fn offset(&mut self, base: LMIRValue, offset: i64) -> LMIRValue {
        if offset == 0 {
            return base;
        }
        self.temp(
            LMIRInstructionKind::PointerBinOp {
                op: LMIRPtrBinOp::ADD,
                ptr_type: self.pointer(),
                type_size: TypeSize::from(1),
                left: base,
                right: self.integer(offset.into(), LMIRIntegerType::I64),
            },
            self.pointer(),
        )
    }

    pub fn allocate(&mut self, ty: MIRTypeID) -> LMIRValue {
        let layout = calculate_type_layout(self.types(), ty);
        self.temp(
            LMIRInstructionKind::Allocate {
                _type: self.ty(ty),
                alignment: layout.alignment() as u8,
            },
            self.pointer(),
        )
    }

    pub fn store(&mut self, address: LMIRValue, value: LMIRValue, ty: MIRTypeID) {
        let lowered = self.ty(ty);
        if lowered.is_void() {
            return;
        }
        if lowered.is_memory_resident() {
            let layout = calculate_type_layout(self.types(), ty);
            self.void(LMIRInstructionKind::Memcpy {
                dest: address,
                src: value,
                size: self.integer(layout.size() as i128, LMIRIntegerType::I64),
                alignment: layout.alignment() as u8,
            });
        } else {
            self.void(LMIRInstructionKind::Store {
                memory: address,
                value,
                _type: lowered,
            });
        }
    }

    pub fn load(&mut self, address: LMIRValue, ty: MIRTypeID) -> LMIRValue {
        let lowered = self.ty(ty);
        if lowered.is_memory_resident() {
            address
        } else {
            self.temp(
                LMIRInstructionKind::Load {
                    memory: address,
                    _type: lowered.clone(),
                },
                lowered,
            )
        }
    }

    pub fn target(&mut self, target: &cx_mir::MIRBlockTarget) -> LMIRBlockTarget {
        let id = self.blocks[self.block_indices[&target.block]].id.clone();
        let params = self
            .body
            .block(target.block)
            .expect("unknown MIR target block")
            .params();
        let args = target
            .args
            .iter()
            .zip(params)
            .map(|(arg, parameter)| {
                let ty = self.body.register(*parameter).unwrap().ty;
                let value = crate::lowering::values::lower_rvalue(self, arg, ty);
                if self.ty(ty).is_memory_resident() {
                    let copy = self.allocate(ty);
                    self.store(copy.clone(), value, ty);
                    copy
                } else {
                    value
                }
            })
            .collect();
        LMIRBlockTarget::with_args(id, args)
    }

    pub fn finish(self) -> LMIRFunction {
        LMIRFunction {
            prototype: self.prototype,
            blocks: self.blocks,
        }
    }
}
