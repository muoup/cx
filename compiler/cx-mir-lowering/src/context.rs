use std::collections::HashMap;

use cx_lmir::types::{LMIRIntegerType, LMIRType, LMIRTypeKind};
use cx_lmir::{
    LMIRBasicBlock, LMIRBlockTarget, LMIRFunction, LMIRFunctionMap, LMIRFunctionPrototype,
    LMIRGlobalType, LMIRGlobalValue, LMIRInstruction, LMIRInstructionKind, LMIRRegister, LMIRUnit,
    LMIRValue, LinkageType,
};
use cx_mir::ty::interface::MTRegistry;
use cx_mir::{
    MIRBasicBlockID, MIRBody, MIRFunction, MIRGlobalID, MIRPlaceID, MIRRegister, MIRTypeID, MIRUnit,
};
use cx_util::identifier::CXIdent;

use crate::lowering::memory;
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

    pub fn integer(&self, value: i128, ty: LMIRIntegerType) -> LMIRValue {
        LMIRValue::IntImmediate {
            _type: LMIRType::with_implicit_abi(
                self.types().architecture(),
                LMIRTypeKind::Integer(ty),
            ),
            val: value as i64,
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
            .filter_map(|(arg, parameter)| {
                let ty = self.body.register(*parameter).unwrap().ty;
                if self.ty(ty).is_void() {
                    return None;
                }
                let value = crate::lowering::values::lower_rvalue(self, arg, ty);
                if self.ty(ty).is_memory_resident() {
                    let copy = memory::allocate(self, ty);
                    memory::store(self, copy.clone(), value, ty);
                    Some(copy)
                } else {
                    Some(value)
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
