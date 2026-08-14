use std::collections::HashMap;

use cx_lmir::compiler_functions::ASSERTION;
use cx_lmir::types::{LMIRIntegerType, LMIRType, LMIRTypeKind, TypeSize};
use cx_lmir::{
    LMIRABISlot, LMIRBasicBlock, LMIRBlockParameter, LMIRBlockTarget, LMIRCoercionType,
    LMIRFloatBinOp, LMIRFloatUnOp, LMIRFunction, LMIRFunctionMap, LMIRFunctionPrototype,
    LMIRFunctionSignature, LMIRGlobalInitializer, LMIRGlobalState as LoweredGlobalState,
    LMIRGlobalType, LMIRGlobalValue, LMIRInstruction, LMIRInstructionKind, LMIRIntBinOp,
    LMIRIntUnOp, LMIRParameter, LMIRParameterABI, LMIRPtrBinOp, LMIRRegister, LMIRReturnABI,
    LMIRUnit, LMIRValue, LinkageType,
};
use cx_log::CXResult;
use cx_mir::{
    MIRAggregateOp, MIRBasicBlockID, MIRBinaryOp, MIRBlockTarget, MIRCoercion, MIRConstant,
    MIRFieldLayout, MIRFloatBinaryOp, MIRFnParam, MIRFnSignature, MIRFunction, MIRFunctionType,
    MIRGlobalState, MIRInstrKind, MIRIntBinaryOp, MIRIntType, MIRPlace, MIRPlaceAggregateOp,
    MIRPointerBinaryOp, MIRPointerOffsetOp, MIRRegister, MIRTypeID, MIRTypeKind, MIRTypeRegistry,
    MIRUnaryOp, MIRUnit, MIRValue, MIRValueAggregateOp,
};
use cx_util::identifier::CXIdent;

use crate::typing::{
    classify_signature, convert_float_type, convert_integer_type, convert_linkage,
    convert_prototype, convert_type,
};

#[path = "instructions.rs"]
mod instructions;
#[path = "memory.rs"]
mod memory;

pub(crate) fn lower_unit(mir: &MIRUnit, types: &MIRTypeRegistry) -> CXResult<LMIRUnit> {
    let mut prototypes = LMIRFunctionMap::new();
    for function in &mir.functions {
        let prototype = convert_prototype(&function.prototype, types);
        prototypes.insert(prototype.name.to_string(), prototype);
    }
    prototypes
        .entry(ASSERTION.symbol_name())
        .or_insert_with(|| assertion_prototype(types));

    let mut globals = mir
        .globals
        .iter()
        .map(|global| {
            let linkage = if matches!(global.state, MIRGlobalState::External) {
                LinkageType::External
            } else {
                convert_linkage(global.linkage)
            };
            let lowered_type = convert_type(global.ty, types);
            let lowered = match &global.state {
                MIRGlobalState::External => LMIRGlobalType::Variable {
                    _type: lowered_type,
                    state: LoweredGlobalState::External,
                },
                MIRGlobalState::ZeroInitialized => LMIRGlobalType::Variable {
                    _type: lowered_type,
                    state: LoweredGlobalState::ZeroInitialized,
                },
                MIRGlobalState::Initialized(MIRConstant::String(value)) => {
                    LMIRGlobalType::StringLiteral(value.clone())
                }
                MIRGlobalState::Initialized(MIRConstant::Unit) => LMIRGlobalType::Variable {
                    _type: lowered_type,
                    state: LoweredGlobalState::ZeroInitialized,
                },
                MIRGlobalState::Initialized(constant) => LMIRGlobalType::Variable {
                    _type: lowered_type,
                    state: LoweredGlobalState::Initialized(lower_global_initializer(constant)),
                },
            };
            LMIRGlobalValue {
                name: global.name.clone(),
                _type: lowered,
                linkage,
            }
        })
        .collect::<Vec<_>>();

    let mut functions = Vec::new();
    for function in &mir.functions {
        if function.is_declaration() {
            continue;
        }
        let lowerer = FunctionLowerer::new(mir, function, types, &prototypes, &mut globals);
        functions.push(lowerer.lower()?);
    }

    Ok(LMIRUnit {
        architecture: *types.architecture(),
        fn_map: prototypes,
        fn_defs: functions,
        global_vars: globals,
    })
}

fn lower_global_initializer(constant: &MIRConstant) -> LMIRGlobalInitializer {
    match constant {
        MIRConstant::Bool(value) => LMIRGlobalInitializer::Integer {
            value: i128::from(*value),
            _type: LMIRIntegerType::I1,
            signed: false,
        },
        MIRConstant::Integer { value, ty, signed } => LMIRGlobalInitializer::Integer {
            value: *value,
            _type: convert_integer_type(*ty),
            signed: *signed,
        },
        MIRConstant::Float { value, ty } => LMIRGlobalInitializer::Float {
            value: *value,
            _type: convert_float_type(*ty),
        },
        MIRConstant::Null { .. } => LMIRGlobalInitializer::Null,
        MIRConstant::Unit
        | MIRConstant::String(_)
        | MIRConstant::Function(_)
        | MIRConstant::Undefined => panic!("unsupported MIR global initializer: {constant:?}"),
    }
}

fn assertion_prototype(types: &MIRTypeRegistry) -> LMIRFunctionPrototype {
    let pointer = LMIRType::default_pointer(types.architecture());
    LMIRFunctionPrototype {
        name: CXIdent::new(ASSERTION.symbol_name()),
        linkage: LinkageType::External,
        signature: LMIRFunctionSignature {
            return_type: LMIRType::unit(),
            return_abi: LMIRReturnABI::Void,
            params: vec![
                LMIRParameter {
                    name: Some(CXIdent::new("condition")),
                    _type: LMIRType::bool(),
                    abi: LMIRParameterABI::Direct {
                        slots: vec![LMIRABISlot {
                            _type: LMIRType::bool(),
                            offset: 0,
                        }],
                    },
                },
                LMIRParameter {
                    name: Some(CXIdent::new("message")),
                    _type: pointer.clone(),
                    abi: LMIRParameterABI::Direct {
                        slots: vec![LMIRABISlot {
                            _type: pointer,
                            offset: 0,
                        }],
                    },
                },
            ],
            var_args: false,
        },
    }
}

#[derive(Clone)]
enum PlaceBinding {
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

struct FunctionLowerer<'a> {
    unit: &'a MIRUnit,
    function: &'a MIRFunction,
    types: &'a MIRTypeRegistry,
    prototypes: &'a LMIRFunctionMap,
    globals: &'a mut Vec<LMIRGlobalValue>,
    prototype: LMIRFunctionPrototype,
    blocks: Vec<LMIRBasicBlock>,
    block_indices: HashMap<MIRBasicBlockID, usize>,
    places: HashMap<MIRPlace, PlaceBinding>,
    current: usize,
    temp: usize,
}

impl<'a> FunctionLowerer<'a> {
    fn new(
        unit: &'a MIRUnit,
        function: &'a MIRFunction,
        types: &'a MIRTypeRegistry,
        prototypes: &'a LMIRFunctionMap,
        globals: &'a mut Vec<LMIRGlobalValue>,
    ) -> Self {
        let entry = function.entry.expect("MIR definition has no entry");
        let mut order = vec![entry];
        order.extend(
            function
                .blocks
                .iter()
                .map(|block| block.id)
                .filter(|id| *id != entry),
        );
        let mut blocks = Vec::with_capacity(order.len());
        let mut block_indices = HashMap::new();
        for block_id in order {
            let block = function.block(block_id).expect("invalid MIR block");
            block_indices.insert(block_id, blocks.len());
            blocks.push(LMIRBasicBlock {
                id: Self::block_id(block_id),
                debug_name: block.debug_name.as_ref().map(ToString::to_string),
                params: block
                    .params
                    .iter()
                    .map(|register| LMIRBlockParameter {
                        register: Self::register_id(*register),
                        _type: convert_type(
                            function
                                .register(*register)
                                .expect("invalid block parameter")
                                .ty,
                            types,
                        ),
                    })
                    .collect(),
                body: Vec::new(),
            });
        }
        Self {
            unit,
            function,
            types,
            prototypes,
            globals,
            prototype: convert_prototype(&function.prototype, types),
            blocks,
            block_indices,
            places: HashMap::new(),
            current: 0,
            temp: 0,
        }
    }

    fn lower(mut self) -> CXResult<LMIRFunction> {
        self.lower_parameters();
        let order = self
            .blocks
            .iter()
            .map(|block| {
                self.function
                    .blocks
                    .iter()
                    .find(|mir| Self::block_id(mir.id) == block.id)
                    .expect("LMIR block has no MIR source")
                    .id
            })
            .collect::<Vec<_>>();
        for block_id in order {
            self.current = *self.block_indices.get(&block_id).unwrap();
            let block = self.function.block(block_id).unwrap();
            for instruction in &block.instrs {
                self.lower_instruction(&instruction.kind);
            }
        }
        Ok(LMIRFunction {
            prototype: self.prototype,
            blocks: self.blocks,
        })
    }

    fn lower_parameters(&mut self) {
        let mut abi_index = usize::from(self.prototype.signature.has_indirect_return_param());
        for (index, parameter) in self.function.prototype.signature.params.iter().enumerate() {
            let place = MIRPlace::Parameter(cx_mir::MIRParameterID::new(index));
            let lowered_type = self.ty(parameter.ty);
            let layout = self.layout(parameter.ty);
            let abi = self.prototype.signature.params[index].abi.clone();
            match abi {
                LMIRParameterABI::Direct { slots } if lowered_type.is_memory_resident() => {
                    let address = self.allocate_temp(&lowered_type, layout.alignment as u8);
                    for (slot_index, slot) in slots.iter().enumerate() {
                        let target = self.offset_address(address.clone(), slot.offset, &slot._type);
                        self.emit_void(LMIRInstructionKind::Store {
                            memory: target,
                            value: LMIRValue::ParameterRef((abi_index + slot_index) as u32),
                            _type: slot._type.clone(),
                        });
                    }
                    abi_index += slots.len();
                    self.places.insert(
                        place,
                        PlaceBinding::Address {
                            value: address,
                            ty: parameter.ty,
                        },
                    );
                }
                LMIRParameterABI::Indirect { .. } => {
                    self.places.insert(
                        place,
                        PlaceBinding::Address {
                            value: LMIRValue::ParameterRef(abi_index as u32),
                            ty: parameter.ty,
                        },
                    );
                    abi_index += 1;
                }
                LMIRParameterABI::Direct { slots }
                    if matches!(
                        self.types.kind(parameter.ty),
                        Some(MIRTypeKind::MemoryReference { .. })
                    ) =>
                {
                    debug_assert_eq!(slots.len(), 1);
                    self.places.insert(
                        place,
                        PlaceBinding::Address {
                            value: LMIRValue::ParameterRef(abi_index as u32),
                            ty: self
                                .types
                                .kind(parameter.ty)
                                .and_then(|kind| match kind {
                                    MIRTypeKind::MemoryReference { inner, .. } => Some(*inner),
                                    _ => None,
                                })
                                .expect("reference parameter is missing its pointee type"),
                        },
                    );
                    abi_index += slots.len();
                }
                LMIRParameterABI::Direct { slots } => {
                    debug_assert_eq!(slots.len(), 1);
                    let address = self.allocate_temp(&lowered_type, layout.alignment as u8);
                    self.emit_void(LMIRInstructionKind::Store {
                        memory: address.clone(),
                        value: LMIRValue::ParameterRef(abi_index as u32),
                        _type: lowered_type,
                    });
                    abi_index += slots.len();
                    self.places.insert(
                        place,
                        PlaceBinding::Address {
                            value: address,
                            ty: parameter.ty.clone(),
                        },
                    );
                }
            }
        }
    }

    fn call_signature(&self, callee: &MIRValue) -> LMIRFunctionSignature {
        if let MIRValue::Constant(MIRConstant::Function(id)) = callee {
            let name = self
                .unit
                .function(*id)
                .unwrap()
                .prototype
                .signature
                .symbol_name
                .as_str();
            return self.prototypes.get(name).unwrap().signature.clone();
        }
        let ty = self
            .value_type(callee)
            .expect("indirect callee has no type");
        let signature = self
            .callable_type(ty)
            .expect("indirect callee is not callable");
        let mir_signature = MIRFnSignature {
            symbol_name: CXIdent::new("<indirect>"),
            debug_name: None,
            params: signature
                .params
                .iter()
                .copied()
                .map(MIRFnParam::new)
                .collect(),
            return_type: signature.return_type,
            variadic: signature.variadic,
            safe: false,
        };
        classify_signature(&mir_signature, self.types)
    }

    fn callable_type(&self, ty: MIRTypeID) -> Option<&MIRFunctionType> {
        match self.types.kind(ty)? {
            MIRTypeKind::Function { signature } => Some(signature),
            MIRTypeKind::PointerTo { inner } | MIRTypeKind::MemoryReference { inner, .. } => {
                self.callable_type(*inner)
            }
            _ => None,
        }
    }

    fn value_type(&self, value: &MIRValue) -> Option<MIRTypeID> {
        match value {
            MIRValue::Register(register) => Some(self.register_decl_type(*register)),
            MIRValue::Place(place) | MIRValue::Copy(place) | MIRValue::Move(place) => {
                Some(self.place_decl_type(*place))
            }
            MIRValue::Constant(MIRConstant::Function(id)) => {
                let function = self.unit.function(*id)?;
                let signature = MIRFunctionType {
                    params: function
                        .prototype
                        .signature
                        .params
                        .iter()
                        .map(|param| param.ty)
                        .collect(),
                    return_type: function.prototype.signature.return_type,
                    variadic: function.prototype.signature.variadic,
                };
                self.types
                    .find(&cx_mir::MIRTypeDefinition::new(MIRTypeKind::Function {
                        signature,
                    }))
            }
            _ => None,
        }
    }

    fn place(&self, place: MIRPlace) -> PlaceBinding {
        match place {
            MIRPlace::Global(global) => PlaceBinding::Address {
                value: LMIRValue::Global(global.index() as u32),
                ty: self
                    .unit
                    .global(global)
                    .expect("invalid global place")
                    .ty
                    .clone(),
            },
            _ => self
                .places
                .get(&place)
                .unwrap_or_else(|| {
                    panic!("MIR place {place:?} used before its storage was lowered")
                })
                .clone(),
        }
    }

    fn address(&self, binding: PlaceBinding) -> LMIRValue {
        match binding {
            PlaceBinding::Address { value, .. } => value,
            PlaceBinding::Bitfield { .. } => {
                panic!("bitfield has no independently addressable value")
            }
        }
    }

    fn binding_type(&self, binding: &PlaceBinding) -> MIRTypeID {
        match binding {
            PlaceBinding::Address { ty, .. } => ty.clone(),
            PlaceBinding::Bitfield { value_type, .. } => value_type.clone(),
        }
    }

    fn place_decl_type(&self, place: MIRPlace) -> MIRTypeID {
        match place {
            MIRPlace::FunctionLocal(id) => self.function.place(id).unwrap().ty.clone(),
            MIRPlace::Parameter(id) => self.function.prototype.signature.params[id.index()]
                .ty
                .clone(),
            MIRPlace::Global(id) => self.unit.global(id).unwrap().ty.clone(),
        }
    }

    fn register_decl_type(&self, register: MIRRegister) -> MIRTypeID {
        self.function
            .register(register)
            .expect("invalid register")
            .ty
            .clone()
    }

    fn register(&self, register: MIRRegister) -> LMIRValue {
        LMIRValue::Register {
            register: Self::register_id(register),
            _type: self.ty(self.register_decl_type(register)),
        }
    }

    fn allocate_temp(&mut self, ty: &LMIRType, alignment: u8) -> LMIRValue {
        self.emit_temp(
            LMIRInstructionKind::Allocate {
                _type: ty.clone(),
                alignment,
            },
            LMIRType::default_pointer(self.types.architecture()),
        )
    }

    fn offset_address(&mut self, base: LMIRValue, offset: usize, pointee: &LMIRType) -> LMIRValue {
        if offset == 0 {
            return base;
        }
        self.emit_temp(
            LMIRInstructionKind::PointerBinOp {
                op: LMIRPtrBinOp::ADD,
                ptr_type: pointee.clone(),
                type_size: TypeSize::from(1),
                left: base,
                right: self.int_constant(offset as i128, LMIRIntegerType::I64),
            },
            LMIRType::default_pointer(self.types.architecture()),
        )
    }

    fn emit_to(&mut self, register: MIRRegister, kind: LMIRInstructionKind) {
        let ty = self.ty(self.register_decl_type(register));
        self.emit_kind_to(register, kind, ty);
    }

    fn emit_kind_to(&mut self, register: MIRRegister, kind: LMIRInstructionKind, ty: LMIRType) {
        self.blocks[self.current].body.push(LMIRInstruction {
            kind,
            value_type: ty,
            result: Some(Self::register_id(register)),
        });
    }

    fn emit_temp(&mut self, kind: LMIRInstructionKind, ty: LMIRType) -> LMIRValue {
        let register = LMIRRegister::new(format!("t{}", self.temp));
        self.temp += 1;
        self.blocks[self.current].body.push(LMIRInstruction {
            kind,
            value_type: ty.clone(),
            result: Some(register.clone()),
        });
        LMIRValue::Register {
            register,
            _type: ty,
        }
    }

    fn emit_void(&mut self, kind: LMIRInstructionKind) {
        self.blocks[self.current].body.push(LMIRInstruction {
            kind,
            value_type: LMIRType::unit(),
            result: None,
        });
    }

    fn ty(&self, ty: MIRTypeID) -> LMIRType {
        convert_type(ty, self.types)
    }

    fn layout(&self, ty: MIRTypeID) -> cx_mir::MIRTypeLayout {
        self.types
            .layout(ty)
            .unwrap_or_else(|err| panic!("invalid MIR type layout: {err}"))
    }

    fn int_constant(&self, value: i128, ty: LMIRIntegerType) -> LMIRValue {
        LMIRValue::IntImmediate {
            val: value as i64,
            _type: LMIRType::with_implicit_abi(
                self.types.architecture(),
                LMIRTypeKind::Integer(ty),
            ),
        }
    }

    fn integer_kind(&self, ty: MIRTypeID) -> LMIRIntegerType {
        match self.types.kind(ty) {
            Some(MIRTypeKind::Integer { ty, .. }) => convert_integer_type(*ty),
            _ => convert_integer_type(self.types.pointer_integer_type()),
        }
    }

    fn switch_constant(&self, constant: &MIRConstant) -> u64 {
        match constant {
            MIRConstant::Bool(value) => u64::from(*value),
            MIRConstant::Integer { value, .. } => *value as u64,
            _ => panic!("non-integer MIR switch constant"),
        }
    }

    fn variant_type(&self, sum_type: MIRTypeID, index: usize) -> MIRTypeID {
        let sum_type = self.semantic_sum_type(sum_type);
        let Some(MIRTypeKind::TaggedUnion { variants }) = self.types.kind(sum_type) else {
            panic!("variant operation on non-tagged union")
        };
        variants[index].ty()
    }

    fn tag_offset(&self, sum_type: MIRTypeID) -> usize {
        let sum_type = self.semantic_sum_type(sum_type);
        self.types
            .tagged_union_tag_offset(sum_type)
            .unwrap_or_else(|error| panic!("invalid tagged-union layout: {error}"))
    }

    fn semantic_sum_type(&self, sum_type: MIRTypeID) -> MIRTypeID {
        match self.types.kind(sum_type) {
            Some(MIRTypeKind::MemoryReference { inner, .. }) => *inner,
            _ => sum_type,
        }
    }

    fn block_id(block: MIRBasicBlockID) -> CXIdent {
        CXIdent::new(format!("b{}", block.index()))
    }

    fn register_id(register: MIRRegister) -> LMIRRegister {
        LMIRRegister::new(format!("r{}", register.index()))
    }
}

fn lower_int_binop(op: MIRIntBinaryOp) -> LMIRIntBinOp {
    match op {
        MIRIntBinaryOp::Add => LMIRIntBinOp::ADD,
        MIRIntBinaryOp::Sub => LMIRIntBinOp::SUB,
        MIRIntBinaryOp::Mul => LMIRIntBinOp::MUL,
        MIRIntBinaryOp::SignedMul => LMIRIntBinOp::IMUL,
        MIRIntBinaryOp::Div => LMIRIntBinOp::UDIV,
        MIRIntBinaryOp::SignedDiv => LMIRIntBinOp::IDIV,
        MIRIntBinaryOp::Mod => LMIRIntBinOp::UREM,
        MIRIntBinaryOp::SignedMod => LMIRIntBinOp::IREM,
        MIRIntBinaryOp::Eq => LMIRIntBinOp::EQ,
        MIRIntBinaryOp::Ne => LMIRIntBinOp::NE,
        MIRIntBinaryOp::Lt => LMIRIntBinOp::ULT,
        MIRIntBinaryOp::Le => LMIRIntBinOp::ULE,
        MIRIntBinaryOp::Gt => LMIRIntBinOp::UGT,
        MIRIntBinaryOp::Ge => LMIRIntBinOp::UGE,
        MIRIntBinaryOp::SignedLt => LMIRIntBinOp::ILT,
        MIRIntBinaryOp::SignedLe => LMIRIntBinOp::ILE,
        MIRIntBinaryOp::SignedGt => LMIRIntBinOp::IGT,
        MIRIntBinaryOp::SignedGe => LMIRIntBinOp::IGE,
        MIRIntBinaryOp::LogicalAnd => LMIRIntBinOp::LAND,
        MIRIntBinaryOp::LogicalOr => LMIRIntBinOp::LOR,
        MIRIntBinaryOp::BitAnd => LMIRIntBinOp::BAND,
        MIRIntBinaryOp::BitOr => LMIRIntBinOp::BOR,
        MIRIntBinaryOp::BitXor => LMIRIntBinOp::BXOR,
        MIRIntBinaryOp::ShiftLeft => LMIRIntBinOp::SHL,
        MIRIntBinaryOp::ArithmeticShiftRight => LMIRIntBinOp::ASHR,
        MIRIntBinaryOp::LogicalShiftRight => LMIRIntBinOp::LSHR,
    }
}

fn lower_float_binop(op: MIRFloatBinaryOp) -> LMIRFloatBinOp {
    match op {
        MIRFloatBinaryOp::Add => LMIRFloatBinOp::ADD,
        MIRFloatBinaryOp::Sub => LMIRFloatBinOp::SUB,
        MIRFloatBinaryOp::Mul => LMIRFloatBinOp::FMUL,
        MIRFloatBinaryOp::Div => LMIRFloatBinOp::FDIV,
        MIRFloatBinaryOp::Eq => LMIRFloatBinOp::EQ,
        MIRFloatBinaryOp::Ne => LMIRFloatBinOp::NEQ,
        MIRFloatBinaryOp::Lt => LMIRFloatBinOp::FLT,
        MIRFloatBinaryOp::Le => LMIRFloatBinOp::FLE,
        MIRFloatBinaryOp::Gt => LMIRFloatBinOp::FGT,
        MIRFloatBinaryOp::Ge => LMIRFloatBinOp::FGE,
    }
}
