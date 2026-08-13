use std::collections::HashMap;

use cx_lmir::compiler_functions::ASSERTION;
use cx_lmir::types::{LMIRIntegerType, LMIRType, LMIRTypeKind, TypeSize};
use cx_lmir::{
    LMIRABISlot, LMIRBasicBlock, LMIRBlockParameter, LMIRBlockTarget, LMIRCoercionType,
    LMIRFloatBinOp, LMIRFloatUnOp, LMIRFunction, LMIRFunctionMap, LMIRFunctionPrototype,
    LMIRFunctionSignature, LMIRGlobalType, LMIRGlobalValue, LMIRInstruction, LMIRInstructionKind,
    LMIRIntBinOp, LMIRIntUnOp, LMIRParameter, LMIRParameterABI, LMIRPtrBinOp, LMIRRegister,
    LMIRReturnABI, LMIRUnit, LMIRValue, LinkageType,
};
use cx_log::CXResult;
use cx_mir::{
    MIRAggregateKind, MIRAggregateOp, MIRBasicBlockID, MIRBinaryOp, MIRBlockTarget, MIRCoercion,
    MIRConstant, MIRFloatBinaryOp, MIRFunction, MIRGlobalInitializer, MIRInstrKind, MIRIntBinaryOp,
    MIRPlace, MIRPlaceAggregateOp, MIRPointerBinaryOp, MIRPointerOffsetOp, MIRRegister, MIRType,
    MIRUnaryOp, MIRUnit, MIRValue, MIRValueAggregateOp,
};
use cx_thir::registry::THIRDecomposedRegistry;
use cx_thir::thir::r#type::{THIRField, THIRType, THIRTypeKind};
use cx_thir::type_context::THIRTypeContext;
use cx_util::identifier::CXIdent;

use crate::typing::{
    classify_signature, convert_float_type, convert_integer_type, convert_linkage,
    convert_prototype, convert_type,
};

pub(crate) fn lower_unit(mir: &MIRUnit, registry: &THIRDecomposedRegistry) -> CXResult<LMIRUnit> {
    let mut prototypes = LMIRFunctionMap::new();
    for function in &mir.functions {
        let prototype = convert_prototype(&function.prototype, registry);
        prototypes.insert(prototype.name.to_string(), prototype);
    }
    prototypes
        .entry(ASSERTION.symbol_name())
        .or_insert_with(|| assertion_prototype(registry));

    let mut globals = mir
        .globals
        .iter()
        .map(|global| {
            let linkage = if global.is_definition {
                convert_linkage(global.linkage)
            } else {
                LinkageType::External
            };
            let lowered = match &global.initializer {
                Some(MIRGlobalInitializer::Bytes(bytes)) => {
                    LMIRGlobalType::StringLiteral(String::from_utf8_lossy(bytes).into_owned())
                }
                initializer => {
                    let initial_value = match initializer {
                        Some(MIRGlobalInitializer::Scalar(MIRConstant::Integer {
                            value, ..
                        })) => Some(*value as i64),
                        Some(MIRGlobalInitializer::Scalar(MIRConstant::Bool(value))) => {
                            Some(i64::from(*value))
                        }
                        Some(MIRGlobalInitializer::Scalar(MIRConstant::Null)) => Some(0),
                        Some(MIRGlobalInitializer::Scalar(MIRConstant::Unit)) | None => None,
                        Some(other) => panic!("unsupported MIR global initializer: {other:?}"),
                    };
                    LMIRGlobalType::Variable {
                        _type: convert_type(global.ty.as_thir(), registry),
                        initial_value,
                    }
                }
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
        let lowerer = FunctionLowerer::new(mir, function, registry, &prototypes, &mut globals);
        functions.push(lowerer.lower()?);
    }

    Ok(LMIRUnit {
        architecture: *registry.architecture(),
        fn_map: prototypes,
        fn_defs: functions,
        global_vars: globals,
    })
}

fn assertion_prototype(registry: &THIRDecomposedRegistry) -> LMIRFunctionPrototype {
    let pointer = LMIRType::default_pointer(registry.architecture());
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
        ty: MIRType,
    },
    Bitfield {
        address: LMIRValue,
        storage_type: MIRType,
        value_type: MIRType,
        bit_offset: usize,
        bit_width: usize,
    },
}

struct FunctionLowerer<'a> {
    unit: &'a MIRUnit,
    function: &'a MIRFunction,
    registry: &'a THIRDecomposedRegistry,
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
        registry: &'a THIRDecomposedRegistry,
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
                                .ty
                                .as_thir(),
                            registry,
                        ),
                    })
                    .collect(),
                body: Vec::new(),
            });
        }
        Self {
            unit,
            function,
            registry,
            prototypes,
            globals,
            prototype: convert_prototype(&function.prototype, registry),
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
            let lowered_type = self.ty(&parameter.ty);
            let layout = self.layout(&parameter.ty);
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
                            ty: parameter.ty.clone(),
                        },
                    );
                }
                LMIRParameterABI::Indirect { .. } => {
                    self.places.insert(
                        place,
                        PlaceBinding::Address {
                            value: LMIRValue::ParameterRef(abi_index as u32),
                            ty: parameter.ty.clone(),
                        },
                    );
                    abi_index += 1;
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

    fn lower_instruction(&mut self, instruction: &MIRInstrKind) {
        match instruction {
            MIRInstrKind::Initialize { .. }
            | MIRInstrKind::Leak { .. }
            | MIRInstrKind::Emit { .. } => {}
            MIRInstrKind::Create { out, ty } => {
                let lowered = self.ty(ty);
                let layout = self.layout(ty);
                let address = self.allocate_temp(&lowered, layout.alignment as u8);
                self.places.insert(
                    *out,
                    PlaceBinding::Address {
                        value: address,
                        ty: ty.clone(),
                    },
                );
            }
            MIRInstrKind::Assign { dest, value, ty } => {
                let binding = self.place(*dest);
                let value = self.lower_value(value, Some(ty));
                self.store_binding(binding, value, ty);
            }
            MIRInstrKind::AddressOf { out, place } => {
                let binding = self.place(*place);
                let address = match binding {
                    PlaceBinding::Address { value, .. } => value,
                    PlaceBinding::Bitfield { .. } => panic!("cannot take address of bitfield"),
                };
                self.emit_to(*out, LMIRInstructionKind::Alias { value: address });
            }
            MIRInstrKind::AggregateOp(operation) => self.lower_aggregate(operation),
            MIRInstrKind::Call { out, callee, args } => self.lower_call(*out, callee, args),
            MIRInstrKind::BinOp { out, op, lhs, rhs } => self.lower_binary(*out, op, lhs, rhs),
            MIRInstrKind::UnOp { out, op, operand } => self.lower_unary(*out, op, operand),
            MIRInstrKind::Coerce {
                out,
                operand,
                coercion,
                to_type,
            } => self.lower_coercion(*out, operand, coercion, to_type),
            MIRInstrKind::Assert { condition, message } => {
                self.lower_assert(condition, message.as_deref())
            }
            MIRInstrKind::Assume { condition } => {
                let condition = self.lower_value(condition, None);
                self.emit_void(LMIRInstructionKind::CompilerAssumption { condition });
            }
            MIRInstrKind::Return { value } => self.lower_return(value.as_ref()),
            MIRInstrKind::Jump { target } => {
                let target = self.lower_target(target);
                self.emit_void(LMIRInstructionKind::Jump { target });
            }
            MIRInstrKind::Branch {
                cond,
                true_target,
                false_target,
            } => {
                let condition = self.lower_value(cond, None);
                let true_target = self.lower_target(true_target);
                let false_target = self.lower_target(false_target);
                self.emit_void(LMIRInstructionKind::Branch {
                    condition,
                    true_target,
                    false_target,
                });
            }
            MIRInstrKind::IntSwitch {
                value,
                cases,
                default,
            } => {
                let value = self.lower_value(value, None);
                let targets = cases
                    .iter()
                    .map(|(case, target)| (self.switch_constant(case), self.lower_target(target)))
                    .collect();
                let default = default
                    .as_ref()
                    .map(|target| self.lower_target(target))
                    .unwrap_or_else(|| self.unreachable_target());
                self.emit_void(LMIRInstructionKind::JumpTable {
                    value,
                    targets,
                    default,
                });
            }
            MIRInstrKind::VariantSwitch {
                subject,
                sum_type,
                cases,
                default,
            } => {
                let tag = self.load_discriminant(self.place(*subject), sum_type, None);
                let targets = cases
                    .iter()
                    .map(|(case, target)| (*case as u64, self.lower_target(target)))
                    .collect();
                let default = default
                    .as_ref()
                    .map(|target| self.lower_target(target))
                    .unwrap_or_else(|| self.unreachable_target());
                self.emit_void(LMIRInstructionKind::JumpTable {
                    value: tag,
                    targets,
                    default,
                });
            }
            MIRInstrKind::Unreachable => self.emit_void(LMIRInstructionKind::Unreachable),
        }
    }

    fn lower_aggregate(&mut self, operation: &MIRAggregateOp) {
        match operation {
            MIRAggregateOp::Place { out, op } => {
                let binding = match op {
                    MIRPlaceAggregateOp::Dereference {
                        pointer,
                        pointee_type,
                    } => PlaceBinding::Address {
                        value: self.lower_value(pointer, None),
                        ty: pointee_type.clone(),
                    },
                    MIRPlaceAggregateOp::Field {
                        base,
                        field,
                        aggregate_type,
                    } => self.field_binding(self.place(*base), aggregate_type, *field),
                    MIRPlaceAggregateOp::Index {
                        base,
                        index,
                        element_type,
                    } => {
                        let base = self.address(self.place(*base));
                        let index = self.lower_value(index, None);
                        let element = self.ty(element_type);
                        let address = self.emit_temp(
                            LMIRInstructionKind::PointerBinOp {
                                op: LMIRPtrBinOp::ADD,
                                ptr_type: element.clone(),
                                type_size: TypeSize::from(self.layout(element_type).size),
                                left: base,
                                right: index,
                            },
                            LMIRType::default_pointer(self.registry.architecture()),
                        );
                        PlaceBinding::Address {
                            value: address,
                            ty: element_type.clone(),
                        }
                    }
                    MIRPlaceAggregateOp::Variant {
                        base, sum_type: _, ..
                    } => PlaceBinding::Address {
                        value: self.address(self.place(*base)),
                        ty: self.place_decl_type(*out),
                    },
                };
                self.places.insert(*out, binding);
            }
            MIRAggregateOp::Value { out, op } => match op {
                MIRValueAggregateOp::Discriminant { value, sum_type } => {
                    let binding = self.value_as_binding(value, sum_type);
                    self.load_discriminant(binding, sum_type, Some(*out));
                }
                MIRValueAggregateOp::Construct { kind, ty, fields } => {
                    self.lower_construct(*out, *kind, ty, fields)
                }
                MIRValueAggregateOp::Variant {
                    variant,
                    value,
                    sum_type,
                } => self.lower_variant_construct(*out, *variant, value, sum_type),
            },
        }
    }

    fn lower_construct(
        &mut self,
        out: MIRRegister,
        kind: MIRAggregateKind,
        ty: &MIRType,
        fields: &[(usize, MIRValue)],
    ) {
        let lowered = self.ty(ty);
        let layout = self.layout(ty);
        self.emit_kind_to(
            out,
            LMIRInstructionKind::Allocate {
                _type: lowered.clone(),
                alignment: layout.alignment as u8,
            },
            lowered.clone(),
        );
        let base = self.register(out);
        for (index, value) in fields {
            let (address, field_ty) = match kind {
                MIRAggregateKind::Struct => {
                    let binding = self.field_binding(
                        PlaceBinding::Address {
                            value: base.clone(),
                            ty: ty.clone(),
                        },
                        ty,
                        *index,
                    );
                    match binding {
                        PlaceBinding::Address { value, ty } => (value, ty),
                        bitfield @ PlaceBinding::Bitfield { .. } => {
                            let value = self.lower_value(value, None);
                            self.store_binding(bitfield, value, &self.register_decl_type(out));
                            continue;
                        }
                    }
                }
                MIRAggregateKind::Array => {
                    let THIRTypeKind::Array { inner_type, .. } = ty.as_thir().kind else {
                        panic!("array aggregate has non-array type")
                    };
                    let field_ty = MIRType::new(self.registry.resolve_type_id(inner_type).clone());
                    let element = self.ty(&field_ty);
                    (
                        self.offset_address(
                            base.clone(),
                            index * self.layout(&field_ty).size,
                            &element,
                        ),
                        field_ty,
                    )
                }
            };
            let value = self.lower_value(value, Some(&field_ty));
            self.store_address(address, value, &field_ty);
        }
    }

    fn lower_variant_construct(
        &mut self,
        out: MIRRegister,
        variant: usize,
        value: &MIRValue,
        sum_type: &MIRType,
    ) {
        let lowered = self.ty(sum_type);
        let layout = self.layout(sum_type);
        self.emit_kind_to(
            out,
            LMIRInstructionKind::Allocate {
                _type: lowered.clone(),
                alignment: layout.alignment as u8,
            },
            lowered,
        );
        let base = self.register(out);
        let variant_ty = self.variant_type(sum_type, variant);
        let value = self.lower_value(value, Some(&variant_ty));
        self.store_address(base.clone(), value, &variant_ty);
        let tag_ty = LMIRType::with_implicit_abi(
            self.registry.architecture(),
            LMIRTypeKind::Integer(LMIRIntegerType::I8),
        );
        let tag_address = self.offset_address(base, self.tag_offset(sum_type), &tag_ty);
        self.emit_void(LMIRInstructionKind::Store {
            memory: tag_address,
            value: self.int_constant(variant as i128, LMIRIntegerType::I8),
            _type: tag_ty,
        });
    }

    fn lower_binary(&mut self, out: MIRRegister, op: &MIRBinaryOp, lhs: &MIRValue, rhs: &MIRValue) {
        let lhs = self.lower_value(lhs, None);
        let rhs = self.lower_value(rhs, None);
        let kind = match op {
            MIRBinaryOp::Integer { op, .. } => LMIRInstructionKind::IntegerBinOp {
                op: lower_int_binop(*op),
                left: lhs,
                right: rhs,
            },
            MIRBinaryOp::Float { op, .. } => LMIRInstructionKind::FloatBinOp {
                op: lower_float_binop(*op),
                left: lhs,
                right: rhs,
            },
            MIRBinaryOp::PointerOffset { op, pointee } => LMIRInstructionKind::PointerBinOp {
                op: match op {
                    MIRPointerOffsetOp::Add => LMIRPtrBinOp::ADD,
                    MIRPointerOffsetOp::Sub => LMIRPtrBinOp::SUB,
                },
                ptr_type: self.ty(pointee),
                type_size: TypeSize::from(self.layout(pointee).size),
                left: lhs,
                right: rhs,
            },
            MIRBinaryOp::Pointer(op) => LMIRInstructionKind::PointerBinOp {
                op: match op {
                    MIRPointerBinaryOp::Eq => LMIRPtrBinOp::EQ,
                    MIRPointerBinaryOp::Ne => LMIRPtrBinOp::NE,
                    MIRPointerBinaryOp::Lt => LMIRPtrBinOp::LT,
                    MIRPointerBinaryOp::Le => LMIRPtrBinOp::LE,
                    MIRPointerBinaryOp::Gt => LMIRPtrBinOp::GT,
                    MIRPointerBinaryOp::Ge => LMIRPtrBinOp::GE,
                },
                ptr_type: LMIRType::default_pointer(self.registry.architecture()),
                type_size: TypeSize::from(1),
                left: lhs,
                right: rhs,
            },
        };
        self.emit_to(out, kind);
    }

    fn lower_unary(&mut self, out: MIRRegister, op: &MIRUnaryOp, operand: &MIRValue) {
        if let MIRUnaryOp::Increment { amount, post } = op {
            let place = match operand {
                MIRValue::Place(place) | MIRValue::Move(place) => self.place(*place),
                _ => panic!("increment requires a place operand"),
            };
            let ty = self.register_decl_type(out);
            let previous = self.load_binding(place.clone(), Some(&ty), None);
            let amount = self.int_constant(*amount as i128, self.integer_kind(&ty));
            let result = self.emit_temp(
                LMIRInstructionKind::IntegerBinOp {
                    op: LMIRIntBinOp::ADD,
                    left: previous.clone(),
                    right: amount,
                },
                self.ty(&ty),
            );
            self.store_binding(place, result.clone(), &ty);
            self.emit_to(
                out,
                LMIRInstructionKind::Alias {
                    value: if *post { previous } else { result },
                },
            );
            return;
        }
        let value = self.lower_value(operand, None);
        let kind = match op {
            MIRUnaryOp::IntegerNeg { .. } => LMIRInstructionKind::IntegerUnOp {
                op: LMIRIntUnOp::NEG,
                value,
            },
            MIRUnaryOp::FloatNeg(_) => LMIRInstructionKind::FloatUnOp {
                op: LMIRFloatUnOp::NEG,
                value,
            },
            MIRUnaryOp::BitNot(_) => LMIRInstructionKind::IntegerUnOp {
                op: LMIRIntUnOp::BNOT,
                value,
            },
            MIRUnaryOp::LogicalNot => LMIRInstructionKind::IntegerUnOp {
                op: LMIRIntUnOp::LNOT,
                value,
            },
            MIRUnaryOp::Increment { .. } => unreachable!(),
        };
        self.emit_to(out, kind);
    }

    fn lower_coercion(
        &mut self,
        out: MIRRegister,
        operand: &MIRValue,
        coercion: &MIRCoercion,
        to_type: &MIRType,
    ) {
        let value = self.lower_value(operand, None);
        let kind = match coercion {
            MIRCoercion::TypeChange => LMIRInstructionKind::Alias { value },
            MIRCoercion::ReinterpretBits => LMIRInstructionKind::Coercion {
                value,
                coercion_type: LMIRCoercionType::BitCast,
            },
            MIRCoercion::Integral {
                sign_extend,
                from,
                to,
            } => {
                if matches!(to, cx_thir::thir::r#type::THIRIntType::I1) {
                    LMIRInstructionKind::IntegerBinOp {
                        op: LMIRIntBinOp::NE,
                        left: value,
                        right: self.int_constant(0, convert_integer_type(*from)),
                    }
                } else {
                    LMIRInstructionKind::Coercion {
                        value,
                        coercion_type: if from.bytes() > to.bytes() {
                            LMIRCoercionType::Trunc
                        } else if *sign_extend {
                            LMIRCoercionType::SExtend
                        } else {
                            LMIRCoercionType::ZExtend
                        },
                    }
                }
            }
            MIRCoercion::FloatCast { from, .. } => LMIRInstructionKind::Coercion {
                value,
                coercion_type: LMIRCoercionType::FloatCast {
                    from: convert_float_type(*from),
                },
            },
            MIRCoercion::IntToFloat { from, signed, .. } => LMIRInstructionKind::Coercion {
                value,
                coercion_type: LMIRCoercionType::IntToFloat {
                    from: convert_integer_type(*from),
                    sextend: *signed,
                },
            },
            MIRCoercion::FloatToInt { from, signed, .. } => LMIRInstructionKind::Coercion {
                value,
                coercion_type: LMIRCoercionType::FloatToInt {
                    from: convert_float_type(*from),
                    sextend: *signed,
                },
            },
            MIRCoercion::PointerToInt { .. } => LMIRInstructionKind::Coercion {
                value,
                coercion_type: LMIRCoercionType::PtrToInt,
            },
            MIRCoercion::IntToPointer { from, sign_extend } => LMIRInstructionKind::Coercion {
                value,
                coercion_type: LMIRCoercionType::IntToPtr {
                    from: convert_integer_type(*from),
                    sextend: *sign_extend,
                },
            },
            MIRCoercion::FunctionToPointer => match value {
                LMIRValue::FunctionRef(function) => LMIRInstructionKind::GetFunctionAddr {
                    func: function.to_string(),
                },
                value => LMIRInstructionKind::Alias { value },
            },
        };
        self.emit_kind_to(out, kind, self.ty(to_type));
    }

    fn lower_call(&mut self, out: Option<MIRRegister>, callee: &MIRValue, args: &[MIRValue]) {
        let signature = self.call_signature(callee);
        let mut lowered_args = Vec::new();
        for (index, argument) in args.iter().enumerate() {
            if let Some(parameter) = signature.params.get(index) {
                lowered_args.extend(self.lower_call_argument(argument, parameter));
            } else {
                lowered_args.push(self.lower_value(argument, None));
            }
        }
        let return_type = out
            .map(|register| self.register_decl_type(register))
            .map(|ty| self.ty(&ty))
            .unwrap_or_else(LMIRType::unit);
        let return_buffer = if matches!(signature.return_abi, LMIRReturnABI::IndirectSret { .. }) {
            let semantic = out
                .map(|register| self.register_decl_type(register))
                .expect("indirect return without result");
            let buffer = self.allocate_temp(&return_type, self.layout(&semantic).alignment as u8);
            lowered_args.insert(0, buffer.clone());
            Some(buffer)
        } else {
            None
        };
        let call_kind = match callee {
            MIRValue::Constant(MIRConstant::Function(id)) => {
                let function = self.unit.function(*id).expect("invalid direct callee");
                LMIRInstructionKind::DirectCall {
                    func: function.prototype.signature.name.clone(),
                    args: lowered_args,
                    method_sig: signature.clone(),
                }
            }
            _ => LMIRInstructionKind::IndirectCall {
                func_ptr: self.lower_value(callee, None),
                args: lowered_args,
                method_sig: signature.clone(),
            },
        };
        match (out, return_buffer) {
            (None, _) => self.emit_void(call_kind),
            (Some(out), Some(buffer)) => {
                self.emit_void(call_kind);
                self.emit_to(out, LMIRInstructionKind::Alias { value: buffer });
            }
            (Some(out), None) if return_type.is_memory_resident() => {
                let call = self.emit_temp(call_kind, return_type.clone());
                let semantic = self.register_decl_type(out);
                self.emit_kind_to(
                    out,
                    LMIRInstructionKind::Allocate {
                        _type: return_type.clone(),
                        alignment: self.layout(&semantic).alignment as u8,
                    },
                    return_type.clone(),
                );
                self.emit_void(LMIRInstructionKind::Store {
                    memory: self.register(out),
                    value: call,
                    _type: return_type,
                });
            }
            (Some(out), None) => self.emit_kind_to(out, call_kind, return_type),
        }
    }

    fn lower_call_argument(
        &mut self,
        argument: &MIRValue,
        parameter: &LMIRParameter,
    ) -> Vec<LMIRValue> {
        match &parameter.abi {
            LMIRParameterABI::Direct { slots } if parameter._type.is_memory_resident() => {
                let source = self.lower_value(argument, None);
                slots
                    .iter()
                    .map(|slot| {
                        let address = self.offset_address(source.clone(), slot.offset, &slot._type);
                        self.emit_temp(
                            LMIRInstructionKind::Load {
                                memory: address,
                                _type: slot._type.clone(),
                            },
                            slot._type.clone(),
                        )
                    })
                    .collect()
            }
            LMIRParameterABI::Indirect { alignment } => {
                let source = self.lower_value(argument, None);
                if matches!(argument, MIRValue::Place(_)) {
                    let copy = self.emit_temp(
                        LMIRInstructionKind::Allocate {
                            _type: parameter._type.clone(),
                            alignment: *alignment,
                        },
                        parameter._type.clone(),
                    );
                    let size = self.int_constant(
                        usize::from(parameter._type.size()) as i128,
                        LMIRIntegerType::I64,
                    );
                    self.emit_void(LMIRInstructionKind::Memcpy {
                        dest: copy.clone(),
                        src: source,
                        size,
                        alignment: *alignment,
                    });
                    vec![copy]
                } else {
                    vec![source]
                }
            }
            LMIRParameterABI::Direct { .. } => vec![self.lower_value(argument, None)],
        }
    }

    fn lower_return(&mut self, value: Option<&MIRValue>) {
        let return_abi = self.prototype.signature.return_abi.clone();
        match (return_abi, value) {
            (LMIRReturnABI::IndirectSret { alignment }, Some(value)) => {
                let semantic = self
                    .function
                    .prototype
                    .signature
                    .return_type
                    .as_ref()
                    .expect("sret function has no semantic return type")
                    .clone();
                let source = self.lower_value(value, Some(&semantic));
                let size =
                    self.int_constant(self.layout(&semantic).size as i128, LMIRIntegerType::I64);
                self.emit_void(LMIRInstructionKind::Memcpy {
                    dest: LMIRValue::ParameterRef(0),
                    src: source,
                    size,
                    alignment,
                });
                self.emit_void(LMIRInstructionKind::Return { value: None });
            }
            (_, value) => {
                let expected = self
                    .function
                    .prototype
                    .signature
                    .return_type
                    .as_ref()
                    .cloned();
                let value = value.map(|value| self.lower_value(value, expected.as_ref()));
                self.emit_void(LMIRInstructionKind::Return { value });
            }
        }
    }

    fn lower_assert(&mut self, condition: &MIRValue, message: Option<&str>) {
        let condition = self.lower_value(condition, None);
        let message = message.unwrap_or("assertion failed").to_owned();
        let global = self.globals.len() as u32;
        self.globals.push(LMIRGlobalValue {
            name: CXIdent::new(format!("assert_message_{global}")),
            _type: LMIRGlobalType::StringLiteral(message),
            linkage: LinkageType::Static,
        });
        let assertion_name = ASSERTION.symbol_name();
        let signature = self
            .prototypes
            .get(&assertion_name)
            .expect("assertion prototype was not installed")
            .signature
            .clone();
        self.emit_void(LMIRInstructionKind::DirectCall {
            func: CXIdent::new(assertion_name),
            args: vec![condition, LMIRValue::Global(global)],
            method_sig: signature,
        });
    }

    fn lower_value(&mut self, value: &MIRValue, expected: Option<&MIRType>) -> LMIRValue {
        match value {
            MIRValue::Register(register) => self.register(*register),
            MIRValue::Place(place) | MIRValue::Move(place) => {
                let binding = self.place(*place);
                let ty = self.binding_type(&binding);
                if matches!(place, MIRPlace::Global(id) if matches!(
                    self.unit.global(*id).and_then(|global| global.initializer.as_ref()),
                    Some(MIRGlobalInitializer::Bytes(_))
                )) {
                    return self.address(binding);
                }
                let lowered = self.ty(&ty);
                if lowered.is_memory_resident() {
                    self.address(binding)
                } else {
                    self.load_binding(binding, Some(&ty), None)
                }
            }
            MIRValue::Constant(constant) => self.lower_constant(constant, expected),
        }
    }

    fn lower_constant(&mut self, constant: &MIRConstant, expected: Option<&MIRType>) -> LMIRValue {
        match constant {
            MIRConstant::Unit => LMIRValue::NULL,
            MIRConstant::Bool(value) => self.int_constant(i128::from(*value), LMIRIntegerType::I1),
            MIRConstant::Integer { value, ty, .. } => {
                self.int_constant(*value, convert_integer_type(*ty))
            }
            MIRConstant::Float { value, ty } => LMIRValue::FloatImmediate {
                val: *value,
                _type: LMIRType::with_implicit_abi(
                    self.registry.architecture(),
                    LMIRTypeKind::Float(convert_float_type(*ty)),
                ),
            },
            MIRConstant::Function(function) => LMIRValue::FunctionRef(
                self.unit
                    .function(*function)
                    .expect("invalid MIR function constant")
                    .prototype
                    .signature
                    .name
                    .clone(),
            ),
            MIRConstant::Null => {
                let expected = expected.expect("null constant requires an expected type");
                let pointer_integer = convert_integer_type(self.registry.pointer_integer_type());
                let zero = self.int_constant(0, pointer_integer);
                self.emit_temp(
                    LMIRInstructionKind::Coercion {
                        value: zero,
                        coercion_type: LMIRCoercionType::IntToPtr {
                            from: pointer_integer,
                            sextend: false,
                        },
                    },
                    self.ty(expected),
                )
            }
            MIRConstant::Undefined => panic!("cannot lower undefined MIR value"),
        }
    }

    fn load_binding(
        &mut self,
        binding: PlaceBinding,
        expected: Option<&MIRType>,
        result: Option<MIRRegister>,
    ) -> LMIRValue {
        match binding {
            PlaceBinding::Address { value, ty } => {
                let lowered = self.ty(expected.unwrap_or(&ty));
                match result {
                    Some(register) => {
                        self.emit_kind_to(
                            register,
                            LMIRInstructionKind::Load {
                                memory: value,
                                _type: lowered.clone(),
                            },
                            lowered,
                        );
                        self.register(register)
                    }
                    None => self.emit_temp(
                        LMIRInstructionKind::Load {
                            memory: value,
                            _type: lowered.clone(),
                        },
                        lowered,
                    ),
                }
            }
            PlaceBinding::Bitfield {
                address,
                storage_type,
                value_type,
                bit_offset,
                bit_width,
            } => {
                let storage = self.ty(&storage_type);
                let mut value = self.emit_temp(
                    LMIRInstructionKind::Load {
                        memory: address,
                        _type: storage.clone(),
                    },
                    storage.clone(),
                );
                if bit_offset != 0 {
                    value = self.emit_temp(
                        LMIRInstructionKind::IntegerBinOp {
                            op: LMIRIntBinOp::LSHR,
                            left: value,
                            right: self
                                .int_constant(bit_offset as i128, self.integer_kind(&storage_type)),
                        },
                        storage.clone(),
                    );
                }
                let mask = if bit_width >= 64 {
                    -1
                } else {
                    (1_i128 << bit_width) - 1
                };
                value = self.emit_temp(
                    LMIRInstructionKind::IntegerBinOp {
                        op: LMIRIntBinOp::BAND,
                        left: value,
                        right: self.int_constant(mask, self.integer_kind(&storage_type)),
                    },
                    storage,
                );
                let target = self.ty(&value_type);
                if self.ty(&storage_type) != target {
                    let coercion = if self.ty(&storage_type).size() > target.size() {
                        LMIRCoercionType::Trunc
                    } else {
                        LMIRCoercionType::ZExtend
                    };
                    value = self.emit_temp(
                        LMIRInstructionKind::Coercion {
                            value,
                            coercion_type: coercion,
                        },
                        target,
                    );
                }
                if let Some(register) = result {
                    self.emit_to(register, LMIRInstructionKind::Alias { value });
                    self.register(register)
                } else {
                    value
                }
            }
        }
    }

    fn store_binding(&mut self, binding: PlaceBinding, value: LMIRValue, ty: &MIRType) {
        match binding {
            PlaceBinding::Address { value: address, .. } => self.store_address(address, value, ty),
            PlaceBinding::Bitfield {
                address,
                storage_type,
                bit_offset,
                bit_width,
                ..
            } => {
                let storage = self.ty(&storage_type);
                let current = self.emit_temp(
                    LMIRInstructionKind::Load {
                        memory: address.clone(),
                        _type: storage.clone(),
                    },
                    storage.clone(),
                );
                let raw_mask = if bit_width >= 64 {
                    -1
                } else {
                    (1_i128 << bit_width) - 1
                };
                let shifted_mask = raw_mask << bit_offset;
                let cleared = self.emit_temp(
                    LMIRInstructionKind::IntegerBinOp {
                        op: LMIRIntBinOp::BAND,
                        left: current,
                        right: self.int_constant(!shifted_mask, self.integer_kind(&storage_type)),
                    },
                    storage.clone(),
                );
                let mut inserted = value;
                if self.ty(ty) != storage {
                    inserted = self.emit_temp(
                        LMIRInstructionKind::Coercion {
                            value: inserted,
                            coercion_type: LMIRCoercionType::ZExtend,
                        },
                        storage.clone(),
                    );
                }
                inserted = self.emit_temp(
                    LMIRInstructionKind::IntegerBinOp {
                        op: LMIRIntBinOp::BAND,
                        left: inserted,
                        right: self.int_constant(raw_mask, self.integer_kind(&storage_type)),
                    },
                    storage.clone(),
                );
                if bit_offset != 0 {
                    inserted = self.emit_temp(
                        LMIRInstructionKind::IntegerBinOp {
                            op: LMIRIntBinOp::SHL,
                            left: inserted,
                            right: self
                                .int_constant(bit_offset as i128, self.integer_kind(&storage_type)),
                        },
                        storage.clone(),
                    );
                }
                let merged = self.emit_temp(
                    LMIRInstructionKind::IntegerBinOp {
                        op: LMIRIntBinOp::BOR,
                        left: cleared,
                        right: inserted,
                    },
                    storage.clone(),
                );
                self.emit_void(LMIRInstructionKind::Store {
                    memory: address,
                    value: merged,
                    _type: storage,
                });
            }
        }
    }

    fn store_address(&mut self, address: LMIRValue, value: LMIRValue, ty: &MIRType) {
        let lowered = self.ty(ty);
        if lowered.is_memory_resident() {
            let layout = self.layout(ty);
            let size = self.int_constant(layout.size as i128, LMIRIntegerType::I64);
            self.emit_void(LMIRInstructionKind::Memcpy {
                dest: address,
                src: value,
                size,
                alignment: layout.alignment as u8,
            });
        } else {
            self.emit_void(LMIRInstructionKind::Store {
                memory: address,
                value,
                _type: lowered,
            });
        }
    }

    fn value_as_binding(&mut self, value: &MIRValue, ty: &MIRType) -> PlaceBinding {
        match value {
            MIRValue::Place(place) | MIRValue::Move(place) => self.place(*place),
            _ => PlaceBinding::Address {
                value: self.lower_value(value, Some(ty)),
                ty: ty.clone(),
            },
        }
    }

    fn load_discriminant(
        &mut self,
        binding: PlaceBinding,
        sum_type: &MIRType,
        result: Option<MIRRegister>,
    ) -> LMIRValue {
        let base = self.address(binding);
        let tag_ty = LMIRType::with_implicit_abi(
            self.registry.architecture(),
            LMIRTypeKind::Integer(LMIRIntegerType::I8),
        );
        let address = self.offset_address(base, self.tag_offset(sum_type), &tag_ty);
        match result {
            Some(register) => {
                self.emit_kind_to(
                    register,
                    LMIRInstructionKind::Load {
                        memory: address,
                        _type: tag_ty.clone(),
                    },
                    tag_ty,
                );
                self.register(register)
            }
            None => self.emit_temp(
                LMIRInstructionKind::Load {
                    memory: address,
                    _type: tag_ty.clone(),
                },
                tag_ty,
            ),
        }
    }

    fn field_binding(
        &mut self,
        base: PlaceBinding,
        aggregate: &MIRType,
        index: usize,
    ) -> PlaceBinding {
        let base = self.address(base);
        match aggregate_member_layout(aggregate.as_thir(), self.registry, index) {
            MemberLayout::Standard { offset, ty } => PlaceBinding::Address {
                value: if offset == 0 {
                    base
                } else {
                    self.emit_temp(
                        LMIRInstructionKind::StructAccess {
                            struct_: base,
                            struct_type: self.ty(aggregate),
                            field_index: index,
                            field_offset: offset,
                        },
                        LMIRType::default_pointer(self.registry.architecture()),
                    )
                },
                ty,
            },
            MemberLayout::Bitfield {
                offset,
                bit_offset,
                bit_width,
                storage_type,
                value_type,
            } => {
                let storage = self.ty(&storage_type);
                let address = self.offset_address(base, offset, &storage);
                PlaceBinding::Bitfield {
                    address,
                    storage_type,
                    value_type,
                    bit_offset,
                    bit_width,
                }
            }
        }
    }

    fn lower_target(&mut self, target: &MIRBlockTarget) -> LMIRBlockTarget {
        let parameter_types = self
            .function
            .block(target.block)
            .expect("invalid block target")
            .params
            .iter()
            .map(|register| self.function.register(*register).unwrap().ty.clone())
            .collect::<Vec<_>>();
        LMIRBlockTarget::with_args(
            Self::block_id(target.block),
            target
                .args
                .iter()
                .zip(parameter_types)
                .map(|(value, ty)| self.lower_value(value, Some(&ty)))
                .collect(),
        )
    }

    fn unreachable_target(&mut self) -> LMIRBlockTarget {
        let id = CXIdent::new(format!("unreachable_{}", self.blocks.len()));
        self.blocks.push(LMIRBasicBlock {
            id: id.clone(),
            debug_name: Some("synthetic switch default".into()),
            params: Vec::new(),
            body: vec![LMIRInstruction {
                kind: LMIRInstructionKind::Unreachable,
                value_type: LMIRType::unit(),
                result: None,
            }],
        });
        LMIRBlockTarget::new(id)
    }

    fn call_signature(&self, callee: &MIRValue) -> LMIRFunctionSignature {
        if let MIRValue::Constant(MIRConstant::Function(id)) = callee {
            let name = self
                .unit
                .function(*id)
                .unwrap()
                .prototype
                .signature
                .name
                .as_str();
            return self.prototypes.get(name).unwrap().signature.clone();
        }
        let ty = self
            .value_type(callee)
            .expect("indirect callee has no type");
        let signature = self
            .registry
            .intern_signature(ty.as_thir())
            .expect("indirect callee is not callable");
        let mir_signature = cx_mir::MIRFnSignature {
            name: CXIdent::new("<indirect>"),
            params: signature
                .params
                .iter()
                .map(|parameter| cx_mir::MIRFnParam {
                    name: parameter.name.clone(),
                    ty: MIRType::new(parameter._type.clone()),
                })
                .collect(),
            return_type: (!matches!(signature.return_type.kind, THIRTypeKind::Unit))
                .then(|| MIRType::new(signature.return_type.clone())),
            variadic: signature.var_args,
        };
        classify_signature(&mir_signature, self.registry)
    }

    fn value_type(&self, value: &MIRValue) -> Option<MIRType> {
        match value {
            MIRValue::Register(register) => Some(self.register_decl_type(*register)),
            MIRValue::Place(place) | MIRValue::Move(place) => Some(self.place_decl_type(*place)),
            MIRValue::Constant(MIRConstant::Function(id)) => {
                self.unit.function(*id).map(|function| {
                    MIRType::from_kind(THIRTypeKind::Function {
                        signature: Box::new(cx_thir::thir::data::THIRFnSignature {
                            return_type: function
                                .prototype
                                .signature
                                .return_type
                                .as_ref()
                                .map(|ty| ty.as_thir().clone())
                                .unwrap_or_else(|| THIRTypeKind::Unit.into()),
                            params: function
                                .prototype
                                .signature
                                .params
                                .iter()
                                .map(|param| cx_thir::thir::data::THIRParameter {
                                    name: param.name.clone(),
                                    local_id: None,
                                    _type: param.ty.as_thir().clone(),
                                })
                                .collect(),
                            var_args: function.prototype.signature.variadic,
                            contract: Default::default(),
                        }),
                    })
                })
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

    fn binding_type(&self, binding: &PlaceBinding) -> MIRType {
        match binding {
            PlaceBinding::Address { ty, .. } => ty.clone(),
            PlaceBinding::Bitfield { value_type, .. } => value_type.clone(),
        }
    }

    fn place_decl_type(&self, place: MIRPlace) -> MIRType {
        match place {
            MIRPlace::FunctionLocal(id) => self.function.place(id).unwrap().ty.clone(),
            MIRPlace::Parameter(id) => self.function.prototype.signature.params[id.index()]
                .ty
                .clone(),
            MIRPlace::Global(id) => self.unit.global(id).unwrap().ty.clone(),
        }
    }

    fn register_decl_type(&self, register: MIRRegister) -> MIRType {
        self.function
            .register(register)
            .expect("invalid register")
            .ty
            .clone()
    }

    fn register(&self, register: MIRRegister) -> LMIRValue {
        LMIRValue::Register {
            register: Self::register_id(register),
            _type: self.ty(&self.register_decl_type(register)),
        }
    }

    fn allocate_temp(&mut self, ty: &LMIRType, alignment: u8) -> LMIRValue {
        self.emit_temp(
            LMIRInstructionKind::Allocate {
                _type: ty.clone(),
                alignment,
            },
            LMIRType::default_pointer(self.registry.architecture()),
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
            LMIRType::default_pointer(self.registry.architecture()),
        )
    }

    fn emit_to(&mut self, register: MIRRegister, kind: LMIRInstructionKind) {
        let ty = self.ty(&self.register_decl_type(register));
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

    fn ty(&self, ty: &MIRType) -> LMIRType {
        convert_type(ty.as_thir(), self.registry)
    }

    fn layout(&self, ty: &MIRType) -> cx_thir::layout::THIRTypeLayout {
        self.registry
            .type_layout(ty.as_thir())
            .unwrap_or_else(|err| panic!("invalid MIR type layout: {}", err.message()))
    }

    fn int_constant(&self, value: i128, ty: LMIRIntegerType) -> LMIRValue {
        LMIRValue::IntImmediate {
            val: value as i64,
            _type: LMIRType::with_implicit_abi(
                self.registry.architecture(),
                LMIRTypeKind::Integer(ty),
            ),
        }
    }

    fn integer_kind(&self, ty: &MIRType) -> LMIRIntegerType {
        match ty.as_thir().kind {
            THIRTypeKind::Integer { _type, .. } => convert_integer_type(_type),
            _ => convert_integer_type(self.registry.pointer_integer_type()),
        }
    }

    fn switch_constant(&self, constant: &MIRConstant) -> u64 {
        match constant {
            MIRConstant::Bool(value) => u64::from(*value),
            MIRConstant::Integer { value, .. } => *value as u64,
            _ => panic!("non-integer MIR switch constant"),
        }
    }

    fn variant_type(&self, sum_type: &MIRType, index: usize) -> MIRType {
        let THIRTypeKind::TaggedUnion { variants } = &sum_type.as_thir().kind else {
            panic!("variant operation on non-tagged union")
        };
        MIRType::new(self.registry.resolve_type_id(variants[index].ty()).clone())
    }

    fn tag_offset(&self, sum_type: &MIRType) -> usize {
        let THIRTypeKind::TaggedUnion { variants } = &sum_type.as_thir().kind else {
            panic!("discriminant operation on non-tagged union")
        };
        variants
            .iter()
            .map(|variant| {
                self.registry
                    .type_layout(self.registry.resolve_type_id(variant.ty()))
                    .unwrap_or_else(|err| panic!("invalid variant layout: {}", err.message()))
                    .size
            })
            .max()
            .unwrap_or(0)
    }

    fn block_id(block: MIRBasicBlockID) -> CXIdent {
        CXIdent::new(format!("b{}", block.index()))
    }

    fn register_id(register: MIRRegister) -> LMIRRegister {
        LMIRRegister::new(format!("r{}", register.index()))
    }
}

enum MemberLayout {
    Standard {
        offset: usize,
        ty: MIRType,
    },
    Bitfield {
        offset: usize,
        bit_offset: usize,
        bit_width: usize,
        storage_type: MIRType,
        value_type: MIRType,
    },
}

fn aggregate_member_layout(
    aggregate: &THIRType,
    registry: &THIRDecomposedRegistry,
    member: usize,
) -> MemberLayout {
    let aggregate = registry.mem_ref_inner(aggregate).unwrap_or(aggregate);
    let fields = match &aggregate.kind {
        THIRTypeKind::Structured { fields } | THIRTypeKind::Union { variants: fields } => fields,
        _ => panic!("field projection on non-aggregate type"),
    };
    let is_union = matches!(aggregate.kind, THIRTypeKind::Union { .. });
    let mut offset = 0usize;
    let mut active: Option<(cx_thir::thir::r#type::THIRTypeID, usize, usize, usize)> = None;
    for (index, field) in fields.iter().enumerate() {
        match field {
            THIRField::Standard { type_id, .. } => {
                if let Some((_, start, size, _)) = active.take() {
                    offset = start + size;
                }
                let ty = registry.resolve_type_id(*type_id);
                let layout = registry
                    .type_layout(ty)
                    .unwrap_or_else(|err| panic!("invalid field layout: {}", err.message()));
                if !is_union {
                    offset = offset.div_ceil(layout.alignment) * layout.alignment;
                } else {
                    offset = 0;
                }
                if index == member {
                    return MemberLayout::Standard {
                        offset,
                        ty: MIRType::new(ty.clone()),
                    };
                }
                if !is_union {
                    offset += layout.size;
                }
            }
            THIRField::Bitfield {
                integer_type_id,
                width,
                ..
            } => {
                let storage = registry.resolve_type_id(*integer_type_id);
                let layout = registry
                    .type_layout(storage)
                    .unwrap_or_else(|err| panic!("invalid bitfield layout: {}", err.message()));
                if is_union {
                    if index == member {
                        return MemberLayout::Bitfield {
                            offset: 0,
                            bit_offset: 0,
                            bit_width: *width,
                            storage_type: MIRType::new(storage.clone()),
                            value_type: MIRType::new(storage.clone()),
                        };
                    }
                    continue;
                }
                if *width == 0 {
                    active = None;
                    offset = offset.div_ceil(layout.alignment) * layout.alignment;
                    continue;
                }
                let (start, bit_offset) = match active.take() {
                    Some((id, start, _size, used))
                        if id == *integer_type_id && used + *width <= layout.size * 8 =>
                    {
                        (start, used)
                    }
                    Some((_, start, size, _)) => {
                        offset = start + size;
                        offset = offset.div_ceil(layout.alignment) * layout.alignment;
                        (offset, 0)
                    }
                    None => {
                        offset = offset.div_ceil(layout.alignment) * layout.alignment;
                        (offset, 0)
                    }
                };
                if index == member {
                    return MemberLayout::Bitfield {
                        offset: start,
                        bit_offset,
                        bit_width: *width,
                        storage_type: MIRType::new(storage.clone()),
                        value_type: MIRType::new(storage.clone()),
                    };
                }
                active = Some((*integer_type_id, start, layout.size, bit_offset + *width));
            }
        }
    }
    panic!("aggregate field index out of bounds")
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
