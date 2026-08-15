use super::*;

impl<'a> FunctionLowerer<'a> {
    pub(super) fn lower_value(&mut self, value: &MIRValue) -> LMIRValue {
        match value {
            MIRValue::Register(register) => self.register(*register),
            MIRValue::Place(place) => self.lower_reference(*place),
            MIRValue::Copy(place) => self.copy_place(*place),
            MIRValue::Move(place) => self.move_place(*place),
            MIRValue::Constant(constant) => self.lower_constant(constant),
        }
    }

    fn lower_reference(&self, place: MIRPlace) -> LMIRValue {
        self.address(self.place(place))
    }

    fn move_place(&mut self, place: MIRPlace) -> LMIRValue {
        let binding = self.place(place);
        let ty = self.binding_type(&binding);
        if self.is_direct_reference_parameter(place)
            || self.is_address_valued(ty)
            || self.ty(ty).is_memory_resident()
        {
            self.address(binding)
        } else {
            self.load_binding(binding, ty, None)
        }
    }

    fn copy_place(&mut self, place: MIRPlace) -> LMIRValue {
        let binding = self.place(place);
        let ty = self.binding_type(&binding);
        if self.is_direct_reference_parameter(place) || self.is_address_valued(ty) {
            return self.address(binding);
        }
        let lowered = self.ty(ty);
        if lowered.is_memory_resident() {
            let layout = self.layout(ty);
            let destination = self.allocate_temp(&lowered, layout.alignment as u8);
            let size = self.int_constant(layout.size as i128, LMIRIntegerType::I64);
            self.emit_void(LMIRInstructionKind::Memcpy {
                dest: destination.clone(),
                src: self.address(binding),
                size,
                alignment: layout.alignment as u8,
            });
            destination
        } else {
            self.load_binding(binding, ty, None)
        }
    }

    pub(super) fn is_address_valued(&self, ty: MIRTypeID) -> bool {
        matches!(self.types.kind(ty), Some(MIRTypeKind::Str))
    }

    fn is_direct_reference_parameter(&self, place: MIRPlace) -> bool {
        let MIRPlace::Parameter(parameter) = place else {
            return false;
        };
        matches!(
            self.function
                .prototype
                .signature
                .params
                .get(parameter.index())
                .and_then(|parameter| self.types.kind(parameter.ty)),
            Some(MIRTypeKind::MemoryReference { .. })
        )
    }

    pub(super) fn lower_constant(&mut self, constant: &MIRConstant) -> LMIRValue {
        match constant {
            MIRConstant::Unit => LMIRValue::NULL,
            MIRConstant::Bool(value) => self.int_constant(i128::from(*value), LMIRIntegerType::I1),
            MIRConstant::Integer { value, ty, .. } => {
                self.int_constant(*value, convert_integer_type(*ty))
            }
            MIRConstant::Float { value, ty } => LMIRValue::FloatImmediate {
                val: *value,
                _type: LMIRType::with_implicit_abi(
                    self.types.architecture(),
                    LMIRTypeKind::Float(convert_float_type(*ty)),
                ),
            },
            MIRConstant::Function(function) => LMIRValue::FunctionRef(
                self.unit
                    .function(*function)
                    .expect("invalid MIR function constant")
                    .prototype
                    .signature
                    .symbol_name
                    .clone(),
            ),
            MIRConstant::Null { ty } => {
                let pointer_integer = convert_integer_type(self.types.pointer_integer_type());
                let zero = self.int_constant(0, pointer_integer);
                self.emit_temp(
                    LMIRInstructionKind::Coercion {
                        value: zero,
                        coercion_type: LMIRCoercionType::IntToPtr {
                            from: pointer_integer,
                            sextend: false,
                        },
                    },
                    self.ty(*ty),
                )
            }
            MIRConstant::String(_) => panic!("string constants must be lowered as globals"),
            MIRConstant::Undefined => panic!("cannot lower undefined MIR value"),
        }
    }

    pub(super) fn load_binding(
        &mut self,
        binding: PlaceBinding,
        ty: MIRTypeID,
        result: Option<MIRRegister>,
    ) -> LMIRValue {
        match binding {
            PlaceBinding::Address {
                value,
                ty: binding_type,
            } => {
                debug_assert!(self.types.same_type(binding_type, ty));
                let lowered = self.ty(binding_type);
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
                let storage = self.ty(storage_type);
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
                                .int_constant(bit_offset as i128, self.integer_kind(storage_type)),
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
                        right: self.int_constant(mask, self.integer_kind(storage_type)),
                    },
                    storage,
                );
                let target = self.ty(value_type);
                if self.ty(storage_type) != target {
                    let coercion = if self.ty(storage_type).size() > target.size() {
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

    pub(super) fn store_binding(&mut self, binding: PlaceBinding, value: LMIRValue, ty: MIRTypeID) {
        match binding {
            PlaceBinding::Address { value: address, .. } => self.store_address(address, value, ty),
            PlaceBinding::Bitfield {
                address,
                storage_type,
                bit_offset,
                bit_width,
                ..
            } => {
                let storage = self.ty(storage_type);
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
                        right: self.int_constant(!shifted_mask, self.integer_kind(storage_type)),
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
                        right: self.int_constant(raw_mask, self.integer_kind(storage_type)),
                    },
                    storage.clone(),
                );
                if bit_offset != 0 {
                    inserted = self.emit_temp(
                        LMIRInstructionKind::IntegerBinOp {
                            op: LMIRIntBinOp::SHL,
                            left: inserted,
                            right: self
                                .int_constant(bit_offset as i128, self.integer_kind(storage_type)),
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

    pub(super) fn store_address(&mut self, address: LMIRValue, value: LMIRValue, ty: MIRTypeID) {
        let lowered = self.ty(ty);
        if lowered.is_void() {
            return;
        }
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

    pub(super) fn value_as_binding(&mut self, value: &MIRValue, ty: MIRTypeID) -> PlaceBinding {
        match value {
            MIRValue::Place(place) => self.place(*place),
            _ => PlaceBinding::Address {
                value: self.lower_value(value),
                ty,
            },
        }
    }

    pub(super) fn load_discriminant(
        &mut self,
        binding: PlaceBinding,
        sum_type: MIRTypeID,
        result: Option<MIRRegister>,
    ) -> LMIRValue {
        let base = self.address(binding);
        let tag_ty = LMIRType::with_implicit_abi(
            self.types.architecture(),
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

    pub(super) fn field_binding(
        &mut self,
        base: PlaceBinding,
        aggregate: MIRTypeID,
        index: usize,
    ) -> PlaceBinding {
        let base = self.address(base);
        let layout = self
            .types
            .field_layout(aggregate, index)
            .unwrap_or_else(|error| panic!("invalid MIR field projection: {error}"));
        match layout {
            MIRFieldLayout::Standard { offset, ty } => PlaceBinding::Address {
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
                        LMIRType::default_pointer(self.types.architecture()),
                    )
                },
                ty,
            },
            MIRFieldLayout::Bitfield {
                offset,
                bit_offset,
                bit_width,
                storage_type,
            } => {
                let storage = self.ty(storage_type);
                let address = self.offset_address(base, offset, &storage);
                PlaceBinding::Bitfield {
                    address,
                    storage_type,
                    value_type: storage_type,
                    bit_offset,
                    bit_width,
                }
            }
        }
    }

    pub(super) fn lower_target(&mut self, target: &MIRBlockTarget) -> LMIRBlockTarget {
        LMIRBlockTarget::with_args(
            Self::block_id(target.block),
            target
                .args
                .iter()
                .map(|value| self.lower_value(value))
                .collect(),
        )
    }

    pub(super) fn unreachable_target(&mut self) -> LMIRBlockTarget {
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
}
