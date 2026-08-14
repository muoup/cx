use super::*;

impl<'a> FunctionLowerer<'a> {
    pub(super) fn lower_instruction(&mut self, instruction: &MIRInstrKind) {
        match instruction {
            MIRInstrKind::ScopeEnter { .. }
            | MIRInstrKind::ScopeExit { .. }
            | MIRInstrKind::Initialize { .. }
            | MIRInstrKind::Leak { .. }
            | MIRInstrKind::Emit { .. } => {}
            MIRInstrKind::Create { out, ty } => {
                let lowered = self.ty(*ty);
                let layout = self.layout(*ty);
                let address = self.allocate_temp(&lowered, layout.alignment as u8);
                self.places.insert(
                    *out,
                    PlaceBinding::Address {
                        value: address,
                        ty: *ty,
                    },
                );
            }
            MIRInstrKind::Assign { dest, value, ty } => {
                let binding = self.place(*dest);
                let value = self.lower_value(value, Some(*ty));
                self.store_binding(binding, value, *ty);
            }
            MIRInstrKind::AddressOf { out, place } => {
                let binding = self.place(*place);
                let address = match binding {
                    PlaceBinding::Address { value, .. } => value,
                    PlaceBinding::Bitfield { .. } => panic!("cannot take address of bitfield"),
                };
                self.emit_to(*out, LMIRInstructionKind::Alias { value: address });
            }
            MIRInstrKind::Dereference {
                out,
                pointer,
                pointee_type,
            } => {
                let value = self.lower_value(pointer, None);
                self.places.insert(
                    *out,
                    PlaceBinding::Address {
                        value,
                        ty: *pointee_type,
                    },
                );
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
            } => self.lower_coercion(*out, operand, coercion, *to_type),
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
                ..
            } => {
                let tag = self.load_discriminant(self.place(*subject), *sum_type, None);
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
                    MIRPlaceAggregateOp::Field {
                        base,
                        field,
                        aggregate_type,
                    } => self.field_binding(self.place(*base), *aggregate_type, *field),
                    MIRPlaceAggregateOp::Index {
                        base,
                        index,
                        element_type,
                    } => {
                        let base = self.address(self.place(*base));
                        let index = self.lower_value(index, None);
                        let element = self.ty(*element_type);
                        let address = self.emit_temp(
                            LMIRInstructionKind::PointerBinOp {
                                op: LMIRPtrBinOp::ADD,
                                ptr_type: element.clone(),
                                type_size: TypeSize::from(self.layout(*element_type).size),
                                left: base,
                                right: index,
                            },
                            LMIRType::default_pointer(self.types.architecture()),
                        );
                        PlaceBinding::Address {
                            value: address,
                            ty: *element_type,
                        }
                    }
                    MIRPlaceAggregateOp::Variant {
                        base,
                        variant,
                        sum_type,
                    } => PlaceBinding::Address {
                        value: self.address(self.place(*base)),
                        ty: self.variant_type(*sum_type, *variant),
                    },
                };
                self.places.insert(*out, binding);
            }
            MIRAggregateOp::Value { out, op } => match op {
                MIRValueAggregateOp::Discriminant { value, sum_type } => {
                    let binding = self.value_as_binding(value, *sum_type);
                    self.load_discriminant(binding, *sum_type, Some(*out));
                }
                MIRValueAggregateOp::Construct { ty, fields } => {
                    self.lower_construct(*out, *ty, fields)
                }
                MIRValueAggregateOp::Variant {
                    variant,
                    value,
                    sum_type,
                } => self.lower_variant_construct(*out, *variant, value, *sum_type),
            },
        }
    }

    fn lower_construct(&mut self, out: MIRRegister, ty: MIRTypeID, fields: &[(usize, MIRValue)]) {
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
            let (address, field_ty) = match self.types.kind(ty).expect("invalid MIR aggregate type")
            {
                MIRTypeKind::Structured { .. } => {
                    let binding = self.field_binding(
                        PlaceBinding::Address {
                            value: base.clone(),
                            ty,
                        },
                        ty,
                        *index,
                    );

                    match binding {
                        PlaceBinding::Address { value, ty } => (value, ty),
                        bitfield @ PlaceBinding::Bitfield { .. } => {
                            let value = self.lower_value(value, None);
                            self.store_binding(bitfield, value, self.register_decl_type(out));
                            continue;
                        }
                    }
                }

                MIRTypeKind::Array { inner, .. } => {
                    let field_ty = *inner;
                    let element = self.ty(field_ty);
                    (
                        self.offset_address(
                            base.clone(),
                            index * self.layout(field_ty).size,
                            &element,
                        ),
                        field_ty,
                    )
                }

                _ => unreachable!("construct aggregate has non-structured, non-array type"),
            };
            let value = self.lower_value(value, Some(field_ty));
            self.store_address(address, value, field_ty);
        }
    }

    fn lower_variant_construct(
        &mut self,
        out: MIRRegister,
        variant: usize,
        value: &MIRValue,
        sum_type: MIRTypeID,
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
        let value = self.lower_value(value, Some(variant_ty));
        self.store_address(base.clone(), value, variant_ty);
        let tag_ty = LMIRType::with_implicit_abi(
            self.types.architecture(),
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
                ptr_type: self.ty(*pointee),
                type_size: TypeSize::from(self.layout(*pointee).size),
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
                ptr_type: LMIRType::default_pointer(self.types.architecture()),
                type_size: TypeSize::from(1),
                left: lhs,
                right: rhs,
            },
        };
        self.emit_to(out, kind);
    }

    fn lower_unary(&mut self, out: MIRRegister, op: &MIRUnaryOp, operand: &MIRValue) {
        if let MIRUnaryOp::Increment { amount, post } = op {
            let place_id = match operand {
                MIRValue::Place(place) | MIRValue::Move(place) => *place,
                _ => panic!("increment requires a place operand"),
            };
            let place = self.place(place_id);
            let ty = self.binding_type(&place);
            let previous = self.load_binding(place.clone(), Some(ty), None);
            let amount = self.int_constant(*amount as i128, self.integer_kind(ty));
            let result = self.emit_temp(
                LMIRInstructionKind::IntegerBinOp {
                    op: LMIRIntBinOp::ADD,
                    left: previous.clone(),
                    right: amount,
                },
                self.ty(ty),
            );
            self.store_binding(place, result.clone(), ty);
            let value = if matches!(
                self.types.kind(self.register_decl_type(out)),
                Some(MIRTypeKind::MemoryReference { .. })
            ) {
                self.address(self.place(place_id))
            } else if *post {
                previous
            } else {
                result
            };
            self.emit_to(out, LMIRInstructionKind::Alias { value });
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
        to_type: MIRTypeID,
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
                if matches!(to, MIRIntType::I1) {
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
                let parameter_type = self.call_parameter_type(callee, index);
                lowered_args.extend(self.lower_call_argument(argument, parameter, parameter_type));
            } else {
                lowered_args.push(self.lower_value(argument, None));
            }
        }
        let return_type = out
            .map(|register| self.register_decl_type(register))
            .map(|ty| self.ty(ty))
            .unwrap_or_else(LMIRType::unit);
        let return_buffer = if matches!(signature.return_abi, LMIRReturnABI::IndirectSret { .. }) {
            let semantic = out
                .map(|register| self.register_decl_type(register))
                .expect("indirect return without result");
            let buffer = self.allocate_temp(&return_type, self.layout(semantic).alignment as u8);
            lowered_args.insert(0, buffer.clone());
            Some(buffer)
        } else {
            None
        };
        let call_kind = match callee {
            MIRValue::Constant(MIRConstant::Function(id)) => {
                let function = self.unit.function(*id).expect("invalid direct callee");
                LMIRInstructionKind::DirectCall {
                    func: function.prototype.signature.symbol_name.clone(),
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
                        alignment: self.layout(semantic).alignment as u8,
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
        parameter_type: Option<MIRTypeID>,
    ) -> Vec<LMIRValue> {
        match &parameter.abi {
            LMIRParameterABI::Direct { slots } if parameter._type.is_memory_resident() => {
                let source = self.lower_value(argument, parameter_type);
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
                let source = self.lower_value(argument, parameter_type);
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
            LMIRParameterABI::Direct { .. } => {
                vec![self.lower_value(argument, parameter_type)]
            }
        }
    }

    fn lower_return(&mut self, value: Option<&MIRValue>) {
        let return_abi = self.prototype.signature.return_abi.clone();
        match (return_abi, value) {
            (LMIRReturnABI::IndirectSret { alignment }, Some(value)) => {
                let semantic = self.function.prototype.signature.return_type.clone();
                let source = self.lower_value(value, Some(semantic));
                let size =
                    self.int_constant(self.layout(semantic).size as i128, LMIRIntegerType::I64);
                self.emit_void(LMIRInstructionKind::Memcpy {
                    dest: LMIRValue::ParameterRef(0),
                    src: source,
                    size,
                    alignment,
                });
                self.emit_void(LMIRInstructionKind::Return { value: None });
            }
            (_, value) => {
                let expected = self.function.prototype.signature.return_type;
                let value = value.map(|value| self.lower_value(value, Some(expected)));
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
}
