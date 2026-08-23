use cx_lmir::types::{LMIRIntegerType, LMIRType, LMIRTypeKind};
use cx_lmir::{
    LMIRBasicBlock, LMIRBlockTarget, LMIRCoercionType, LMIRInstruction, LMIRInstructionKind,
    LMIRIntBinOp, LMIRValue,
};
use cx_mir::ty::interface::MTRegistry;
use cx_mir::ty::layout::{field_layout, tagged_union_tag_offset};
use cx_mir::{
    MIRBlockTarget, MIRConstant, MIRFieldLayout, MIRPlace, MIRRegister, MIRTypeID, MIRTypeKind,
    MIRValue,
};
use cx_util::identifier::CXIdent;

use crate::context::{FunctionLoweringContext, PlaceBinding};

use super::output::{
    allocate_temp, block_id, emit_kind_to, emit_temp, emit_to, emit_void, global_index,
    int_constant, integer_kind, lowered_type, mir_layout, offset_address, register_value,
};
use super::typing::{convert_float_type, convert_integer_type};

pub(super) fn binding_for_place(
    context: &FunctionLoweringContext<'_>,
    place: MIRPlace,
) -> PlaceBinding {
    match place {
        MIRPlace::Global(global) => PlaceBinding::Address {
            value: LMIRValue::Global(global_index(context, global)),
            ty: super::globals::global_type(
                context.unit().global(global).expect("invalid global place"),
                context.types(),
            ),
        },
        _ => context
            .place_binding(place)
            .unwrap_or_else(|| panic!("MIR place {place:?} used before its storage was lowered")),
    }
}

pub(super) fn address(_context: &FunctionLoweringContext<'_>, binding: PlaceBinding) -> LMIRValue {
    match binding {
        PlaceBinding::Address { value, .. } => value,
        PlaceBinding::Bitfield { .. } => {
            panic!("bitfield has no independently addressable value")
        }
    }
}

pub(super) fn binding_type(
    _context: &FunctionLoweringContext<'_>,
    binding: &PlaceBinding,
) -> MIRTypeID {
    match binding {
        PlaceBinding::Address { ty, .. } => *ty,
        PlaceBinding::Bitfield { value_type, .. } => *value_type,
    }
}

pub(super) fn is_address_valued(context: &FunctionLoweringContext<'_>, ty: MIRTypeID) -> bool {
    matches!(context.types().kind(ty), Ok(MIRTypeKind::Str))
}

fn is_direct_reference_parameter(context: &FunctionLoweringContext<'_>, place: MIRPlace) -> bool {
    let MIRPlace::Parameter(parameter) = place else {
        return false;
    };
    matches!(
        context
            .function()
            .prototype()
            .signature
            .params
            .get(parameter.index())
            .and_then(|parameter| context.types().kind(parameter.ty).ok()),
        Some(MIRTypeKind::MemoryReference { .. })
    )
}

pub(super) fn lower_value(
    context: &mut FunctionLoweringContext<'_>,
    value: &MIRValue,
) -> LMIRValue {
    match value {
        MIRValue::Register(register) => register_value(context, *register),
        MIRValue::PlaceRef(place) => lower_reference(context, *place),
        MIRValue::Copy(place) => copy_place(context, *place),
        MIRValue::Move(place) => move_place(context, *place),
        MIRValue::Constant(constant) => lower_constant(context, constant),
    }
}

fn lower_reference(context: &FunctionLoweringContext<'_>, place: MIRPlace) -> LMIRValue {
    address(context, binding_for_place(context, place))
}

fn move_place(context: &mut FunctionLoweringContext<'_>, place: MIRPlace) -> LMIRValue {
    let binding = binding_for_place(context, place);
    let ty = binding_type(context, &binding);

    if is_direct_reference_parameter(context, place)
        || is_address_valued(context, ty)
        || lowered_type(context, ty).is_memory_resident()
    {
        address(context, binding)
    } else {
        load_binding(context, binding, ty, None)
    }
}

fn copy_place(context: &mut FunctionLoweringContext<'_>, place: MIRPlace) -> LMIRValue {
    let binding = binding_for_place(context, place);
    let ty = binding_type(context, &binding);
    if is_direct_reference_parameter(context, place) || is_address_valued(context, ty) {
        return address(context, binding);
    }
    let lowered = lowered_type(context, ty);
    if lowered.is_memory_resident() {
        let layout = mir_layout(context, ty);
        let destination = allocate_temp(context, &lowered, layout.alignment as u8);
        let size = int_constant(context, layout.size as i128, LMIRIntegerType::I64);
        emit_void(
            context,
            LMIRInstructionKind::Memcpy {
                dest: destination.clone(),
                src: address(context, binding),
                size,
                alignment: layout.alignment as u8,
            },
        );
        destination
    } else {
        load_binding(context, binding, ty, None)
    }
}

pub(super) fn lower_constant(
    context: &mut FunctionLoweringContext<'_>,
    constant: &MIRConstant,
) -> LMIRValue {
    match constant {
        MIRConstant::Unit => LMIRValue::NULL,
        MIRConstant::Bool(value) => int_constant(context, i128::from(*value), LMIRIntegerType::I1),
        MIRConstant::Integer { value, ty, .. } => {
            int_constant(context, *value, convert_integer_type(*ty))
        }
        MIRConstant::Float { value, ty } => LMIRValue::FloatImmediate {
            val: *value,
            _type: LMIRType::with_implicit_abi(
                context.types().architecture(),
                LMIRTypeKind::Float(convert_float_type(*ty)),
            ),
        },
        MIRConstant::Function(function) => LMIRValue::FunctionRef(
            context
                .unit()
                .function(*function)
                .expect("invalid MIR function constant")
                .prototype()
                .signature
                .symbol_name
                .clone(),
        ),
        MIRConstant::Null { ty } => {
            let pointer_integer = convert_integer_type(context.types().pointer_integer_type());
            let zero = int_constant(context, 0, pointer_integer);
            emit_temp(
                context,
                LMIRInstructionKind::Coercion {
                    value: zero,
                    coercion_type: LMIRCoercionType::IntToPtr {
                        from: pointer_integer,
                        sextend: false,
                    },
                },
                lowered_type(context, *ty),
            )
        }
        MIRConstant::Global { global, .. } => LMIRValue::Global(global_index(context, *global)),
        MIRConstant::GlobalOffset { .. } => {
            panic!("global offset constants must be lowered as initializers")
        }
        MIRConstant::String(_) => panic!("string constants must be lowered as globals"),
        MIRConstant::Aggregate { .. } => {
            panic!("aggregate constants must be lowered as globals")
        }
        MIRConstant::Undefined => panic!("cannot lower undefined MIR value"),
    }
}

pub(super) fn load_binding(
    context: &mut FunctionLoweringContext<'_>,
    binding: PlaceBinding,
    ty: MIRTypeID,
    result: Option<MIRRegister>,
) -> LMIRValue {
    match binding {
        PlaceBinding::Address {
            value,
            ty: binding_type,
        } => {
            debug_assert!(context.types().same_type(binding_type, ty));
            let lowered = lowered_type(context, binding_type);
            match result {
                Some(register) => {
                    emit_kind_to(
                        context,
                        register,
                        LMIRInstructionKind::Load {
                            memory: value,
                            _type: lowered.clone(),
                        },
                        lowered,
                    );
                    register_value(context, register)
                }
                None => emit_temp(
                    context,
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
            let storage = lowered_type(context, storage_type);
            let mut value = emit_temp(
                context,
                LMIRInstructionKind::Load {
                    memory: address,
                    _type: storage.clone(),
                },
                storage.clone(),
            );
            if bit_offset != 0 {
                value = emit_temp(
                    context,
                    LMIRInstructionKind::IntegerBinOp {
                        op: LMIRIntBinOp::LSHR,
                        left: value,
                        right: int_constant(
                            context,
                            bit_offset as i128,
                            integer_kind(context, storage_type),
                        ),
                    },
                    storage.clone(),
                );
            }
            let mask = if bit_width >= 64 {
                -1
            } else {
                (1_i128 << bit_width) - 1
            };
            value = emit_temp(
                context,
                LMIRInstructionKind::IntegerBinOp {
                    op: LMIRIntBinOp::BAND,
                    left: value,
                    right: int_constant(context, mask, integer_kind(context, storage_type)),
                },
                storage,
            );
            let target = lowered_type(context, value_type);
            if lowered_type(context, storage_type) != target {
                let coercion = if lowered_type(context, storage_type).size() > target.size() {
                    LMIRCoercionType::Trunc
                } else {
                    LMIRCoercionType::ZExtend
                };
                value = emit_temp(
                    context,
                    LMIRInstructionKind::Coercion {
                        value,
                        coercion_type: coercion,
                    },
                    target,
                );
            }
            if let Some(register) = result {
                emit_to(context, register, LMIRInstructionKind::Alias { value });
                register_value(context, register)
            } else {
                value
            }
        }
    }
}

pub(super) fn store_binding(
    context: &mut FunctionLoweringContext<'_>,
    binding: PlaceBinding,
    value: LMIRValue,
    ty: MIRTypeID,
) {
    match binding {
        PlaceBinding::Address { value: address, .. } => store_address(context, address, value, ty),
        PlaceBinding::Bitfield {
            address,
            storage_type,
            bit_offset,
            bit_width,
            ..
        } => {
            let storage = lowered_type(context, storage_type);
            let current = emit_temp(
                context,
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
            let cleared = emit_temp(
                context,
                LMIRInstructionKind::IntegerBinOp {
                    op: LMIRIntBinOp::BAND,
                    left: current,
                    right: int_constant(
                        context,
                        !shifted_mask,
                        integer_kind(context, storage_type),
                    ),
                },
                storage.clone(),
            );
            let mut inserted = value;
            if lowered_type(context, ty) != storage {
                inserted = emit_temp(
                    context,
                    LMIRInstructionKind::Coercion {
                        value: inserted,
                        coercion_type: LMIRCoercionType::ZExtend,
                    },
                    storage.clone(),
                );
            }
            inserted = emit_temp(
                context,
                LMIRInstructionKind::IntegerBinOp {
                    op: LMIRIntBinOp::BAND,
                    left: inserted,
                    right: int_constant(context, raw_mask, integer_kind(context, storage_type)),
                },
                storage.clone(),
            );
            if bit_offset != 0 {
                inserted = emit_temp(
                    context,
                    LMIRInstructionKind::IntegerBinOp {
                        op: LMIRIntBinOp::SHL,
                        left: inserted,
                        right: int_constant(
                            context,
                            bit_offset as i128,
                            integer_kind(context, storage_type),
                        ),
                    },
                    storage.clone(),
                );
            }
            let merged = emit_temp(
                context,
                LMIRInstructionKind::IntegerBinOp {
                    op: LMIRIntBinOp::BOR,
                    left: cleared,
                    right: inserted,
                },
                storage.clone(),
            );
            emit_void(
                context,
                LMIRInstructionKind::Store {
                    memory: address,
                    value: merged,
                    _type: storage,
                },
            );
        }
    }
}

pub(super) fn store_address(
    context: &mut FunctionLoweringContext<'_>,
    address: LMIRValue,
    value: LMIRValue,
    ty: MIRTypeID,
) {
    let lowered = lowered_type(context, ty);
    if lowered.is_void() {
        return;
    }
    if lowered.is_memory_resident() {
        let layout = mir_layout(context, ty);
        let size = int_constant(context, layout.size as i128, LMIRIntegerType::I64);
        emit_void(
            context,
            LMIRInstructionKind::Memcpy {
                dest: address,
                src: value,
                size,
                alignment: layout.alignment as u8,
            },
        );
    } else {
        emit_void(
            context,
            LMIRInstructionKind::Store {
                memory: address,
                value,
                _type: lowered,
            },
        );
    }
}

pub(super) fn value_as_binding(
    context: &mut FunctionLoweringContext<'_>,
    value: &MIRValue,
    ty: MIRTypeID,
) -> PlaceBinding {
    match value {
        MIRValue::PlaceRef(place) => binding_for_place(context, *place),
        _ => PlaceBinding::Address {
            value: lower_value(context, value),
            ty,
        },
    }
}

pub(super) fn load_discriminant(
    context: &mut FunctionLoweringContext<'_>,
    binding: PlaceBinding,
    sum_type: MIRTypeID,
    result: Option<MIRRegister>,
) -> LMIRValue {
    let base = address(context, binding);
    let tag_ty = LMIRType::with_implicit_abi(
        context.types().architecture(),
        LMIRTypeKind::Integer(LMIRIntegerType::I8),
    );
    let address = offset_address(
        context,
        base,
        tagged_union_tag_offset(context.types(), sum_type).unwrap(),
        &tag_ty,
    );

    match result {
        Some(register) => {
            emit_kind_to(
                context,
                register,
                LMIRInstructionKind::Load {
                    memory: address,
                    _type: tag_ty.clone(),
                },
                tag_ty,
            );
            register_value(context, register)
        }
        None => emit_temp(
            context,
            LMIRInstructionKind::Load {
                memory: address,
                _type: tag_ty.clone(),
            },
            tag_ty,
        ),
    }
}

pub(super) fn field_binding(
    context: &mut FunctionLoweringContext<'_>,
    base: PlaceBinding,
    aggregate: MIRTypeID,
    index: usize,
) -> PlaceBinding {
    let base = address(context, base);
    let layout = field_layout(context.types(), aggregate, index).unwrap();

    match layout {
        MIRFieldLayout::Standard { offset, ty } => PlaceBinding::Address {
            value: if offset == 0 {
                base
            } else {
                emit_temp(
                    context,
                    LMIRInstructionKind::StructAccess {
                        struct_: base,
                        struct_type: lowered_type(context, aggregate),
                        field_index: index,
                        field_offset: offset,
                    },
                    LMIRType::default_pointer(context.types().architecture()),
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
            let storage = lowered_type(context, storage_type);
            let address = offset_address(context, base, offset, &storage);
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

pub(super) fn lower_target(
    context: &mut FunctionLoweringContext<'_>,
    target: &MIRBlockTarget,
) -> LMIRBlockTarget {
    LMIRBlockTarget::with_args(
        block_id(target.block),
        target
            .args
            .iter()
            .map(|value| lower_value(context, value))
            .collect(),
    )
}

pub(super) fn unreachable_target(context: &mut FunctionLoweringContext<'_>) -> LMIRBlockTarget {
    let id = CXIdent::new(format!("unreachable_{}", context.blocks_len()));
    context.push_block(LMIRBasicBlock {
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
