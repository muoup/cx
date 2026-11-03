use crate::lowering::memory;
use cx_lmir::types::{LMIRIntegerType, LMIRType, LMIRTypeKind};
use cx_lmir::{LMIRCoercionType, LMIRInstructionKind, LMIRIntBinOp, LMIRIntUnOp, LMIRValue};
use cx_mir::ty::interface::MTRegistry;
use cx_mir::ty::layout::{calculate_field_layout, calculate_type_layout};
use cx_mir::{
    MIRBitfieldAccess, MIRConstant, MIRFieldLayout, MIRGlobalRef, MIRRegister, MIRTarget,
    MIRTypeID, MIRTypeKind, MIRValue,
};

use crate::context::FunctionContext;

use super::typing::{convert_float_type, convert_integer_type};

pub(crate) fn read(context: &mut FunctionContext<'_, '_>, value: &MIRValue) -> LMIRValue {
    match value {
        MIRValue::Register(id) => context.reg(*id),
        MIRValue::PlaceRef { place, ty } => contents(context, context.places[place].clone(), *ty),
        MIRValue::GlobalRef(reference) => {
            let address = global_address(context, *reference);
            contents(context, address, reference.ty)
        }
        MIRValue::Indirect { register, ty } => contents(context, context.reg(*register), *ty),
        MIRValue::Constant(value) => lower_constant(context, value),
    }
}

fn contents(context: &mut FunctionContext<'_, '_>, address: LMIRValue, ty: MIRTypeID) -> LMIRValue {
    match context.types().definition(ty).unwrap().kind() {
        MIRTypeKind::Function { .. } => address,
        _ => memory::load(context, address, ty),
    }
}

pub(super) fn value_type(context: &FunctionContext<'_, '_>, value: &MIRValue) -> Option<MIRTypeID> {
    match value {
        MIRValue::Register(id) => Some(context.body.register(*id).ty()),
        MIRValue::PlaceRef { ty, .. } => Some(*ty),
        MIRValue::GlobalRef(reference) => Some(reference.ty),
        MIRValue::Indirect { ty, .. } => Some(*ty),
        MIRValue::Constant(_) => None,
    }
}

fn pointee(context: &FunctionContext<'_, '_>, register: MIRRegister) -> MIRTypeID {
    let pointer = context.body.register(register).ty();
    match context.types().definition(pointer).unwrap().kind() {
        MIRTypeKind::MemoryReference { inner, .. } | MIRTypeKind::PointerTo { inner } => *inner,
        _ => panic!("indirect operand is not a pointer"),
    }
}

pub(super) fn global_address(
    context: &mut FunctionContext<'_, '_>,
    reference: MIRGlobalRef,
) -> LMIRValue {
    let base = LMIRValue::Global(context.global.global_indices[&reference.global]);
    memory::offset(context, base, reference.offset)
}

pub(super) fn lower_constant(
    context: &mut FunctionContext<'_, '_>,
    value: &MIRConstant,
) -> LMIRValue {
    match value {
        MIRConstant::Unit => LMIRValue::NULL,
        MIRConstant::Integer { ty, value } => context.integer(*value, convert_integer_type(*ty)),
        MIRConstant::Float { ty, value } => LMIRValue::FloatImmediate {
            val: *value,
            ty: LMIRType::with_implicit_abi(
                context.types().architecture(),
                LMIRTypeKind::Float(convert_float_type(*ty)),
            ),
        },
        MIRConstant::Nullptr { ty } => {
            let from = LMIRIntegerType::I64;
            memory::temp(
                context,
                LMIRInstructionKind::Coercion {
                    value: context.integer(0, from),
                    coercion_type: LMIRCoercionType::IntToPtr {
                        from,
                        sextend: false,
                    },
                },
                context.ty(*ty),
            )
        }
        MIRConstant::Address { base, offset } => {
            use cx_mir::constant::MIRAddressable;
            let base = match base {
                MIRAddressable::Global(id) => LMIRValue::Global(context.global.global_indices[id]),
                MIRAddressable::Local(place) => context.places[place].clone(),
                MIRAddressable::Function(id) => {
                    assert_eq!(*offset, 0, "function addresses cannot have an offset");
                    let name = context
                        .global
                        .unit
                        .function(*id)
                        .unwrap()
                        .prototype()
                        .symbol_name
                        .to_string();
                    return memory::temp(
                        context,
                        LMIRInstructionKind::GetFunctionAddr { func: name },
                        context.pointer(),
                    );
                }
                MIRAddressable::Block { function, label } => {
                    let (function, block) = context.global.block_address(*function, label);
                    LMIRValue::BlockAddress { function, block }
                }
            };
            memory::offset(context, base, *offset)
        }
        MIRConstant::String { value, offset } => {
            let base = LMIRValue::Global(context.global.string(value));
            memory::offset(context, base, *offset)
        }
        MIRConstant::Function(id) => LMIRValue::FunctionRef(
            context
                .global
                .unit
                .function(*id)
                .expect("unknown MIR function")
                .prototype()
                .symbol_name
                .clone(),
        ),
        MIRConstant::BlockAddress { function, label } => {
            let (function, block) = context.global.block_address(*function, label);
            LMIRValue::BlockAddress { function, block }
        }
        MIRConstant::Aggregate { ty, fields } => {
            let address = memory::allocate(context, *ty);
            memory::void(
                context,
                LMIRInstructionKind::ZeroMemory {
                    memory: address.clone(),
                    ty: context.ty(*ty),
                },
            );
            let kind = context.types().definition(*ty).unwrap().kind().clone();
            for (index, field) in fields {
                if matches!(kind, MIRTypeKind::TaggedUnion { .. }) {
                    let tag = memory::offset(
                        context,
                        address.clone(),
                        tagged_union_tag_offset(context, *ty) as i64,
                    );
                    memory::void(
                        context,
                        LMIRInstructionKind::Store {
                            memory: tag,
                            value: context.integer(*index as i128, LMIRIntegerType::I8),
                            ty: LMIRType::with_implicit_abi(
                                context.types().architecture(),
                                LMIRTypeKind::Integer(LMIRIntegerType::I8),
                            ),
                        },
                    );
                }
                let location = member_location(context, *ty, *index);
                let source = lower_constant(context, field);
                store_member(context, address.clone(), location, source);
            }
            address
        }
        MIRConstant::Undefined => panic!("undefined MIR value reached LMIR lowering"),
    }
}

/// Where member `index` of an array or aggregate lives: its byte offset, the type stored there,
/// and for a bitfield its bit offset and width within that storage unit.
pub(super) fn member_location(
    context: &FunctionContext<'_, '_>,
    ty: MIRTypeID,
    index: usize,
) -> (usize, MIRTypeID, Option<(usize, usize)>) {
    if let MIRTypeKind::Array { inner, .. } = context.types().definition(ty).unwrap().kind() {
        let stride = calculate_type_layout(context.types(), *inner).size();
        return (index * stride, *inner, None);
    }
    match calculate_field_layout(context.types(), ty, index)
        .expect("aggregate field index out of bounds")
    {
        MIRFieldLayout::Standard { offset, ty } => (offset, ty, None),
        MIRFieldLayout::Bitfield {
            offset,
            bit_offset,
            bit_width,
            storage_type,
        } => (offset, storage_type, Some((bit_offset, bit_width))),
    }
}

/// Stores `value` into a member of the aggregate at `address`, as located by `member_location`.
pub(super) fn store_member(
    context: &mut FunctionContext<'_, '_>,
    address: LMIRValue,
    (offset, ty, bits): (usize, MIRTypeID, Option<(usize, usize)>),
    value: LMIRValue,
) {
    let destination = memory::offset(context, address, offset as i64);
    match bits {
        Some((bit_offset, bit_width)) => {
            write_bitfield(context, destination, value, ty, bit_offset, bit_width)
        }
        None => memory::store(context, destination, value, ty),
    }
}

pub(super) fn tagged_union_tag_offset(context: &FunctionContext<'_, '_>, ty: MIRTypeID) -> usize {
    let MIRTypeKind::TaggedUnion { variants } = context.types().definition(ty).unwrap().kind()
    else {
        panic!("tag access on non-tagged union")
    };
    let alignment = variants
        .iter()
        .map(|field| calculate_type_layout(context.types(), field.ty()).alignment())
        .max()
        .unwrap_or(1);
    let size = variants
        .iter()
        .map(|field| calculate_type_layout(context.types(), field.ty()).size())
        .max()
        .unwrap_or(0);
    size.div_ceil(alignment) * alignment
}

pub(super) fn target_type(context: &FunctionContext<'_, '_>, target: MIRTarget) -> MIRTypeID {
    match target {
        MIRTarget::Register(id) => context.body.register(id).ty(),
        MIRTarget::Place(id) => context.body.place(id).ty(),
        MIRTarget::Global(reference) => reference.ty,
        MIRTarget::Indirect(id) => pointee(context, id),
    }
}

pub(super) fn write_target(
    context: &mut FunctionContext<'_, '_>,
    target: MIRTarget,
    value: LMIRValue,
    ty: Option<MIRTypeID>,
) {
    let ty = ty.unwrap_or_else(|| target_type(context, target));
    match target {
        MIRTarget::Register(id) => {
            memory::assign(context, id, LMIRInstructionKind::Alias { value })
        }
        MIRTarget::Place(id) => memory::store(context, context.places[&id].clone(), value, ty),
        MIRTarget::Global(reference) => {
            let address = global_address(context, reference);
            memory::store(context, address, value, ty);
        }
        MIRTarget::Indirect(id) => memory::store(context, context.reg(id), value, ty),
    }
}

/// The address of an addressable store target.
pub(super) fn target_address(
    context: &mut FunctionContext<'_, '_>,
    target: MIRTarget,
) -> LMIRValue {
    match target {
        MIRTarget::Place(id) => context.places[&id].clone(),
        MIRTarget::Global(reference) => global_address(context, reference),
        MIRTarget::Indirect(id) => context.reg(id),
        MIRTarget::Register(_) => unreachable!("a register store target has no address"),
    }
}

/// Extracts a bitfield from the storage unit at `address`, sign-extending it when `bitfield` is
/// signed.
pub(super) fn read_bitfield(
    context: &mut FunctionContext<'_, '_>,
    address: LMIRValue,
    storage_ty: MIRTypeID,
    bitfield: &MIRBitfieldAccess,
) -> LMIRValue {
    let (word, word_type) = bitfield_word(context, storage_ty);
    if bitfield.bit_width == 0 {
        return context.integer(0, integer_type(context, storage_ty));
    }
    let word_bits = word.bytes() as usize * 8;
    let storage = memory::temp(
        context,
        LMIRInstructionKind::Load {
            memory: address,
            ty: word_type.clone(),
        },
        word_type.clone(),
    );
    let value = if bitfield.bit_offset == 0 {
        storage
    } else {
        let shift_amount = context.integer(bitfield.bit_offset as i128, word);
        integer_binop(
            context,
            LMIRIntBinOp::LSHR,
            storage,
            shift_amount,
            word_type.clone(),
        )
    };
    let value = if bitfield.bit_width < word_bits {
        let mask = bit_mask(context, word, bitfield.bit_width);
        integer_binop(context, LMIRIntBinOp::BAND, value, mask, word_type.clone())
    } else {
        value
    };
    let value = if bitfield.signed && bitfield.bit_width < word_bits {
        let shift_amount = context.integer((word_bits - bitfield.bit_width) as i128, word);
        let value = integer_binop(
            context,
            LMIRIntBinOp::SHL,
            value,
            shift_amount.clone(),
            word_type.clone(),
        );
        integer_binop(context, LMIRIntBinOp::ASHR, value, shift_amount, word_type)
    } else {
        value
    };
    if word == integer_type(context, storage_ty) {
        return value;
    }
    let storage_type = context.ty(storage_ty);
    memory::temp(
        context,
        LMIRInstructionKind::Coercion {
            value,
            coercion_type: LMIRCoercionType::Trunc,
        },
        storage_type,
    )
}

/// Inserts `value` into the `bit_width` bits at `bit_offset` of the storage unit at `address`,
/// preserving the unit's other bits.
pub(super) fn write_bitfield(
    context: &mut FunctionContext<'_, '_>,
    address: LMIRValue,
    value: LMIRValue,
    storage_ty: MIRTypeID,
    bit_offset: usize,
    bit_width: usize,
) {
    if bit_width == 0 {
        return;
    }
    let (word, word_type) = bitfield_word(context, storage_ty);
    let value = if word == integer_type(context, storage_ty) {
        value
    } else {
        memory::temp(
            context,
            LMIRInstructionKind::Coercion {
                value,
                coercion_type: LMIRCoercionType::ZExtend,
            },
            word_type.clone(),
        )
    };
    let field_mask = bit_mask(context, word, bit_width);
    let value = integer_binop(
        context,
        LMIRIntBinOp::BAND,
        value,
        field_mask.clone(),
        word_type.clone(),
    );
    let (shifted_value, field_mask) = if bit_offset == 0 {
        (value, field_mask)
    } else {
        let shift_amount = context.integer(bit_offset as i128, word);
        (
            integer_binop(
                context,
                LMIRIntBinOp::SHL,
                value,
                shift_amount.clone(),
                word_type.clone(),
            ),
            integer_binop(
                context,
                LMIRIntBinOp::SHL,
                field_mask,
                shift_amount,
                word_type.clone(),
            ),
        )
    };
    let field_mask = memory::temp(
        context,
        LMIRInstructionKind::IntegerUnOp {
            op: LMIRIntUnOp::BNOT,
            value: field_mask,
        },
        word_type.clone(),
    );
    let storage = memory::temp(
        context,
        LMIRInstructionKind::Load {
            memory: address.clone(),
            ty: word_type.clone(),
        },
        word_type.clone(),
    );
    let preserved = integer_binop(
        context,
        LMIRIntBinOp::BAND,
        storage,
        field_mask,
        word_type.clone(),
    );
    let value = integer_binop(
        context,
        LMIRIntBinOp::BOR,
        preserved,
        shifted_value,
        word_type.clone(),
    );
    memory::void(
        context,
        LMIRInstructionKind::Store {
            memory: address,
            value,
            ty: word_type,
        },
    );
}

/// The integer word a bitfield of `storage_ty` is accessed through; a `_Bool` bitfield's word is
/// its whole byte, since other bitfields may share it.
fn bitfield_word(
    context: &FunctionContext<'_, '_>,
    storage_ty: MIRTypeID,
) -> (LMIRIntegerType, LMIRType) {
    let word = match integer_type(context, storage_ty) {
        LMIRIntegerType::I1 => LMIRIntegerType::I8,
        word => word,
    };
    (
        word,
        LMIRType::with_implicit_abi(context.types().architecture(), LMIRTypeKind::Integer(word)),
    )
}

fn bit_mask(
    context: &mut FunctionContext<'_, '_>,
    word: LMIRIntegerType,
    width: usize,
) -> LMIRValue {
    let bits = word.bytes() as usize * 8;
    let all_ones = context.integer(-1, word);
    if width >= bits {
        return all_ones;
    }
    let shift_amount = context.integer((bits - width) as i128, word);
    let word_type =
        LMIRType::with_implicit_abi(context.types().architecture(), LMIRTypeKind::Integer(word));
    integer_binop(
        context,
        LMIRIntBinOp::LSHR,
        all_ones,
        shift_amount,
        word_type,
    )
}

fn integer_binop(
    context: &mut FunctionContext<'_, '_>,
    op: LMIRIntBinOp,
    left: LMIRValue,
    right: LMIRValue,
    ty: LMIRType,
) -> LMIRValue {
    memory::temp(
        context,
        LMIRInstructionKind::IntegerBinOp { op, left, right },
        ty,
    )
}

fn integer_type(context: &FunctionContext<'_, '_>, ty: MIRTypeID) -> LMIRIntegerType {
    let MIRTypeKind::Integer { ty, .. } = context.types().definition(ty).unwrap().kind() else {
        panic!("bitfield storage type is not an integer")
    };
    convert_integer_type(*ty)
}
