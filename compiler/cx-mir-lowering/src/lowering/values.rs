use crate::lowering::memory;
use cx_lmir::types::{LMIRIntegerType, LMIRType, LMIRTypeKind};
use cx_lmir::{LMIRCoercionType, LMIRInstructionKind, LMIRIntBinOp, LMIRIntUnOp, LMIRValue};
use cx_mir::ty::interface::MTRegistry;
use cx_mir::ty::layout::{calculate_field_layout, calculate_type_layout};
use cx_mir::{
    MIRBitfieldAccess, MIRConstant, MIRFieldLayout, MIRGlobalRef, MIRIntType, MIRRegister,
    MIRTarget, MIRTypeID, MIRTypeKind, MIRValue,
};

use crate::context::FunctionContext;

use super::typing::{convert_float_type, convert_integer_type};

pub(crate) fn lower_value(context: &mut FunctionContext<'_, '_>, value: &MIRValue) -> LMIRValue {
    match value {
        MIRValue::Register(id) => context.reg(*id),
        MIRValue::PlaceRef(id) => context.places[id].clone(),
        MIRValue::Constant(MIRConstant::GlobalRef(reference)) => {
            global_address(context, *reference)
        }
        MIRValue::Constant(value) => lower_constant(context, value),
    }
}

pub(crate) fn lower_rvalue(
    context: &mut FunctionContext<'_, '_>,
    value: &MIRValue,
    expected: MIRTypeID,
) -> LMIRValue {
    if context.ty(expected).is_void() {
        return LMIRValue::NULL;
    }
    if let MIRValue::Register(register) = value {
        if loads_through(context, *register, expected) {
            if let Some(value) = read_through_reference(context, *register) {
                return value;
            }
        }
    }
    let address = match value {
        MIRValue::PlaceRef(place) => Some((
            context.places[place].clone(),
            context.body.place(*place).unwrap().ty,
        )),
        MIRValue::Constant(MIRConstant::GlobalRef(reference)) => {
            Some((global_address(context, *reference), reference.ty))
        }
        _ => None,
    };
    if let Some((address, source_ty)) = address {
        if matches!(
            context.types().definition(expected).unwrap().kind(),
            MIRTypeKind::MemoryReference { .. }
        ) {
            return if matches!(
                context.types().definition(source_ty).unwrap().kind(),
                MIRTypeKind::MemoryReference { .. }
            ) {
                memory::load(context, address, source_ty)
            } else {
                address
            };
        }
        if context.ty(source_ty).is_memory_resident() {
            return address;
        }
        return memory::load(context, address, source_ty);
    }
    lower_value(context, value)
}

/// Whether reading `register` as a value of type `expected` loads through it: always when
/// `expected` is not a reference, and for a reference only when `register` refers to one.
fn loads_through(
    context: &FunctionContext<'_, '_>,
    register: MIRRegister,
    expected: MIRTypeID,
) -> bool {
    let types = context.types();
    if !matches!(
        types.definition(expected).unwrap().kind(),
        MIRTypeKind::MemoryReference { .. }
    ) {
        return true;
    }
    let register_type = context.body.register(register).unwrap().ty;
    match types.definition(register_type).unwrap().kind() {
        MIRTypeKind::MemoryReference { inner } => {
            !types.same_type(register_type, expected) && types.same_type(*inner, expected)
        }
        _ => false,
    }
}

pub(super) fn lower_read(context: &mut FunctionContext<'_, '_>, value: &MIRValue) -> LMIRValue {
    if let MIRValue::Constant(MIRConstant::GlobalRef(reference)) = value {
        let address = global_address(context, *reference);
        return memory::load(context, address, reference.ty);
    }
    if let MIRValue::PlaceRef(place) = value {
        let ty = context.body.place(*place).unwrap().ty;
        return memory::load(context, context.places[place].clone(), ty);
    }
    if let MIRValue::Register(register) = value {
        if let Some(value) = read_through_reference(context, *register) {
            return value;
        }
    }
    lower_value(context, value)
}

fn read_through_reference(
    context: &mut FunctionContext<'_, '_>,
    register: MIRRegister,
) -> Option<LMIRValue> {
    let ty = context.body.register(register).unwrap().ty;
    let MIRTypeKind::MemoryReference { inner } = context.types().definition(ty).unwrap().kind()
    else {
        return None;
    };
    let inner = *inner;
    if matches!(
        context.types().definition(inner).unwrap().kind(),
        MIRTypeKind::Function { .. }
    ) {
        return Some(context.reg(register));
    }
    Some(memory::load(context, context.reg(register), inner))
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
        MIRConstant::GlobalRef(reference) => global_address(context, *reference),
        MIRConstant::String(text) => LMIRValue::Global(context.global.string(text)),
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
        MIRTarget::Register(id) => {
            context
                .body
                .register(id)
                .expect("unknown target register")
                .ty
        }
        MIRTarget::Place(id) => context.body.place(id).expect("unknown target place").ty,
        MIRTarget::Global(reference) => reference.ty,
        MIRTarget::Indirect(id) => {
            let pointer = context
                .body
                .register(id)
                .expect("unknown indirect target")
                .ty;
            match context.types().definition(pointer).unwrap().kind() {
                MIRTypeKind::MemoryReference { inner, .. } | MIRTypeKind::PointerTo { inner } => {
                    *inner
                }
                _ => panic!("indirect target is not a pointer"),
            }
        }
    }
}

pub(super) fn write_target(
    context: &mut FunctionContext<'_, '_>,
    target: MIRTarget,
    value: LMIRValue,
) {
    let ty = target_type(context, target);
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
    let storage_type = context.ty(storage_ty);
    let storage_bits = integer_bits(context, storage_ty);
    let storage = memory::load(context, address, storage_ty);
    if bitfield.bit_width == 0 {
        return context.integer(0, integer_type(context, storage_ty));
    }
    let value = if bitfield.bit_offset == 0 {
        storage
    } else {
        let shift_amount = context.integer(
            bitfield.bit_offset as i128,
            integer_type(context, storage_ty),
        );
        integer_binop(
            context,
            LMIRIntBinOp::LSHR,
            storage,
            shift_amount,
            storage_type.clone(),
        )
    };
    let value = if bitfield.bit_width < storage_bits {
        let mask = bit_mask(context, storage_ty, bitfield.bit_width);
        integer_binop(
            context,
            LMIRIntBinOp::BAND,
            value,
            mask,
            storage_type.clone(),
        )
    } else {
        value
    };
    if bitfield.signed && bitfield.bit_width < storage_bits {
        let shift = storage_bits - bitfield.bit_width;
        let shift_amount = context.integer(shift as i128, integer_type(context, storage_ty));
        let value = integer_binop(
            context,
            LMIRIntBinOp::SHL,
            value,
            shift_amount.clone(),
            storage_type.clone(),
        );
        integer_binop(
            context,
            LMIRIntBinOp::ASHR,
            value,
            shift_amount,
            storage_type,
        )
    } else {
        value
    }
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
    let storage_type = context.ty(storage_ty);
    let integer_type = integer_type(context, storage_ty);
    let field_mask = bit_mask(context, storage_ty, bit_width);
    let value = integer_binop(
        context,
        LMIRIntBinOp::BAND,
        value,
        field_mask.clone(),
        storage_type.clone(),
    );
    let shifted_value = if bit_offset == 0 {
        value
    } else {
        let shift_amount = context.integer(bit_offset as i128, integer_type);
        integer_binop(
            context,
            LMIRIntBinOp::SHL,
            value,
            shift_amount,
            storage_type.clone(),
        )
    };
    let field_mask = if bit_offset == 0 {
        field_mask
    } else {
        let shift_amount = context.integer(bit_offset as i128, integer_type);
        integer_binop(
            context,
            LMIRIntBinOp::SHL,
            field_mask,
            shift_amount,
            storage_type.clone(),
        )
    };
    let field_mask = memory::temp(
        context,
        LMIRInstructionKind::IntegerUnOp {
            op: LMIRIntUnOp::BNOT,
            value: field_mask,
        },
        storage_type.clone(),
    );
    let storage = memory::load(context, address.clone(), storage_ty);
    let preserved = integer_binop(
        context,
        LMIRIntBinOp::BAND,
        storage,
        field_mask,
        storage_type.clone(),
    );
    let value = integer_binop(
        context,
        LMIRIntBinOp::BOR,
        preserved,
        shifted_value,
        storage_type,
    );
    memory::store(context, address, value, storage_ty);
}

fn bit_mask(
    context: &mut FunctionContext<'_, '_>,
    storage_ty: MIRTypeID,
    width: usize,
) -> LMIRValue {
    let bits = integer_bits(context, storage_ty);
    if width >= bits {
        return context.integer(-1, integer_type(context, storage_ty));
    }
    let all_ones = context.integer(-1, integer_type(context, storage_ty));
    let shift_amount = context.integer((bits - width) as i128, integer_type(context, storage_ty));
    let lowered_type = context.ty(storage_ty);
    integer_binop(
        context,
        LMIRIntBinOp::LSHR,
        all_ones,
        shift_amount,
        lowered_type,
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

fn integer_bits(context: &FunctionContext<'_, '_>, ty: MIRTypeID) -> usize {
    match context.types().definition(ty).unwrap().kind() {
        MIRTypeKind::Integer { ty, .. } => match ty {
            MIRIntType::I1 => 1,
            MIRIntType::I8 => 8,
            MIRIntType::I16 => 16,
            MIRIntType::I32 => 32,
            MIRIntType::I64 => 64,
            MIRIntType::I128 => 128,
        },
        _ => panic!("bitfield storage type is not an integer"),
    }
}

fn integer_type(context: &FunctionContext<'_, '_>, ty: MIRTypeID) -> LMIRIntegerType {
    let MIRTypeKind::Integer { ty, .. } = context.types().definition(ty).unwrap().kind() else {
        panic!("bitfield storage type is not an integer")
    };
    convert_integer_type(*ty)
}
