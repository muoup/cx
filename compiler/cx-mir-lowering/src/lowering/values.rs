use crate::lowering::memory;
use cx_lmir::types::{LMIRIntegerType, LMIRType, LMIRTypeKind};
use cx_lmir::{LMIRCoercionType, LMIRInstructionKind, LMIRIntBinOp, LMIRIntUnOp, LMIRValue};
use cx_mir::ty::interface::MTRegistry;
use cx_mir::ty::layout::calculate_type_layout;
use cx_mir::{
    MIRBitfieldAccess, MIRConstant, MIRField, MIRGlobalRef, MIRIntType, MIRRegister, MIRTarget,
    MIRTypeID, MIRTypeKind, MIRValue,
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
        if !matches!(
            context.types().definition(expected).unwrap().kind(),
            MIRTypeKind::MemoryReference { .. }
        ) {
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
    let MIRTypeKind::MemoryReference { inner, bitfield } =
        context.types().definition(ty).unwrap().kind()
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
    if let Some(bitfield) = context
        .bitfields
        .get(&register)
        .cloned()
        .or_else(|| bitfield.clone())
    {
        return Some(read_bitfield(context, context.reg(register), inner, &bitfield));
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
            _type: LMIRType::with_implicit_abi(
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
                    _type: context.ty(*ty),
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
                            _type: LMIRType::with_implicit_abi(
                                context.types().architecture(),
                                LMIRTypeKind::Integer(LMIRIntegerType::I8),
                            ),
                        },
                    );
                }
                let (offset, field_ty) = aggregate_member(context, *ty, *index);
                let destination = memory::offset(context, address.clone(), offset as i64);
                let source = lower_constant(context, field);
                memory::store(context, destination, source, field_ty);
            }
            address
        }
        MIRConstant::Undefined => panic!("undefined MIR value reached LMIR lowering"),
    }
}

pub(super) fn aggregate_member(
    context: &FunctionContext<'_, '_>,
    ty: MIRTypeID,
    index: usize,
) -> (usize, MIRTypeID) {
    match context.types().definition(ty).unwrap().kind() {
        MIRTypeKind::Array { inner, .. } => (
            index * calculate_type_layout(context.types(), *inner).size(),
            *inner,
        ),
        _ => {
            let (offset, ty, _) = field_location(context, ty, index);
            (offset, ty)
        }
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
        MIRTarget::Indirect(id) => {
            let pointer_type = context.body.register(id).unwrap().ty;
            let bitfield = context.bitfields.get(&id).cloned().or_else(|| {
                match context.types().definition(pointer_type).unwrap().kind() {
                    MIRTypeKind::MemoryReference { bitfield, .. } => bitfield.clone(),
                    _ => None,
                }
            });
            if let Some(bitfield) = bitfield {
                write_bitfield(context, context.reg(id), value, ty, &bitfield);
            } else {
                memory::store(context, context.reg(id), value, ty);
            }
        }
    }
}

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

pub(super) fn write_bitfield(
    context: &mut FunctionContext<'_, '_>,
    address: LMIRValue,
    value: LMIRValue,
    storage_ty: MIRTypeID,
    bitfield: &MIRBitfieldAccess,
) {
    if bitfield.bit_width == 0 {
        return;
    }
    let storage_type = context.ty(storage_ty);
    let integer_type = integer_type(context, storage_ty);
    let field_mask = bit_mask(context, storage_ty, bitfield.bit_width);
    let value = integer_binop(
        context,
        LMIRIntBinOp::BAND,
        value,
        field_mask.clone(),
        storage_type.clone(),
    );
    let shifted_value = if bitfield.bit_offset == 0 {
        value
    } else {
        let shift_amount = context.integer(bitfield.bit_offset as i128, integer_type);
        integer_binop(
            context,
            LMIRIntBinOp::SHL,
            value,
            shift_amount,
            storage_type.clone(),
        )
    };
    let field_mask = if bitfield.bit_offset == 0 {
        field_mask
    } else {
        let shift_amount = context.integer(bitfield.bit_offset as i128, integer_type);
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

pub(super) fn field_location(
    context: &FunctionContext<'_, '_>,
    ty: MIRTypeID,
    index: usize,
) -> (usize, MIRTypeID, Option<(usize, usize)>) {
    let kind = context
        .types()
        .definition(ty)
        .expect("unknown aggregate")
        .kind();
    let (fields, is_union) = match kind {
        MIRTypeKind::Structured { fields } => (fields, false),
        MIRTypeKind::Union { variants } | MIRTypeKind::TaggedUnion { variants } => (variants, true),
        _ => panic!("field access on non-aggregate"),
    };
    let mut offset = 0;
    let mut bits: Option<(MIRTypeID, usize)> = None;
    for (position, field) in fields.iter().enumerate() {
        let field_ty = field.ty();
        let layout = calculate_type_layout(context.types(), field_ty);
        let alignment = layout.alignment();
        if is_union {
            if position == index {
                return (0, field_ty, bitfield(field, 0));
            }
            continue;
        }
        match field {
            MIRField::Standard { .. } => {
                bits = None;
                offset = align(offset, alignment);
                if position == index {
                    return (offset, field_ty, None);
                }
                offset += layout.size();
            }
            MIRField::Bitfield { width, .. } => {
                if *width == 0 {
                    bits = None;
                    offset = align(offset, alignment);
                    if position == index {
                        return (offset, field_ty, Some((0, 0)));
                    }
                    continue;
                }
                let bit_offset = match bits {
                    Some((storage, used))
                        if storage == field_ty && used + width <= layout.size() * 8 =>
                    {
                        bits = Some((storage, used + width));
                        used
                    }
                    _ => {
                        offset = align(offset, alignment);
                        let start = offset;
                        offset += layout.size();
                        bits = Some((field_ty, *width));
                        if position == index {
                            return (start, field_ty, Some((0, *width)));
                        }
                        continue;
                    }
                };
                if position == index {
                    return (offset - layout.size(), field_ty, Some((bit_offset, *width)));
                }
            }
        }
    }
    panic!("aggregate field index out of bounds")
}

fn bitfield(field: &MIRField, offset: usize) -> Option<(usize, usize)> {
    match field {
        MIRField::Bitfield { width, .. } => Some((offset, *width)),
        _ => None,
    }
}

pub(super) fn bitfield_access(
    context: &FunctionContext<'_, '_>,
    ty: MIRTypeID,
    location: Option<(usize, usize)>,
) -> Option<MIRBitfieldAccess> {
    let (bit_offset, bit_width) = location?;
    let MIRTypeKind::Integer { signed, .. } = context.types().definition(ty).unwrap().kind() else {
        panic!("bitfield storage type is not an integer")
    };
    Some(MIRBitfieldAccess {
        bit_offset,
        bit_width,
        signed: *signed,
    })
}

fn align(value: usize, alignment: usize) -> usize {
    value.div_ceil(alignment) * alignment
}
