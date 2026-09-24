use crate::lowering::memory;
use cx_lmir::types::{LMIRIntegerType, LMIRType, LMIRTypeKind};
use cx_lmir::{LMIRCoercionType, LMIRInstructionKind, LMIRValue};
use cx_mir::ty::interface::MTRegistry;
use cx_mir::ty::layout::calculate_type_layout;
use cx_mir::{MIRConstant, MIRField, MIRGlobalRef, MIRTarget, MIRTypeID, MIRTypeKind, MIRValue};

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
    if let MIRValue::Register(register) = value {
        let source_ty = context.body.register(*register).unwrap().ty;
        if !matches!(
            context.types().definition(expected).unwrap().kind(),
            MIRTypeKind::MemoryReference { .. }
        ) {
            if let MIRTypeKind::MemoryReference { inner, .. } =
                context.types().definition(source_ty).unwrap().kind()
            {
                let inner = *inner;
                return memory::load(context, context.reg(*register), inner);
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
            return address;
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
        let ty = context.body.register(*register).unwrap().ty;
        if let MIRTypeKind::MemoryReference { inner, .. } =
            context.types().definition(ty).unwrap().kind()
        {
            let inner = *inner;
            return memory::load(context, context.reg(*register), inner);
        }
    }
    lower_value(context, value)
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
        MIRTarget::Indirect(id) => memory::store(context, context.reg(id), value, ty),
    }
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

fn align(value: usize, alignment: usize) -> usize {
    value.div_ceil(alignment) * alignment
}
