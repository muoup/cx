use cx_log::{CXResult, catalogue::mir};
use cx_mir::{
    MIRBitfieldAccess, MIRComptimeBody, MIRIntType, MIRConstant, MIRFieldLayout, MIRGlobalRef, MIRGlobalState,
    MIRTarget, MIRTypeID, MIRTypeKind, MIRValue,
    ty::{
        interface::MTRegistry,
        layout::{calculate_field_layouts, calculate_type_layout},
    },
};
use cx_tokens::TokenRange;

use crate::{
    ComptimeContext,
    execution::{
        engine::{Engine, ExecutionFrame},
        scalar::{bits, int_const, integer, mask, signed},
    },
    log::comptime_error,
};

pub(crate) fn read_target<'c, 'thir, C: ComptimeContext<'thir>>(
    engine: &Engine<'c, 'thir, C>,
    frame: &ExecutionFrame,
    body: &MIRComptimeBody<'_>,
    target: MIRTarget,
    range: &TokenRange,
) -> CXResult<MIRConstant> {
    match target {
        MIRTarget::Place(place) => {
            engine.read(frame, body, &cx_mir::MIRValue::PlaceRef(place), range)
        }
        MIRTarget::Register(register) => {
            engine.read(frame, body, &cx_mir::MIRValue::Register(register), range)
        }
        MIRTarget::Global(reference) => read_global(engine.context(), reference, range),
        MIRTarget::Indirect(register) => {
            let pointer = engine.read(frame, body, &cx_mir::MIRValue::Register(register), range)?;
            let inline_reference = frame.is_inline_reference(register);
            match pointer {
                MIRConstant::GlobalRef(reference) => {
                    read_global(engine.context(), reference, range)
                }
                MIRConstant::Nullptr { .. } => comptime_error(
                    range.clone(),
                    (
                        &mir::COMPTIME_INVALID_OPERATION,
                        "dereference a null pointer".into(),
                    ),
                ),
                value if inline_reference => Ok(value),
                _ => comptime_error(
                    range.clone(),
                    (
                        &mir::COMPTIME_INVALID_OPERATION,
                        "dereference a non-global pointer".into(),
                    ),
                ),
            }
        }
    }
}

pub(crate) fn read_global<'thir, C: ComptimeContext<'thir>>(
    context: &C,
    reference: MIRGlobalRef,
    range: &TokenRange,
) -> CXResult<MIRConstant> {
    let Some(global) = context.global(reference.global) else {
        return comptime_error(range.clone(), (&mir::COMPTIME_UNAVAILABLE, "global".into()));
    };
    let root = match global.state() {
        MIRGlobalState::External => {
            return comptime_error(
                range.clone(),
                (&mir::COMPTIME_UNAVAILABLE, "external global".into()),
            );
        }
        MIRGlobalState::ZeroInitialized => zero_value(context.types(), global.ty()),
        MIRGlobalState::Initialized(value) => value.clone(),
    };
    let Some(offset) = usize::try_from(reference.offset).ok() else {
        return comptime_error(
            range.clone(),
            (
                &mir::COMPTIME_INVALID_OPERATION,
                "negative global offset".into(),
            ),
        );
    };
    let Some(value) = read_offset(context.types(), &root, global.ty(), offset, reference.ty) else {
        return comptime_error(
            range.clone(),
            (
                &mir::COMPTIME_INVALID_OPERATION,
                "invalid global projection".into(),
            ),
        );
    };
    Ok(value)
}

pub(crate) fn zero_value<R: MTRegistry>(registry: &R, ty: MIRTypeID) -> MIRConstant {
    match registry.definition(ty).map(|definition| definition.kind()) {
        Some(MIRTypeKind::Integer { ty, .. }) => MIRConstant::Integer { value: 0, ty: *ty },
        Some(MIRTypeKind::Float { ty }) => MIRConstant::Float {
            value: 0.0.into(),
            ty: *ty,
        },
        Some(MIRTypeKind::PointerTo { .. } | MIRTypeKind::MemoryReference { .. }) => {
            MIRConstant::Nullptr { ty }
        }
        Some(
            MIRTypeKind::Array { .. }
            | MIRTypeKind::Structured { .. }
            | MIRTypeKind::Union { .. }
            | MIRTypeKind::TaggedUnion { .. },
        ) => MIRConstant::Aggregate {
            ty,
            fields: Vec::new(),
        },
        _ => MIRConstant::Unit,
    }
}

fn read_offset<R: MTRegistry>(
    registry: &R,
    value: &MIRConstant,
    ty: MIRTypeID,
    offset: usize,
    target: MIRTypeID,
) -> Option<MIRConstant> {
    if offset == 0 && registry.same_type(ty, target) {
        return Some(value.clone());
    }
    let kind = registry.definition(ty)?.kind();
    match kind {
        MIRTypeKind::Array { length, inner } => {
            let stride = calculate_type_layout(registry, *inner).size();
            if stride == 0 || offset >= stride.checked_mul(*length)? {
                return None;
            }
            let index = offset / stride;
            let remainder = offset % stride;
            let element =
                aggregate_field(value, index).unwrap_or_else(|| zero_value(registry, *inner));
            read_offset(registry, &element, *inner, remainder, target)
        }
        MIRTypeKind::Structured { .. } | MIRTypeKind::Union { .. } => {
            let layouts = calculate_field_layouts(registry, ty)?;
            for (index, layout) in layouts.iter().enumerate() {
                let size = match layout {
                    MIRFieldLayout::Bitfield { bit_width: 0, .. } => continue,
                    layout => calculate_type_layout(registry, layout.ty()).size(),
                };
                let field_offset = layout.offset();
                if offset < field_offset || offset >= field_offset.checked_add(size)? {
                    continue;
                }
                if matches!(layout, MIRFieldLayout::Bitfield { .. }) {
                    if offset == field_offset && registry.same_type(layout.ty(), target) {
                        return Some(bitfield_unit(registry, value, ty, field_offset, target));
                    }
                    continue;
                }
                let child = aggregate_field(value, index)
                    .unwrap_or_else(|| zero_value(registry, layout.ty()));
                return read_offset(registry, &child, layout.ty(), offset - field_offset, target);
            }
            None
        }
        _ => None,
    }
}

pub(crate) fn field_byte_offset<R: MTRegistry>(
    registry: &R,
    ty: MIRTypeID,
    index: usize,
) -> Option<(usize, MIRTypeID)> {
    let layout = calculate_field_layouts(registry, ty)?.get(index).copied()?;
    Some((layout.offset(), layout.ty()))
}

/// Assembles the integer word of type `storage_ty` at `unit_offset` of an inline aggregate from
/// the bits of every bitfield overlapping it.
pub(crate) fn bitfield_unit<R: MTRegistry>(
    registry: &R,
    aggregate: &MIRConstant,
    aggregate_ty: MIRTypeID,
    unit_offset: usize,
    storage_ty: MIRTypeID,
) -> MIRConstant {
    let storage = match registry.definition(storage_ty).map(|ty| ty.kind()) {
        Some(MIRTypeKind::Integer { ty: MIRIntType::I1 }) => MIRIntType::I8,
        Some(MIRTypeKind::Integer { ty }) => *ty,
        _ => panic!("bitfield storage type is not an integer"),
    };
    let unit_start = unit_offset * 8;
    let unit_end = unit_start + bits(storage) as usize;
    let mut unit = 0u128;
    for (index, layout) in calculate_field_layouts(registry, aggregate_ty)
        .unwrap_or_default()
        .into_iter()
        .enumerate()
    {
        let MIRFieldLayout::Bitfield {
            offset,
            bit_offset,
            bit_width,
            ..
        } = layout
        else {
            continue;
        };
        let start = offset * 8 + bit_offset;
        if bit_width == 0 || start + bit_width <= unit_start || start >= unit_end {
            continue;
        }
        if let Some(MIRConstant::Integer { value, .. }) = aggregate_field(aggregate, index) {
            let value = mask(value as u128, bit_width as u32);
            unit |= if start >= unit_start {
                value << (start - unit_start)
            } else {
                value >> (unit_start - start)
            };
        }
    }
    int_const(unit, storage)
}

/// Extracts a bitfield from the value of its storage unit as a `result` integer.
pub(crate) fn extract_bitfield(
    unit: &MIRConstant,
    access: &MIRBitfieldAccess,
    result: MIRIntType,
    range: &TokenRange,
) -> CXResult<MIRConstant> {
    let (unit, ty) = integer(unit, range)?;
    let width = access.bit_width as u32;
    if width == 0 {
        return Ok(int_const(0, result));
    }
    let value = mask(unit >> access.bit_offset, width);
    let value = if access.signed && width < bits(ty) {
        signed(value, width) as u128
    } else {
        value
    };
    Ok(int_const(value, result))
}

/// Reads `value` as a value of type `ty`, loading through a reference register when `ty` is not
/// itself a reference.
pub(crate) fn read_rvalue<'c, 'thir, C: ComptimeContext<'thir>>(
    engine: &Engine<'c, 'thir, C>,
    frame: &ExecutionFrame,
    body: &MIRComptimeBody<'_>,
    value: &MIRValue,
    ty: MIRTypeID,
    range: &TokenRange,
) -> CXResult<MIRConstant> {
    let types = engine.context().types();
    let is_reference = |ty: MIRTypeID| {
        types
            .definition(ty)
            .is_some_and(|ty| types.is_reference_type(ty))
    };
    // Mirrors runtime lowering: a register is loaded through when it refers to a value of `ty`
    let loads_through =
        |register_ty: MIRTypeID| match types.definition(register_ty).map(|ty| ty.kind()) {
            Some(MIRTypeKind::MemoryReference { inner }) => {
                !is_reference(ty)
                    || (!types.same_type(register_ty, ty) && types.same_type(*inner, ty))
            }
            _ => false,
        };
    if let MIRValue::Register(register) = value
        && body
            .register(*register)
            .is_some_and(|register| loads_through(register.ty))
    {
        return read_target(engine, frame, body, MIRTarget::Indirect(*register), range);
    }
    if !is_reference(ty) {
        match value {
            MIRValue::Constant(MIRConstant::GlobalRef(reference))
                if !is_reference(reference.ty) =>
            {
                return read_global(engine.context(), *reference, range);
            }
            _ => {}
        }
    }
    engine.read(frame, body, value, range)
}

pub(super) fn aggregate_field(value: &MIRConstant, index: usize) -> Option<MIRConstant> {
    let MIRConstant::Aggregate { fields, .. } = value else {
        return None;
    };
    fields
        .iter()
        .find(|(field, _)| *field == index)
        .map(|(_, value)| value.clone())
}
