use cx_log::{CXResult, catalogue::mir};
use cx_mir::{
    MIRComptimeBody, MIRConstant, MIRField, MIRGlobalRef, MIRGlobalState, MIRTarget, MIRTypeID,
    MIRTypeKind,
    ty::{
        interface::MTRegistry,
        layout::{calculate_field_layout, calculate_type_layout},
    },
};
use cx_tokens::TokenRange;

use crate::{
    ComptimeContext,
    execution::engine::{Engine, ExecutionFrame},
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
        MIRTypeKind::Structured { fields } | MIRTypeKind::Union { variants: fields } => {
            let is_union = matches!(kind, MIRTypeKind::Union { .. });
            for (index, field) in fields.iter().enumerate() {
                let field_offset = field_offset(registry, fields, index, is_union)?;
                let field_ty = field.ty();
                let size = calculate_field_layout(registry, field).size();
                if offset < field_offset || offset >= field_offset.checked_add(size)? {
                    continue;
                }
                let remainder = offset - field_offset;
                let child =
                    aggregate_field(value, index).unwrap_or_else(|| zero_value(registry, field_ty));
                if matches!(field, MIRField::Bitfield { .. }) {
                    return None;
                }
                return read_offset(registry, &child, field_ty, remainder, target);
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
    let kind = registry.definition(ty)?.kind();
    let (fields, is_union) = match kind {
        MIRTypeKind::Structured { fields } => (fields, false),
        MIRTypeKind::Union { variants } | MIRTypeKind::TaggedUnion { variants } => (variants, true),
        _ => return None,
    };
    let field = fields.get(index)?;
    Some((field_offset(registry, fields, index, is_union)?, field.ty()))
}

fn field_offset<R: MTRegistry>(
    registry: &R,
    fields: &[MIRField],
    index: usize,
    is_union: bool,
) -> Option<usize> {
    if is_union {
        return Some(0);
    }
    let mut offset = 0;
    let mut bitfield: Option<(MIRTypeID, usize, usize)> = None;
    for (position, field) in fields.iter().enumerate() {
        let field_ty = field.ty();
        let layout = calculate_type_layout(registry, field_ty);
        match field {
            MIRField::Standard { .. } => {
                bitfield = None;
                offset = align(offset, layout.alignment());
                if position == index {
                    return Some(offset);
                }
                offset = offset.checked_add(layout.size())?;
            }
            MIRField::Bitfield { width, .. } => {
                if *width == 0 {
                    bitfield = None;
                    offset = align(offset, layout.alignment());
                    if position == index {
                        return Some(offset);
                    }
                } else if let Some((storage, start, used)) = bitfield
                    && storage == field_ty
                    && used + width <= layout.size() * 8
                {
                    if position == index {
                        return Some(start);
                    }
                    bitfield = Some((storage, start, used + width));
                } else {
                    offset = align(offset, layout.alignment());
                    let start = offset;
                    offset = offset.checked_add(layout.size())?;
                    if position == index {
                        return Some(start);
                    }
                    bitfield = Some((field_ty, start, *width));
                }
            }
        }
    }
    None
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

fn align(value: usize, alignment: usize) -> usize {
    value.div_ceil(alignment) * alignment
}
