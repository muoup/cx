use crate::ty::interface::MTRegistry;

use super::{MIRField, MIRTypeID, MIRTypeKind};

#[derive(Debug, Clone, Copy, PartialEq, Eq, Hash)]
pub struct MIRTypeLayout {
    pub size: usize,
    pub alignment: usize,
}

#[derive(Debug, Clone, Copy, PartialEq, Eq)]
pub enum MIRFieldLayout {
    Standard {
        offset: usize,
        ty: MIRTypeID,
    },
    Bitfield {
        offset: usize,
        bit_offset: usize,
        bit_width: usize,
        storage_type: MIRTypeID,
    },
}

#[derive(Debug, Clone, PartialEq, Eq)]
pub enum MIRLayoutError {
    InvalidType(MIRTypeID),
    DuplicateType(MIRTypeID),
    RecursiveType(MIRTypeID),
    InvalidBitfieldWidth { width: usize, storage_bits: usize },
    InvalidAlignment(usize),
    InvalidField { ty: MIRTypeID, field: usize },
    SizeOverflow,
}

pub fn layout_of<T: MTRegistry>(
    registry: &T,
    id: MIRTypeID,
) -> Result<MIRTypeLayout, MIRLayoutError> {
    layout_inner(registry, id)
}

fn layout_inner<T: MTRegistry>(
    registry: &T,
    id: MIRTypeID,
) -> Result<MIRTypeLayout, MIRLayoutError> {
    let definition = registry.resolve_type_id(id)?;

    let mut layout = match &definition.kind {
        MIRTypeKind::Void => MIRTypeLayout {
            size: 0,
            alignment: 1,
        },
        MIRTypeKind::Integer { ty, .. } => scalar_layout(ty.bytes()),
        MIRTypeKind::Float { ty } => scalar_layout(ty.bytes()),
        MIRTypeKind::PointerTo { .. }
        | MIRTypeKind::MemoryReference { .. }
        | MIRTypeKind::Function { .. } => MIRTypeLayout {
            size: registry.architecture().pointer_size(),
            alignment: registry.architecture().pointer_alignment(),
        },
        MIRTypeKind::Array { length, inner } => {
            let inner = layout_inner(registry, *inner)?;

            MIRTypeLayout {
                size: inner
                    .size
                    .checked_mul(*length)
                    .ok_or(MIRLayoutError::SizeOverflow)?,
                alignment: inner.alignment,
            }
        }
        MIRTypeKind::Structured { fields } => struct_layout(registry, fields)?,
        MIRTypeKind::Union { variants } => union_layout(registry, variants)?,
        MIRTypeKind::TaggedUnion { variants } => tagged_union_layout(registry, variants)?,
        MIRTypeKind::Opaque { size, alignment } => MIRTypeLayout {
            size: *size,
            alignment: *alignment,
        },
        MIRTypeKind::Str => MIRTypeLayout {
            size: 1,
            alignment: 1,
        },
        MIRTypeKind::Undefined => return Err(MIRLayoutError::InvalidType(id)),
    };

    layout.alignment = layout
        .alignment
        .clamp(1, registry.architecture().pointer_size());
    layout.size = align_to(layout.size, layout.alignment)?;
    Ok(layout)
}

pub fn field_layout<T: MTRegistry>(
    registry: &T,
    aggregate: MIRTypeID,
    field_index: usize,
) -> Result<MIRFieldLayout, MIRLayoutError> {
    let aggregate_def = registry
        .definition(aggregate)
        .ok_or_else(|| MIRLayoutError::InvalidType(aggregate))?;

    let aggregate_type = match aggregate_def.kind {
        MIRTypeKind::MemoryReference { inner, .. } => registry
            .definition(inner)
            .ok_or_else(|| MIRLayoutError::InvalidType(inner))?,
        _ => aggregate_def,
    };

    let (fields, is_union) = match &aggregate_type.kind {
        MIRTypeKind::Structured { fields } => (fields, false),
        MIRTypeKind::Union { variants } => (variants, true),

        _ => {
            return Err(MIRLayoutError::InvalidField {
                ty: aggregate,
                field: field_index,
            });
        }
    };

    aggregate_field_layout(registry, aggregate, &fields, is_union, field_index)
}

pub fn tagged_union_tag_offset<T: MTRegistry>(registry: &T, sum: MIRTypeID) -> Result<usize, MIRLayoutError> {
    let sum_def = registry
        .definition(sum)
        .ok_or_else(|| MIRLayoutError::InvalidType(sum))?;

    let MIRTypeKind::TaggedUnion { variants } = &sum_def.kind else {
        return Err(MIRLayoutError::InvalidType(sum));
    };

    let data = union_layout(registry, &variants)?;
    align_to(data.size, 1)
}

fn struct_layout<T: MTRegistry>(
    registry: &T,
    fields: &[MIRField],
) -> Result<MIRTypeLayout, MIRLayoutError> {
    let mut size = 0;
    let mut alignment = 1;
    let mut active = None;

    for field in fields {
        match field {
            MIRField::Standard { type_id, .. } => {
                flush_bitfield(&mut size, &mut active)?;
                let field_layout = layout_inner(registry, *type_id)?;
                size = align_to(size, field_layout.alignment)?;
                size = size
                    .checked_add(field_layout.size)
                    .ok_or(MIRLayoutError::SizeOverflow)?;
                alignment = alignment.max(field_layout.alignment);
            }
            MIRField::Bitfield {
                integer_type_id,
                width,
                ..
            } => {
                let storage = layout_inner(registry, *integer_type_id)?;
                validate_bitfield(*width, storage)?;
                alignment = alignment.max(storage.alignment);
                if *width == 0 {
                    flush_bitfield(&mut size, &mut active)?;
                    size = align_to(size, storage.alignment)?;
                    continue;
                }

                let can_share = active.as_ref().is_some_and(|active: &ActiveBitfield| {
                    active.type_id == *integer_type_id
                        && active.used_bits + *width <= active.layout.size * 8
                });
                if !can_share {
                    flush_bitfield(&mut size, &mut active)?;
                    size = align_to(size, storage.alignment)?;
                    active = Some(ActiveBitfield {
                        type_id: *integer_type_id,
                        layout: storage,
                        used_bits: 0,
                    });
                }
                active
                    .as_mut()
                    .expect("active bitfield was initialized")
                    .used_bits += *width;
            }
        }
    }
    flush_bitfield(&mut size, &mut active)?;
    Ok(MIRTypeLayout {
        size: align_to(size, alignment)?,
        alignment,
    })
}

fn union_layout<T: MTRegistry>(
    registry: &T,
    fields: &[MIRField],
) -> Result<MIRTypeLayout, MIRLayoutError> {
    let mut size = 0;
    let mut alignment = 1;
    for field in fields {
        let layout = field_storage_layout(registry, field)?;
        size = size.max(layout.size);
        alignment = alignment.max(layout.alignment);
    }
    Ok(MIRTypeLayout {
        size: align_to(size, alignment)?,
        alignment,
    })
}

fn tagged_union_layout<T: MTRegistry>(
    registry: &T, 
    variants: &[MIRField],
) -> Result<MIRTypeLayout, MIRLayoutError> {
    let data = union_layout(registry, variants)?;
    let tag = scalar_layout(1);
    let alignment = data.alignment.max(tag.alignment);
    let size = align_to(data.size, tag.alignment)?
        .checked_add(tag.size)
        .ok_or(MIRLayoutError::SizeOverflow)?;
    Ok(MIRTypeLayout {
        size: align_to(size, alignment)?,
        alignment,
    })
}

fn field_storage_layout<T: MTRegistry>(
    registry: &T,
    field: &MIRField,
) -> Result<MIRTypeLayout, MIRLayoutError> {
    match field {
        MIRField::Standard { type_id, .. } => layout_inner(registry, *type_id),
        MIRField::Bitfield {
            integer_type_id,
            width,
            ..
        } => {
            let layout = layout_inner(registry, *integer_type_id)?;
            validate_bitfield(*width, layout)?;
            Ok(if *width == 0 {
                MIRTypeLayout {
                    size: 0,
                    alignment: layout.alignment,
                }
            } else {
                layout
            })
        }
    }
}

fn aggregate_field_layout<T: MTRegistry>(
    registry: &T,
    aggregate: MIRTypeID,
    fields: &[MIRField],
    is_union: bool,
    field_index: usize,
) -> Result<MIRFieldLayout, MIRLayoutError> {
    let mut offset = 0;
    let mut active: Option<(MIRTypeID, usize, usize, usize)> = None;
    for (index, field) in fields.iter().enumerate() {
        match field {
            MIRField::Standard { type_id, .. } => {
                if let Some((_, start, size, _)) = active.take() {
                    offset = start + size;
                }
                let ty = layout_inner(registry, *type_id)?;
                offset = if is_union {
                    0
                } else {
                    align_to(offset, ty.alignment)?
                };
                if index == field_index {
                    return Ok(MIRFieldLayout::Standard {
                        offset,
                        ty: *type_id,
                    });
                }
                if !is_union {
                    offset = offset
                        .checked_add(ty.size)
                        .ok_or(MIRLayoutError::SizeOverflow)?;
                }
            }
            MIRField::Bitfield {
                integer_type_id,
                width,
                ..
            } => {
                let ty = layout_inner(registry, *integer_type_id)?;
                validate_bitfield(*width, ty)?;
                if is_union {
                    if index == field_index {
                        return Ok(MIRFieldLayout::Bitfield {
                            offset: 0,
                            bit_offset: 0,
                            bit_width: *width,
                            storage_type: *integer_type_id,
                        });
                    }
                    continue;
                }
                if *width == 0 {
                    active = None;
                    offset = align_to(offset, ty.alignment)?;
                    continue;
                }
                let (start, bit_offset) = match active.take() {
                    Some((id, start, _size, used))
                        if id == *integer_type_id && used + *width <= ty.size * 8 =>
                    {
                        (start, used)
                    }
                    Some((_, start, size, _)) => {
                        offset = align_to(start + size, ty.alignment)?;
                        (offset, 0)
                    }
                    None => {
                        offset = align_to(offset, ty.alignment)?;
                        (offset, 0)
                    }
                };
                if index == field_index {
                    return Ok(MIRFieldLayout::Bitfield {
                        offset: start,
                        bit_offset,
                        bit_width: *width,
                        storage_type: *integer_type_id,
                    });
                }
                active = Some((*integer_type_id, start, ty.size, bit_offset + *width));
            }
        }
    }
    Err(MIRLayoutError::InvalidField {
        ty: aggregate,
        field: field_index,
    })
}

fn validate_bitfield(width: usize, storage: MIRTypeLayout) -> Result<(), MIRLayoutError> {
    let storage_bits = storage
        .size
        .checked_mul(8)
        .ok_or(MIRLayoutError::SizeOverflow)?;
    if width > storage_bits {
        Err(MIRLayoutError::InvalidBitfieldWidth {
            width,
            storage_bits,
        })
    } else {
        Ok(())
    }
}

fn scalar_layout(size: usize) -> MIRTypeLayout {
    MIRTypeLayout {
        size,
        alignment: size.clamp(1, 8),
    }
}

struct ActiveBitfield {
    type_id: MIRTypeID,
    layout: MIRTypeLayout,
    used_bits: usize,
}

fn flush_bitfield(
    size: &mut usize,
    active: &mut Option<ActiveBitfield>,
) -> Result<(), MIRLayoutError> {
    if let Some(active) = active.take() {
        *size = size
            .checked_add(active.layout.size)
            .ok_or(MIRLayoutError::SizeOverflow)?;
    }
    Ok(())
}

fn align_to(size: usize, alignment: usize) -> Result<usize, MIRLayoutError> {
    if alignment == 0 {
        return Err(MIRLayoutError::InvalidAlignment(alignment));
    }
    let remainder = size % alignment;
    if remainder == 0 {
        Ok(size)
    } else {
        size.checked_add(alignment - remainder)
            .ok_or(MIRLayoutError::SizeOverflow)
    }
}
