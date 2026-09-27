use std::collections::HashSet;

use crate::ty::{MIRField, MIRTypeKind, interface::MTRegistry};

use super::MIRTypeID;

#[derive(Debug, Clone, Copy, PartialEq, Eq, Hash)]
pub struct MIRTypeLayout {
    size: usize,
    alignment: usize,
}

impl MIRTypeLayout {
    pub fn new(size: usize, alignment: usize) -> Self {
        Self { size, alignment }
    }

    pub fn size(&self) -> usize {
        self.size
    }

    pub fn alignment(&self) -> usize {
        self.alignment
    }
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

impl MIRFieldLayout {
    pub fn offset(&self) -> usize {
        match self {
            Self::Standard { offset, .. } | Self::Bitfield { offset, .. } => *offset,
        }
    }

    /// The type stored at `offset()`; for a bitfield, its storage unit.
    pub fn ty(&self) -> MIRTypeID {
        match self {
            Self::Standard { ty, .. } => *ty,
            Self::Bitfield { storage_type, .. } => *storage_type,
        }
    }
}

pub fn calculate_type_layout<Registry: MTRegistry>(
    registry: &Registry,
    ty: MIRTypeID,
) -> MIRTypeLayout {
    let mut active = HashSet::new();
    layout_inner(registry, ty, &mut active)
}

pub fn calculate_field_layouts<Registry: MTRegistry>(
    registry: &Registry,
    ty: MIRTypeID,
) -> Option<Vec<MIRFieldLayout>> {
    let (fields, is_union) = match registry.definition(ty)?.kind() {
        MIRTypeKind::Structured { fields } => (fields, false),
        MIRTypeKind::Union { variants } | MIRTypeKind::TaggedUnion { variants } => (variants, true),
        _ => return None,
    };
    let (layouts, _) = layout_fields(
        fields,
        is_union,
        |ty| calculate_type_layout(registry, ty),
    );
    Some(layouts)
}

pub fn calculate_field_layout<Registry: MTRegistry>(
    registry: &Registry,
    ty: MIRTypeID,
    index: usize,
) -> Option<MIRFieldLayout> {
    calculate_field_layouts(registry, ty)?.get(index).copied()
}

fn layout_inner<Registry: MTRegistry>(
    registry: &Registry,
    ty: MIRTypeID,
    active: &mut HashSet<MIRTypeID>,
) -> MIRTypeLayout {
    fn aggregate_layout<Registry: MTRegistry>(
        registry: &Registry,
        fields: &[MIRField],
        is_union: bool,
        active: &mut HashSet<MIRTypeID>,
    ) -> MIRTypeLayout {
        layout_fields(fields, is_union, |ty| layout_inner(registry, ty, active)).1
    }

    assert!(active.insert(ty), "recursive value type {ty}");
    let definition = registry.definition(ty).expect("MIR type has no definition");
    let layout = match definition.kind() {
        MIRTypeKind::Void => MIRTypeLayout::new(0, 1),
        MIRTypeKind::Integer { ty, .. } => MIRTypeLayout::new(
            ty.bytes(),
            ty.bytes().min(registry.architecture().pointer_size()),
        ),
        MIRTypeKind::Float { ty } => MIRTypeLayout::new(
            ty.bytes(),
            ty.bytes().min(registry.architecture().pointer_size()),
        ),
        MIRTypeKind::PointerTo { .. }
        | MIRTypeKind::MemoryReference { .. }
        | MIRTypeKind::Function { .. } => MIRTypeLayout::new(
            registry.architecture().pointer_size(),
            registry.architecture().pointer_alignment(),
        ),
        MIRTypeKind::Array { length, inner } => {
            let element = layout_inner(registry, *inner, active);
            MIRTypeLayout::new(
                element
                    .size
                    .checked_mul(*length)
                    .expect("array layout overflows"),
                element.alignment,
            )
        }
        MIRTypeKind::IncompleteArray { .. } => panic!("incomplete array has no layout"),
        MIRTypeKind::Structured { fields } => aggregate_layout(registry, fields, false, active),
        MIRTypeKind::Union { variants } => aggregate_layout(registry, variants, true, active),
        MIRTypeKind::TaggedUnion { variants } => {
            let data = aggregate_layout(registry, variants, true, active);
            MIRTypeLayout::new(
                data.size.checked_add(1).expect("sum layout overflows"),
                data.alignment,
            )
        }
        MIRTypeKind::Opaque { size, alignment } => MIRTypeLayout::new(*size, *alignment),
        MIRTypeKind::Str => MIRTypeLayout::new(1, 1),
        MIRTypeKind::Undefined => panic!("undefined MIR type {ty} has no layout"),
    };
    active.remove(&ty);

    let alignment = layout.alignment.max(1);
    MIRTypeLayout::new(align_to(layout.size, alignment), alignment)
}

/// System V bitfield layout: a bitfield takes the next free bit unless it would cross an alignment
/// boundary of its type, and is accessed as a word of its type at the aligned offset containing it.
fn layout_fields(
    fields: &[MIRField],
    is_union: bool,
    mut storage_layout: impl FnMut(MIRTypeID) -> MIRTypeLayout,
) -> (Vec<MIRFieldLayout>, MIRTypeLayout) {
    let mut layouts = Vec::with_capacity(fields.len());
    let mut bits = 0usize;
    let mut size = 0usize;
    let mut alignment = 1usize;

    for field in fields {
        let storage = storage_layout(field.ty());

        let layout = match field {
            MIRField::Standard { type_id, .. } => {
                alignment = alignment.max(storage.alignment);
                let offset = if is_union {
                    0
                } else {
                    align_to(bits.div_ceil(8), storage.alignment)
                };
                let end = offset
                    .checked_add(storage.size)
                    .expect("aggregate layout overflows");
                bits = end * 8;
                size = size.max(end);
                MIRFieldLayout::Standard {
                    offset,
                    ty: *type_id,
                }
            }
            MIRField::Bitfield {
                name,
                integer_type_id,
                width,
            } => {
                let capacity = storage.size * 8;
                assert!(*width <= capacity, "bitfield exceeds its storage type");
                let boundary = storage.alignment * 8;

                let start = if is_union {
                    0
                } else if *width == 0 || bits % boundary + *width > capacity {
                    align_to(bits, boundary)
                } else {
                    bits
                };
                let offset = start / boundary * storage.alignment;
                if *width != 0 {
                    bits = start + *width;
                    size = size.max(bits.div_ceil(8));
                    if name.is_some() {
                        alignment = alignment.max(storage.alignment);
                        size = size.max(offset + storage.size);
                    }
                } else {
                    bits = start;
                }

                MIRFieldLayout::Bitfield {
                    offset,
                    bit_offset: start - offset * 8,
                    bit_width: *width,
                    storage_type: *integer_type_id,
                }
            }
        };
        layouts.push(layout);
    }

    if !is_union {
        size = size.max(bits.div_ceil(8));
    }
    (
        layouts,
        MIRTypeLayout::new(align_to(size, alignment), alignment),
    )
}

fn align_to(size: usize, alignment: usize) -> usize {
    assert!(alignment != 0, "type alignment must be nonzero");
    let remainder = size % alignment;
    if remainder == 0 {
        size
    } else {
        size.checked_add(alignment - remainder)
            .expect("type layout overflows")
    }
}
