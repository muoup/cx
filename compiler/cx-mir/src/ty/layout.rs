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

    pub fn apply_minimum(self, minimum: MIRTypeLayout) -> MIRTypeLayout {
        MIRTypeLayout {
            size: self.size.max(minimum.size),
            alignment: self.alignment.max(minimum.alignment),
        }
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

pub fn calculate_type_layout<Registry: MTRegistry>(
    registry: &Registry,
    ty: MIRTypeID,
) -> MIRTypeLayout {
    let mut active = HashSet::new();
    layout_inner(registry, ty, &mut active)
}

pub fn calculate_field_layout<Registry: MTRegistry>(
    registry: &Registry,
    field: &MIRField,
) -> MIRTypeLayout {
    match field {
        MIRField::Standard { type_id, .. } => calculate_type_layout(registry, *type_id),
        MIRField::Bitfield {
            integer_type_id,
            width,
            ..
        } => {
            let layout = calculate_type_layout(registry, *integer_type_id);
            assert!(
                *width <= layout.size * 8,
                "bitfield exceeds its storage type"
            );
            MIRTypeLayout::new(if *width == 0 { 0 } else { layout.size }, layout.alignment)
        }
    }
}

fn layout_inner<Registry: MTRegistry>(
    registry: &Registry,
    ty: MIRTypeID,
    active: &mut HashSet<MIRTypeID>,
) -> MIRTypeLayout {
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

    let layout = definition
        .layout()
        .map_or(layout, |minimum| layout.apply_minimum(minimum));
    let alignment = layout.alignment.max(1);
    MIRTypeLayout::new(align_to(layout.size, alignment), alignment)
}

fn aggregate_layout<Registry: MTRegistry>(
    registry: &Registry,
    fields: &[MIRField],
    is_union: bool,
    active: &mut HashSet<MIRTypeID>,
) -> MIRTypeLayout {
    let mut size = 0usize;
    let mut alignment = 1usize;
    let mut bitfield: Option<(MIRTypeID, usize)> = None;

    for field in fields {
        let storage = layout_inner(registry, field.ty(), active);
        alignment = alignment.max(storage.alignment);

        match field {
            MIRField::Standard { .. } => {
                bitfield = None;
                if is_union {
                    size = size.max(storage.size);
                } else {
                    size = align_to(size, storage.alignment)
                        .checked_add(storage.size)
                        .expect("aggregate layout overflows");
                }
            }
            MIRField::Bitfield { width, .. } => {
                assert!(
                    *width <= storage.size * 8,
                    "bitfield exceeds its storage type"
                );
                if *width == 0 {
                    bitfield = None;
                    if !is_union {
                        size = align_to(size, storage.alignment);
                    }
                } else if is_union {
                    size = size.max(storage.size);
                } else if let Some((ty, used)) = bitfield.as_mut()
                    && *ty == field.ty()
                    && *used + *width <= storage.size * 8
                {
                    *used += *width;
                } else {
                    size = align_to(size, storage.alignment)
                        .checked_add(storage.size)
                        .expect("aggregate layout overflows");
                    bitfield = Some((field.ty(), *width));
                }
            }
        }
    }

    MIRTypeLayout::new(align_to(size, alignment), alignment)
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
