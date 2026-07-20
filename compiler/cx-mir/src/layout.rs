use cx_log::{CXRawResult, error::message::CXStdErrMessage};

use crate::{
    mir::{
        data::{MIRType, MIRTypeKind},
        r#type::MIRField,
    },
    type_context::MIRTypeContext,
};

#[derive(Debug, Clone, Copy, PartialEq, Eq)]
pub struct MIRTypeLayout {
    pub size: usize,
    pub alignment: usize,
}

pub fn layout_of<Context: MIRTypeContext + ?Sized>(
    definitions: &Context,
    ty: &MIRType,
) -> CXRawResult<MIRTypeLayout> {
    match &ty.kind {
        MIRTypeKind::Unit => Ok(MIRTypeLayout {
            size: 0,
            alignment: 1,
        }),
        MIRTypeKind::Integer { _type, .. } => Ok(scalar_layout(_type.bytes())),
        MIRTypeKind::Float { _type } => Ok(scalar_layout(_type.bytes())),
        MIRTypeKind::PointerTo { .. } | MIRTypeKind::MemoryReference { .. } => {
            let architecture = definitions.architecture();
            Ok(MIRTypeLayout {
                size: architecture.pointer_size(),
                alignment: architecture.pointer_alignment(),
            })
        }
        MIRTypeKind::Array { length, inner_type } => {
            let inner = layout_of(definitions, definitions.resolve_type_id(*inner_type))?;
            Ok(MIRTypeLayout {
                size: inner.size * length,
                alignment: inner.alignment,
            })
        }
        MIRTypeKind::Structured { fields } => struct_layout(definitions, fields),
        MIRTypeKind::Union { variants } => union_layout(definitions, variants),
        MIRTypeKind::TaggedUnion { variants } => tagged_union_layout(definitions, variants),
        MIRTypeKind::Opaque { size, alignment } => Ok(MIRTypeLayout {
            size: *size,
            alignment: *alignment,
        }),
        MIRTypeKind::Function { .. } | MIRTypeKind::Str => Err(CXStdErrMessage::error(
            "MIRLayoutError",
            format!("cannot compute layout of type: {ty:?}"),
        )),
        MIRTypeKind::Undefined => Err(CXStdErrMessage::error(
            "MIRLayoutError",
            format!("cannot compute layout of undefined type: {ty:?}"),
        )),
    }
    .map(|mut layout| {
        if let Some(alignment) = ty.attributes.minimum_alignment {
            layout.alignment = layout.alignment.max(alignment);
        }

        layout.alignment = layout
            .alignment
            .clamp(1, definitions.architecture().pointer_size());
        layout
    })
}

fn scalar_layout(size: usize) -> MIRTypeLayout {
    MIRTypeLayout {
        size,
        alignment: size.clamp(1, 8),
    }
}

fn struct_layout<Context: MIRTypeContext + ?Sized>(
    definitions: &Context,
    fields: &[MIRField],
) -> CXRawResult<MIRTypeLayout> {
    let mut size = 0;
    let mut alignment = 1;
    let mut active_bitfield: Option<ActiveBitfield> = None;

    for field in fields {
        match field {
            MIRField::Standard { type_id, .. } => {
                flush_bitfield(&mut size, &mut active_bitfield);
                let field_layout = layout_of(definitions, definitions.resolve_type_id(*type_id))?;
                size = align_to(size, field_layout.alignment);
                size += field_layout.size;
                alignment = alignment.max(field_layout.alignment);
            }
            MIRField::Bitfield {
                integer_type_id,
                width,
                ..
            } => {
                let storage =
                    layout_of(definitions, definitions.resolve_type_id(*integer_type_id))?;
                let storage_bits = storage.size * 8;
                if *width > storage_bits {
                    return Err(CXStdErrMessage::error(
                        "MIRLayoutError",
                        format!(
                            "invalid bitfield width: {} exceeds storage size of {} bits",
                            width, storage_bits
                        ),
                    ));
                }

                alignment = alignment.max(storage.alignment);
                if *width == 0 {
                    flush_bitfield(&mut size, &mut active_bitfield);
                    size = align_to(size, storage.alignment);
                    continue;
                }

                let can_share = active_bitfield.as_ref().is_some_and(|active| {
                    active.type_id == *integer_type_id
                        && active.used_bits + *width <= active.layout.size * 8
                });
                if !can_share {
                    flush_bitfield(&mut size, &mut active_bitfield);
                    size = align_to(size, storage.alignment);
                    active_bitfield = Some(ActiveBitfield {
                        type_id: *integer_type_id,
                        layout: storage,
                        used_bits: 0,
                    });
                }

                active_bitfield.as_mut().unwrap().used_bits += *width;
            }
        }
    }

    flush_bitfield(&mut size, &mut active_bitfield);
    Ok(MIRTypeLayout {
        size: align_to(size, alignment),
        alignment,
    })
}

fn union_layout<Context: MIRTypeContext + ?Sized>(
    definitions: &Context,
    fields: &[MIRField],
) -> CXRawResult<MIRTypeLayout> {
    let mut size = 0;
    let mut alignment = 1;

    for field in fields {
        let layout = field_layout(definitions, field)?;
        size = size.max(layout.size);
        alignment = alignment.max(layout.alignment);
    }

    Ok(MIRTypeLayout {
        size: align_to(size, alignment),
        alignment,
    })
}

fn tagged_union_layout<Context: MIRTypeContext + ?Sized>(
    definitions: &Context,
    variants: &[MIRField],
) -> CXRawResult<MIRTypeLayout> {
    let data = union_layout(definitions, variants)?;
    let tag = scalar_layout(1);
    let alignment = data.alignment.max(tag.alignment);
    let tag_offset = align_to(data.size, tag.alignment);

    Ok(MIRTypeLayout {
        size: align_to(tag_offset + tag.size, alignment),
        alignment,
    })
}

fn field_layout<Context: MIRTypeContext + ?Sized>(
    definitions: &Context,
    field: &MIRField,
) -> CXRawResult<MIRTypeLayout> {
    match field {
        MIRField::Standard { type_id, .. } => {
            layout_of(definitions, definitions.resolve_type_id(*type_id))
        }
        MIRField::Bitfield {
            integer_type_id,
            width,
            ..
        } => {
            let layout = layout_of(definitions, definitions.resolve_type_id(*integer_type_id))?;
            let storage_bits = layout.size * 8;
            if *width > storage_bits {
                return Err(CXStdErrMessage::error(
                    "MIRLayoutError",
                    format!(
                        "invalid bitfield width: {} exceeds storage size of {} bits",
                        width, storage_bits
                    ),
                ));
            }
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

fn align_to(size: usize, alignment: usize) -> usize {
    size.div_ceil(alignment) * alignment
}

struct ActiveBitfield {
    type_id: crate::mir::r#type::MIRTypeId,
    layout: MIRTypeLayout,
    used_bits: usize,
}

fn flush_bitfield(size: &mut usize, active: &mut Option<ActiveBitfield>) {
    if let Some(active) = active.take() {
        *size += active.layout.size;
    }
}
