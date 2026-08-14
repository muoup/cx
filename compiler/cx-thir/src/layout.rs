use cx_log::{CXRawResult, error::message::CXStdErrMessage};

use crate::{
    thir::{
        data::{THIRType, THIRTypeID, THIRTypeKind},
        r#type::THIRField,
    },
    type_context::THIRTypeContext,
};

#[derive(Debug, Clone, Copy, PartialEq, Eq)]
pub struct THIRTypeLayout {
    pub size: usize,
    pub alignment: usize,
}

pub fn layout_of<Context: THIRTypeContext + ?Sized>(
    definitions: &Context,
    ty: &THIRType,
) -> CXRawResult<THIRTypeLayout> {
    match &ty.kind {
        THIRTypeKind::Void => Ok(THIRTypeLayout {
            size: 0,
            alignment: 1,
        }),
        THIRTypeKind::Integer { _type, .. } => Ok(scalar_layout(_type.bytes())),
        THIRTypeKind::Float { _type } => Ok(scalar_layout(_type.bytes())),
        THIRTypeKind::PointerTo { .. } | THIRTypeKind::MemoryReference { .. } => {
            let architecture = definitions.architecture();
            Ok(THIRTypeLayout {
                size: architecture.pointer_size(),
                alignment: architecture.pointer_alignment(),
            })
        }
        THIRTypeKind::Array { length, inner_type } => {
            let inner = layout_of(definitions, definitions.resolve_type_id(*inner_type))?;
            Ok(THIRTypeLayout {
                size: inner.size * length,
                alignment: inner.alignment,
            })
        }
        THIRTypeKind::Structured { fields } => struct_layout(definitions, fields),
        THIRTypeKind::Union { variants } => union_layout(definitions, variants),
        THIRTypeKind::TaggedUnion { variants } => tagged_union_layout(definitions, variants),
        THIRTypeKind::Opaque { size, alignment } => Ok(THIRTypeLayout {
            size: *size,
            alignment: *alignment,
        }),
        THIRTypeKind::Function { .. } | THIRTypeKind::Str => Err(CXStdErrMessage::error(
            "MIRLayoutError",
            format!("cannot compute layout of type: {ty:?}"),
        )),
        THIRTypeKind::Undefined => Err(CXStdErrMessage::error(
            "MIRLayoutError",
            format!("cannot compute layout of undefined type: {ty:?}"),
        )),
    }
    .map(|mut layout| {
        layout.alignment = layout
            .alignment
            .clamp(1, definitions.architecture().pointer_size());

        if let Some(alignment) = ty.attributes.minimum_alignment {
            layout.alignment = layout.alignment.max(alignment);
        }

        layout.size = align_to(layout.size, layout.alignment);
        layout
    })
}

fn scalar_layout(size: usize) -> THIRTypeLayout {
    THIRTypeLayout {
        size,
        alignment: size.clamp(1, 8),
    }
}

fn struct_layout<Context: THIRTypeContext + ?Sized>(
    definitions: &Context,
    fields: &[THIRField],
) -> CXRawResult<THIRTypeLayout> {
    let mut size = 0;
    let mut alignment = 1;
    let mut active_bitfield: Option<ActiveBitfield> = None;

    for field in fields {
        match field {
            THIRField::Standard { type_id, .. } => {
                flush_bitfield(&mut size, &mut active_bitfield);
                let field_layout = layout_of(definitions, definitions.resolve_type_id(*type_id))?;
                size = align_to(size, field_layout.alignment);
                size += field_layout.size;
                alignment = alignment.max(field_layout.alignment);
            }
            THIRField::Bitfield {
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
    Ok(THIRTypeLayout {
        size: align_to(size, alignment),
        alignment,
    })
}

fn union_layout<Context: THIRTypeContext + ?Sized>(
    definitions: &Context,
    fields: &[THIRField],
) -> CXRawResult<THIRTypeLayout> {
    let mut size = 0;
    let mut alignment = 1;

    for field in fields {
        let layout = field_layout(definitions, field)?;
        size = size.max(layout.size);
        alignment = alignment.max(layout.alignment);
    }

    Ok(THIRTypeLayout {
        size: align_to(size, alignment),
        alignment,
    })
}

fn tagged_union_layout<Context: THIRTypeContext + ?Sized>(
    definitions: &Context,
    variants: &[THIRField],
) -> CXRawResult<THIRTypeLayout> {
    let data = union_layout(definitions, variants)?;
    let tag = scalar_layout(1);
    let alignment = data.alignment.max(tag.alignment);
    let tag_offset = align_to(data.size, tag.alignment);

    Ok(THIRTypeLayout {
        size: align_to(tag_offset + tag.size, alignment),
        alignment,
    })
}

fn field_layout<Context: THIRTypeContext + ?Sized>(
    definitions: &Context,
    field: &THIRField,
) -> CXRawResult<THIRTypeLayout> {
    match field {
        THIRField::Standard { type_id, .. } => {
            layout_of(definitions, definitions.resolve_type_id(*type_id))
        }
        THIRField::Bitfield {
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
                THIRTypeLayout {
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
    type_id: THIRTypeID,
    layout: THIRTypeLayout,
    used_bits: usize,
}

fn flush_bitfield(size: &mut usize, active: &mut Option<ActiveBitfield>) {
    if let Some(active) = active.take() {
        *size += active.layout.size;
    }
}
