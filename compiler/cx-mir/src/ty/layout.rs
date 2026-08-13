use std::collections::HashSet;

use super::{MIRField, MIRTypeID, MIRTypeKind, MIRTypeRegistry};

#[derive(Debug, Clone, Copy, PartialEq, Eq)]
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

impl MIRTypeRegistry {
    pub fn compute_layouts(&mut self) -> Result<(), MIRLayoutError> {
        for index in 0..self.definitions.len() {
            let id = MIRTypeID::new(index);
            if self.definition(id).is_none() {
                continue;
            }
            if matches!(self.kind(id), Some(MIRTypeKind::Undefined)) {
                continue;
            }
            let layout = self.layout_of(id)?;
            self.layouts[index] = Some(layout);
        }
        Ok(())
    }

    pub fn layout(&self, id: MIRTypeID) -> Result<MIRTypeLayout, MIRLayoutError> {
        self.layouts
            .get(id.index())
            .and_then(|layout| *layout)
            .map(Ok)
            .unwrap_or_else(|| self.layout_of(id))
    }

    pub fn layout_of(&self, id: MIRTypeID) -> Result<MIRTypeLayout, MIRLayoutError> {
        self.layout_inner(id, &mut HashSet::new())
    }

    pub fn field_layout(
        &self,
        aggregate: MIRTypeID,
        field_index: usize,
    ) -> Result<MIRFieldLayout, MIRLayoutError> {
        let aggregate = match self.kind(aggregate) {
            Some(MIRTypeKind::MemoryReference { inner, .. }) => *inner,
            _ => aggregate,
        };
        let (fields, is_union) = match self.kind(aggregate) {
            Some(MIRTypeKind::Structured { fields }) => (fields, false),
            Some(MIRTypeKind::Union { variants }) => (variants, true),
            _ => {
                return Err(MIRLayoutError::InvalidField {
                    ty: aggregate,
                    field: field_index,
                });
            }
        };
        self.aggregate_field_layout(aggregate, fields, is_union, field_index)
    }

    pub fn tagged_union_tag_offset(&self, sum: MIRTypeID) -> Result<usize, MIRLayoutError> {
        let Some(MIRTypeKind::TaggedUnion { variants }) = self.kind(sum) else {
            return Err(MIRLayoutError::InvalidType(sum));
        };
        let data = self.union_layout(variants, &mut HashSet::new())?;
        align_to(data.size, 1)
    }

    fn layout_inner(
        &self,
        id: MIRTypeID,
        visiting: &mut HashSet<MIRTypeID>,
    ) -> Result<MIRTypeLayout, MIRLayoutError> {
        if !visiting.insert(id) {
            return Err(MIRLayoutError::RecursiveType(id));
        }
        let definition = self.definition(id).ok_or(MIRLayoutError::InvalidType(id))?;
        let mut layout = match &definition.kind {
            MIRTypeKind::Unit => MIRTypeLayout {
                size: 0,
                alignment: 1,
            },
            MIRTypeKind::Integer { ty, .. } => scalar_layout(ty.bytes()),
            MIRTypeKind::Float { ty } => scalar_layout(ty.bytes()),
            MIRTypeKind::PointerTo { .. }
            | MIRTypeKind::MemoryReference { .. }
            | MIRTypeKind::Function { .. } => MIRTypeLayout {
                size: self.architecture.pointer_size(),
                alignment: self.architecture.pointer_alignment(),
            },
            MIRTypeKind::Array { length, inner } => {
                let inner = self.layout_inner(*inner, visiting)?;
                MIRTypeLayout {
                    size: inner
                        .size
                        .checked_mul(*length)
                        .ok_or(MIRLayoutError::SizeOverflow)?,
                    alignment: inner.alignment,
                }
            }
            MIRTypeKind::Structured { fields } => self.struct_layout(fields, visiting)?,
            MIRTypeKind::Union { variants } => self.union_layout(variants, visiting)?,
            MIRTypeKind::TaggedUnion { variants } => {
                self.tagged_union_layout(variants, visiting)?
            }
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
        visiting.remove(&id);

        layout.alignment = layout.alignment.clamp(1, self.architecture.pointer_size());
        if let Some(alignment) = definition.minimum_alignment {
            if alignment == 0 || !alignment.is_power_of_two() {
                return Err(MIRLayoutError::InvalidAlignment(alignment));
            }
            layout.alignment = layout.alignment.max(alignment);
        }
        layout.size = align_to(layout.size, layout.alignment)?;
        Ok(layout)
    }

    fn struct_layout(
        &self,
        fields: &[MIRField],
        visiting: &mut HashSet<MIRTypeID>,
    ) -> Result<MIRTypeLayout, MIRLayoutError> {
        let mut size = 0;
        let mut alignment = 1;
        let mut active = None;

        for field in fields {
            match field {
                MIRField::Standard { type_id } => {
                    flush_bitfield(&mut size, &mut active)?;
                    let field_layout = self.layout_inner(*type_id, visiting)?;
                    size = align_to(size, field_layout.alignment)?;
                    size = size
                        .checked_add(field_layout.size)
                        .ok_or(MIRLayoutError::SizeOverflow)?;
                    alignment = alignment.max(field_layout.alignment);
                }
                MIRField::Bitfield {
                    integer_type_id,
                    width,
                } => {
                    let storage = self.layout_inner(*integer_type_id, visiting)?;
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

    fn union_layout(
        &self,
        fields: &[MIRField],
        visiting: &mut HashSet<MIRTypeID>,
    ) -> Result<MIRTypeLayout, MIRLayoutError> {
        let mut size = 0;
        let mut alignment = 1;
        for field in fields {
            let layout = self.field_storage_layout(field, visiting)?;
            size = size.max(layout.size);
            alignment = alignment.max(layout.alignment);
        }
        Ok(MIRTypeLayout {
            size: align_to(size, alignment)?,
            alignment,
        })
    }

    fn tagged_union_layout(
        &self,
        variants: &[MIRField],
        visiting: &mut HashSet<MIRTypeID>,
    ) -> Result<MIRTypeLayout, MIRLayoutError> {
        let data = self.union_layout(variants, visiting)?;
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

    fn field_storage_layout(
        &self,
        field: &MIRField,
        visiting: &mut HashSet<MIRTypeID>,
    ) -> Result<MIRTypeLayout, MIRLayoutError> {
        match field {
            MIRField::Standard { type_id } => self.layout_inner(*type_id, visiting),
            MIRField::Bitfield {
                integer_type_id,
                width,
            } => {
                let layout = self.layout_inner(*integer_type_id, visiting)?;
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

    fn aggregate_field_layout(
        &self,
        aggregate: MIRTypeID,
        fields: &[MIRField],
        is_union: bool,
        field_index: usize,
    ) -> Result<MIRFieldLayout, MIRLayoutError> {
        let mut offset = 0;
        let mut active: Option<(MIRTypeID, usize, usize, usize)> = None;
        for (index, field) in fields.iter().enumerate() {
            match field {
                MIRField::Standard { type_id } => {
                    if let Some((_, start, size, _)) = active.take() {
                        offset = start + size;
                    }
                    let layout = self.layout(*type_id)?;
                    offset = if is_union {
                        0
                    } else {
                        align_to(offset, layout.alignment)?
                    };
                    if index == field_index {
                        return Ok(MIRFieldLayout::Standard {
                            offset,
                            ty: *type_id,
                        });
                    }
                    if !is_union {
                        offset = offset
                            .checked_add(layout.size)
                            .ok_or(MIRLayoutError::SizeOverflow)?;
                    }
                }
                MIRField::Bitfield {
                    integer_type_id,
                    width,
                } => {
                    let layout = self.layout(*integer_type_id)?;
                    validate_bitfield(*width, layout)?;
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
                        offset = align_to(offset, layout.alignment)?;
                        continue;
                    }
                    let (start, bit_offset) = match active.take() {
                        Some((id, start, _size, used))
                            if id == *integer_type_id && used + *width <= layout.size * 8 =>
                        {
                            (start, used)
                        }
                        Some((_, start, size, _)) => {
                            offset = align_to(start + size, layout.alignment)?;
                            (offset, 0)
                        }
                        None => {
                            offset = align_to(offset, layout.alignment)?;
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
                    active = Some((*integer_type_id, start, layout.size, bit_offset + *width));
                }
            }
        }
        Err(MIRLayoutError::InvalidField {
            ty: aggregate,
            field: field_index,
        })
    }
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
