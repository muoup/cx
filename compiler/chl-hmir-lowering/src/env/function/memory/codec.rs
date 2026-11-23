use std::{cell::RefCell, ops::Range};

use chl_hmir::ty::{
    HMIRTypeID, HMIRTypeKind, HMIRUndefined,
    aggregate::HMIRBitfield,
    context::HMIRTypeContext,
    intrinsic::{HMIRFloatWidth, HMIRIntType},
};
use chl_log::{CHLResult, catalogue::mir};
use chl_mir::ty::layout::{MIRFieldLayout, MIRTypeLayout, calculate_type_layout, layout_fields};
use chl_tokens::TokenRange;
use chl_util::unsafe_float::FloatWrapper;

use super::{ByteState, Cell, Fault, Image, Memory, address};
use crate::{
    emit::comptime::int,
    env::{HMIREnvironment, resolver::demand},
    eval::types,
    log::hmir_error,
    lower::types::lower_type,
    value::{HMIRValueKind, immediate::HMIRImmValue},
};

pub(crate) fn layout(
    env: &mut HMIREnvironment,
    ty: HMIRTypeID,
    span: &TokenRange,
) -> CHLResult<MIRTypeLayout> {
    demand::force(env, ty)?;
    match env.resolve_kind(ty).clone() {
        HMIRTypeKind::Array { element, length } => {
            let element = layout(env, element, span)?;
            let size = usize::try_from(length)
                .ok()
                .and_then(|length| element.size().checked_mul(length))
                .ok_or_else(|| hmir_error(span, &mir::SIZE_OVERFLOW, ()))?;
            Ok(MIRTypeLayout::new(size, element.alignment()))
        }
        HMIRTypeKind::Undefined(_) => Err(hmir_error(
            span,
            &mir::COMPTIME_INVALID_OPERATION,
            format!("storage of the incomplete type '{}'", types::name(env, ty)),
        )),
        _ if types::opaque_slot(env, ty) => Ok(MIRTypeLayout::new(1, 1)),
        HMIRTypeKind::Struct { .. }
        | HMIRTypeKind::Union { .. }
        | HMIRTypeKind::TaggedUnion { .. }
            if types::comptime_only(env, ty) =>
        {
            let (_, payload) = record_layout(env, ty, span)?;
            Ok(
                if matches!(env.resolve_kind(ty), HMIRTypeKind::TaggedUnion { .. }) {
                    payload.with_tag()
                } else {
                    payload
                },
            )
        }
        _ => {
            let lowered = lower_type(env, ty, span)?;
            Ok(calculate_type_layout(env.module(), lowered))
        }
    }
}

fn record_layout(
    env: &mut HMIREnvironment,
    ty: HMIRTypeID,
    span: &TokenRange,
) -> CHLResult<(Vec<MIRFieldLayout<HMIRTypeID>>, MIRTypeLayout)> {
    let kind = env.resolve_kind(ty).clone();
    let (fields, is_union) = match kind {
        HMIRTypeKind::Struct { fields } => (fields, false),
        HMIRTypeKind::Union { fields } | HMIRTypeKind::TaggedUnion { fields } => (fields, true),
        _ => unreachable!("only records have field layouts"),
    };
    let mut input = Vec::with_capacity(fields.len());
    for field in fields {
        let ty = *field.ty();
        let storage = match *env.resolve_kind(ty) {
            HMIRTypeKind::Undefined(HMIRUndefined::Array { element }) => {
                MIRTypeLayout::new(0, layout(env, element, span)?.alignment())
            }
            _ => layout(env, ty, span)?,
        };
        input.push((
            ty,
            storage,
            field
                .bit_width()
                .map(|width| (width, field.name().is_some())),
        ));
    }
    Ok(layout_fields(input.into_iter(), is_union))
}

pub(crate) fn member_layout(
    env: &mut HMIREnvironment,
    ty: HMIRTypeID,
    index: usize,
    span: &TokenRange,
) -> CHLResult<(usize, Option<HMIRBitfield>)> {
    match *env.resolve_kind(ty) {
        HMIRTypeKind::Struct { .. } | HMIRTypeKind::Union { .. } => {
            let field = record_layout(env, ty, span)?.0[index];
            Ok((field.offset(), field_bitfield(field)))
        }
        HMIRTypeKind::Array { element, .. } => {
            Ok((index * layout(env, element, span)?.size(), None))
        }
        _ => Ok((0, None)),
    }
}

fn field_bitfield(field: MIRFieldLayout<HMIRTypeID>) -> Option<HMIRBitfield> {
    match field {
        MIRFieldLayout::Bitfield {
            bit_offset: offset,
            bit_width: width,
            ..
        } => Some(HMIRBitfield { offset, width }),
        _ => None,
    }
}

pub(super) fn bitfield_window(bitfield: HMIRBitfield) -> (std::ops::Range<usize>, HMIRBitfield) {
    let start = bitfield.offset / 8;
    let end = (bitfield.offset + bitfield.width).div_ceil(8);
    (
        start..end,
        HMIRBitfield {
            offset: bitfield.offset % 8,
            ..bitfield
        },
    )
}

pub(super) fn extract(
    env: &HMIREnvironment,
    ty: HMIRTypeID,
    unit: &Image,
    bitfield: HMIRBitfield,
    span: &TokenRange,
) -> CHLResult<HMIRValueKind> {
    let int = types::int_type(env, ty);
    let word = unit
        .bytes(0..unit.len())
        .map_err(|fault| fault.error(span))?;
    Ok(HMIRImmValue::Int(bits(word, int::signed(int), bitfield)).into())
}

pub(super) fn insert(mut unit: Image, value: &HMIRValueKind, bitfield: HMIRBitfield) -> Image {
    let HMIRValueKind::Immediate(HMIRImmValue::Int(value)) = value else {
        unreachable!("a bitfield holds an integer")
    };
    unit.cells.clear();
    unit.states.fill(ByteState::Data);
    set_bits(&mut unit.bytes, *value, bitfield);
    unit
}

fn word(bytes: &[u8], bitfield: HMIRBitfield) -> (u128, u128) {
    let mut buffer = [0; 16];
    buffer[..bytes.len()].copy_from_slice(bytes);
    let mask = (u128::MAX >> (128 - bitfield.width)) << bitfield.offset;
    (u128::from_le_bytes(buffer), mask)
}

fn bits(bytes: &[u8], signed: bool, bitfield: HMIRBitfield) -> i128 {
    if bitfield.width == 0 {
        return 0;
    }
    let (word, mask) = word(bytes, bitfield);
    let bits = ((word & mask) >> bitfield.offset) as i128;
    let shift = 128 - bitfield.width;
    match signed {
        true => bits << shift >> shift,
        false => bits,
    }
}

fn set_bits(bytes: &mut [u8], value: i128, bitfield: HMIRBitfield) {
    if bitfield.width == 0 {
        return;
    }
    let (current, mask) = word(bytes, bitfield);
    let word = (current & !mask) | ((value as u128) << bitfield.offset & mask);
    let length = bytes.len();
    bytes.copy_from_slice(&word.to_le_bytes()[..length]);
}

pub(super) struct Codec<'a, 'global, 'hmir> {
    env: &'a mut HMIREnvironment<'global, 'hmir>,
    memory: &'a RefCell<Memory>,
    span: &'a TokenRange,
    preserving_addresses: bool,
}

impl<'a, 'global, 'hmir> Codec<'a, 'global, 'hmir> {
    pub(super) fn new(
        env: &'a mut HMIREnvironment<'global, 'hmir>,
        memory: &'a RefCell<Memory>,
        span: &'a TokenRange,
    ) -> Self {
        Self {
            env,
            memory,
            span,
            preserving_addresses: false,
        }
    }

    pub(super) fn encode(&mut self, ty: HMIRTypeID, value: &HMIRValueKind) -> CHLResult<Image> {
        let size = layout(self.env, ty, self.span)?.size();
        let mut image = Image::new(size, ByteState::Data);
        match value {
            HMIRValueKind::Immediate(value) => self.write(ty, value, &mut image, 0)?,
            _ if types::opaque_slot(self.env, ty) => self.opaque(&mut image, 0, ty, value.clone()),
            _ if self.is_pointer(ty) => self.write_pointer(&mut image, 0, value)?,
            _ => return Err(Fault::Opaque.error(self.span)),
        }
        Ok(image)
    }

    fn opaque(&self, image: &mut Image, offset: usize, ty: HMIRTypeID, value: HMIRValueKind) {
        let ty = self.env.underlying(ty);
        image.cells.insert(offset, Cell { ty, value });
    }

    fn is_pointer(&self, ty: HMIRTypeID) -> bool {
        self.env.is_pointer(ty) || self.env.is_reference(ty)
    }

    fn write_pointer(
        &mut self,
        image: &mut Image,
        offset: usize,
        value: &HMIRValueKind,
    ) -> CHLResult<()> {
        let bytes = address::encode(self.env, self.memory, value, self.span)?;
        let size = self.env.architecture().pointer_size();
        image.put(offset, &bytes[..size]);
        if bytes != [0; 16] {
            image.states[offset..offset + size].fill(ByteState::Address);
        }
        Ok(())
    }

    fn write(
        &mut self,
        ty: HMIRTypeID,
        value: &HMIRImmValue,
        image: &mut Image,
        offset: usize,
    ) -> CHLResult<()> {
        let ty = types::storage(self.env, ty, self.span);
        if types::opaque_slot(self.env, ty) {
            self.opaque(image, offset, ty, value.clone().into());
            return Ok(());
        }
        match (self.env.resolve_kind(ty).clone(), value) {
            (HMIRTypeKind::Int(int), HMIRImmValue::Int(value)) => {
                image.put(offset, &value.to_le_bytes()[..int.width().bytes()]);
            }
            (HMIRTypeKind::Float(float), HMIRImmValue::Float(value)) => {
                let value = f64::from(value);
                match float.width() {
                    HMIRFloatWidth::F32 => image.put(offset, &(value as f32).to_le_bytes()),
                    HMIRFloatWidth::F64 => image.put(offset, &value.to_le_bytes()),
                }
            }
            (_, HMIRImmValue::Unit) => {}
            _ if self.is_pointer(ty) => {
                self.write_pointer(image, offset, &value.clone().into())?;
            }
            (HMIRTypeKind::Array { element, .. }, HMIRImmValue::Aggregate { fields: items }) => {
                let stride = layout(self.env, element, self.span)?.size();
                for (index, item) in items.iter().enumerate() {
                    self.write(element, item, image, offset + index * stride)?;
                }
            }
            (HMIRTypeKind::Struct { fields }, HMIRImmValue::Aggregate { fields: items }) => {
                let layouts = self::record_layout(self.env, ty, self.span)?.0;
                for ((field, layout), item) in fields.iter().zip(layouts).zip(items) {
                    let Some(bitfield) = field_bitfield(layout) else {
                        self.write(*field.ty(), item, image, offset + layout.offset())?;
                        continue;
                    };
                    let HMIRImmValue::Int(value) = item else {
                        unreachable!("a bitfield holds an integer")
                    };
                    let (window, bitfield) = bitfield_window(bitfield);
                    let base = offset + layout.offset();
                    set_bits(
                        &mut image.bytes[base + window.start..base + window.end],
                        *value,
                        bitfield,
                    );
                }
            }
            (HMIRTypeKind::Union { fields }, HMIRImmValue::Union { variant, payload }) => {
                self.write(*fields[*variant].ty(), payload, image, offset)?;
            }
            (HMIRTypeKind::TaggedUnion { fields }, HMIRImmValue::Union { variant, payload }) => {
                self.write(*fields[*variant].ty(), payload, image, offset)?;
                let tag = offset + record_layout(self.env, ty, self.span)?.1.size();
                image.put(tag, &[*variant as u8]);
            }
            _ => return Err(Fault::Opaque.error(self.span)),
        }
        Ok(())
    }

    pub(super) fn decode(&mut self, ty: HMIRTypeID, image: &Image) -> CHLResult<HMIRValueKind> {
        let ty = types::storage(self.env, ty, self.span);
        match self.env.resolve_kind(ty) {
            HMIRTypeKind::PointerTo(_) | HMIRTypeKind::ReferenceTo { .. } => self.pointer(image, 0),
            _ if types::opaque_slot(self.env, ty) => self.cell(image, 0, ty).cloned(),
            _ => self.immediate(ty, image, 0).map(HMIRValueKind::Immediate),
        }
    }

    fn pointer(&self, image: &Image, offset: usize) -> CHLResult<HMIRValueKind> {
        let bytes = image
            .bytes(offset..offset + self.env.architecture().pointer_size())
            .map_err(|fault| fault.error(self.span))?;
        self.memory
            .borrow()
            .addresses
            .decode(bytes)
            .map_err(|fault| fault.error(self.span))
    }

    fn cell<'image>(
        &self,
        image: &'image Image,
        offset: usize,
        ty: HMIRTypeID,
    ) -> CHLResult<&'image HMIRValueKind> {
        let fault = match image.cells.get(&offset) {
            Some(cell) if cell.ty == self.env.underlying(ty) => return Ok(&cell.value),
            Some(_) => Fault::Opaque,
            None if image.states[offset] == ByteState::Uninitialized => Fault::Uninitialized,
            None => Fault::Opaque,
        };
        Err(fault.error(self.span))
    }

    fn bytes<'image>(&self, image: &'image Image, range: Range<usize>) -> CHLResult<&'image [u8]> {
        let bytes = image
            .bytes(range.clone())
            .map_err(|fault| fault.error(self.span))?;
        if self.preserving_addresses && image.states[range].contains(&ByteState::Address) {
            return Err(Fault::Relocation.error(self.span));
        }
        Ok(bytes)
    }

    pub(super) fn discriminant(
        &mut self,
        ty: HMIRTypeID,
        image: &Image,
        offset: usize,
    ) -> CHLResult<usize> {
        let (variants, payload) = record_layout(self.env, ty, self.span)?;
        let tag = offset + payload.size();
        let variant = self.bytes(image, tag..tag + 1)?[0] as usize;
        if variant < variants.len() {
            Ok(variant)
        } else {
            Err(Fault::Discriminant.error(self.span))
        }
    }

    fn immediate(
        &mut self,
        ty: HMIRTypeID,
        image: &Image,
        offset: usize,
    ) -> CHLResult<HMIRImmValue> {
        let span = self.span;
        let fault = |fault: Fault| fault.error(span);
        let ty = types::storage(self.env, ty, span);
        Ok(match self.env.resolve_kind(ty).clone() {
            HMIRTypeKind::Int(int) => {
                let bytes = self.bytes(image, offset..offset + int.width().bytes())?;
                HMIRImmValue::Int(integer(bytes, int))
            }
            HMIRTypeKind::Float(float) => {
                let bytes = self.bytes(image, offset..offset + float.width().bytes())?;
                let value = match float.width() {
                    HMIRFloatWidth::F32 => f32::from_le_bytes(bytes.try_into().unwrap()) as f64,
                    HMIRFloatWidth::F64 => f64::from_le_bytes(bytes.try_into().unwrap()),
                };
                HMIRImmValue::Float(FloatWrapper::from(value))
            }
            HMIRTypeKind::Void | HMIRTypeKind::Unreachable => HMIRImmValue::Unit,
            HMIRTypeKind::Array { element, length } => {
                let stride = layout(self.env, element, span)?.size();
                HMIRImmValue::Aggregate {
                    fields: (0..length as usize)
                        .map(|index| self.immediate(element, image, offset + index * stride))
                        .collect::<CHLResult<_>>()?,
                }
            }
            HMIRTypeKind::Struct { fields } => {
                let layouts = self::record_layout(self.env, ty, span)?.0;
                let mut items = Vec::with_capacity(fields.len());
                for (field, layout) in fields.iter().zip(layouts) {
                    let Some(bitfield) = field_bitfield(layout) else {
                        items.push(self.immediate(*field.ty(), image, offset + layout.offset())?);
                        continue;
                    };
                    let (window, bitfield) = bitfield_window(bitfield);
                    let base = offset + layout.offset();
                    let bytes = self.bytes(image, base + window.start..base + window.end)?;
                    let int = types::int_type(self.env, *field.ty());
                    items.push(HMIRImmValue::Int(bits(bytes, int::signed(int), bitfield)));
                }
                HMIRImmValue::Aggregate { fields: items }
            }
            HMIRTypeKind::Union { fields } => {
                let capacity = layout(self.env, ty, span)?.size();
                let mut members = Vec::with_capacity(fields.len());
                for (variant, field) in fields.iter().enumerate() {
                    let size = layout(self.env, *field.ty(), span)?.size();
                    if size <= capacity {
                        members.push((size, variant));
                    }
                }
                members.sort_by_key(|(size, _)| std::cmp::Reverse(*size));
                let mut failure = None;
                for (size, variant) in members {
                    if image.states[offset + size..offset + capacity].contains(&ByteState::Address)
                    {
                        failure.get_or_insert_with(|| fault(Fault::Relocation));
                        continue;
                    }
                    let preserving = std::mem::replace(&mut self.preserving_addresses, true);
                    let result = self.immediate(*fields[variant].ty(), image, offset);
                    self.preserving_addresses = preserving;
                    match result {
                        Ok(payload) => {
                            let payload = Box::new(payload);
                            return Ok(HMIRImmValue::Union { variant, payload });
                        }
                        Err(error) => {
                            failure.get_or_insert(error);
                        }
                    }
                }
                return Err(failure.unwrap_or_else(|| fault(Fault::Uninitialized)));
            }
            HMIRTypeKind::TaggedUnion { fields } => {
                let variant = self.discriminant(ty, image, offset)?;
                let payload = Box::new(self.immediate(*fields[variant].ty(), image, offset)?);
                HMIRImmValue::Union { variant, payload }
            }
            HMIRTypeKind::PointerTo(_) | HMIRTypeKind::ReferenceTo { .. } => {
                match self.pointer(image, offset)? {
                    HMIRValueKind::Immediate(value) => value,
                    _ => return Err(fault(Fault::Nested)),
                }
            }
            _ if types::opaque_slot(self.env, ty) => match self.cell(image, offset, ty)? {
                HMIRValueKind::Immediate(value) => value.clone(),
                _ => return Err(fault(Fault::Nested)),
            },
            _ => return Err(fault(Fault::Opaque)),
        })
    }
}

fn integer(bytes: &[u8], ty: HMIRIntType) -> i128 {
    let mut buffer = [0; 16];
    buffer[..bytes.len()].copy_from_slice(bytes);
    int::normalize(i128::from_le_bytes(buffer), ty)
}
