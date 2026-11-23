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

use super::{Fault, Image};
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

pub(super) fn record_layout(
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
            let bitfield = match field {
                MIRFieldLayout::Bitfield {
                    bit_offset: offset,
                    bit_width: width,
                    ..
                } => Some(HMIRBitfield { offset, width }),
                _ => None,
            };
            Ok((field.offset(), bitfield))
        }
        HMIRTypeKind::Array { element, .. } => {
            Ok((index * layout(env, element, span)?.size(), None))
        }
        _ => Ok((0, None)),
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

pub(super) fn insert(unit: &Image, value: &HMIRValueKind, bitfield: HMIRBitfield) -> Image {
    let HMIRValueKind::Immediate(HMIRImmValue::Int(value)) = value else {
        unreachable!("a bitfield holds an integer")
    };
    let mut image = Image::zeroed(unit.len());
    image.bytes.copy_from_slice(&unit.bytes);
    set_bits(&mut image.bytes, *value, bitfield);
    image
}

// The variant a tagged union at 'offset' holds
pub(super) fn discriminant(
    env: &mut HMIREnvironment,
    ty: HMIRTypeID,
    image: &Image,
    offset: usize,
    span: &TokenRange,
) -> CHLResult<usize> {
    let (variants, payload) = record_layout(env, ty, span)?;
    let tag = offset + payload.size();
    let variant = image.bytes(tag..tag + 1).map_err(|fault| fault.error(span))?[0] as usize;
    match variant < variants.len() {
        true => Ok(variant),
        false => Err(Fault::Discriminant.error(span)),
    }
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

// An address of comptime storage is held as an opaque cell, like any value without a byte representation
pub(super) fn encode(
    env: &mut HMIREnvironment,
    ty: HMIRTypeID,
    value: &HMIRValueKind,
    span: &TokenRange,
) -> CHLResult<Image> {
    let size = layout(env, ty, span)?.size();
    let mut image = Image::zeroed(size);
    match value {
        HMIRValueKind::Immediate(value) => write(env, ty, value, &mut image, 0, span)?,
        value => image.put_cell(0, size, opaque_type(env, ty), value.clone()),
    }
    Ok(image)
}

fn write(
    env: &mut HMIREnvironment,
    ty: HMIRTypeID,
    value: &HMIRImmValue,
    image: &mut Image,
    offset: usize,
    span: &TokenRange,
) -> CHLResult<()> {
    let ty = types::storage(env, ty, span);
    match (env.resolve_kind(ty).clone(), value) {
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
        (_, HMIRImmValue::Unit) | (HMIRTypeKind::PointerTo(_), HMIRImmValue::Int(0)) => {}
        (HMIRTypeKind::Array { element, .. }, HMIRImmValue::Aggregate { fields: items }) => {
            let stride = layout(env, element, span)?.size();
            for (index, item) in items.iter().enumerate() {
                write(env, element, item, image, offset + index * stride, span)?;
            }
        }
        (HMIRTypeKind::Struct { fields }, HMIRImmValue::Aggregate { fields: items }) => {
            let layouts = self::record_layout(env, ty, span)?.0;
            for ((field, layout), item) in fields.iter().zip(layouts).zip(items) {
                let MIRFieldLayout::Bitfield {
                    offset: unit,
                    bit_offset,
                    bit_width,
                    ..
                } = layout
                else {
                    write(
                        env,
                        *field.ty(),
                        item,
                        image,
                        offset + layout.offset(),
                        span,
                    )?;
                    continue;
                };
                let HMIRImmValue::Int(value) = item else {
                    unreachable!("a bitfield holds an integer")
                };
                let (window, bitfield) = bitfield_window(HMIRBitfield {
                    offset: bit_offset,
                    width: bit_width,
                });
                let base = offset + unit;
                set_bits(
                    &mut image.bytes[base + window.start..base + window.end],
                    *value,
                    bitfield,
                );
            }
        }
        (HMIRTypeKind::Union { fields }, HMIRImmValue::Union { variant, payload }) => {
            write(env, *fields[*variant].ty(), payload, image, offset, span)?;
        }
        (HMIRTypeKind::TaggedUnion { fields }, HMIRImmValue::Union { variant, payload }) => {
            write(env, *fields[*variant].ty(), payload, image, offset, span)?;
            let tag = offset + record_layout(env, ty, span)?.1.size();
            image.put(tag, &[*variant as u8]);
        }
        (_, value) => {
            let size = layout(env, ty, span)?.size();
            image.put_cell(offset, size, opaque_type(env, ty), value.clone().into());
        }
    }
    Ok(())
}

pub(super) fn decode(
    env: &mut HMIREnvironment,
    ty: HMIRTypeID,
    image: &Image,
    span: &TokenRange,
) -> CHLResult<HMIRValueKind> {
    let size = layout(env, ty, span)?.size();
    if let Some(value) = image
        .cell(0, size, opaque_type(env, ty))
        .filter(|_| opaque(env, ty))
    {
        return Ok(value.clone());
    }
    immediate(env, ty, image, 0, span).map(HMIRValueKind::Immediate)
}

// A type whose values may be held as cells rather than bytes
fn opaque(env: &HMIREnvironment, ty: HMIRTypeID) -> bool {
    !matches!(
        env.resolve_kind(ty),
        HMIRTypeKind::Int(_)
            | HMIRTypeKind::Float(_)
            | HMIRTypeKind::Void
            | HMIRTypeKind::Unreachable
            | HMIRTypeKind::Array { .. }
            | HMIRTypeKind::Str(Some(_))
            | HMIRTypeKind::Struct { .. }
            | HMIRTypeKind::Union { .. }
            | HMIRTypeKind::TaggedUnion { .. }
    )
}

fn opaque_type(env: &HMIREnvironment, ty: HMIRTypeID) -> Option<HMIRTypeID> {
    types::opaque_slot(env, ty).then(|| env.underlying(ty))
}

fn immediate(
    env: &mut HMIREnvironment,
    ty: HMIRTypeID,
    image: &Image,
    offset: usize,
    span: &TokenRange,
) -> CHLResult<HMIRImmValue> {
    let fault = |fault: Fault| fault.error(span);
    let ty = types::storage(env, ty, span);
    Ok(match env.resolve_kind(ty).clone() {
        HMIRTypeKind::Int(int) => HMIRImmValue::Int(integer(image, offset, int).map_err(fault)?),
        HMIRTypeKind::Float(float) => {
            let bytes = image
                .bytes(offset..offset + float.width().bytes())
                .map_err(fault)?;
            let value = match float.width() {
                HMIRFloatWidth::F32 => f32::from_le_bytes(bytes.try_into().unwrap()) as f64,
                HMIRFloatWidth::F64 => f64::from_le_bytes(bytes.try_into().unwrap()),
            };
            HMIRImmValue::Float(FloatWrapper::from(value))
        }
        HMIRTypeKind::Void | HMIRTypeKind::Unreachable => HMIRImmValue::Unit,
        HMIRTypeKind::Array { element, length } => {
            let stride = layout(env, element, span)?.size();
            HMIRImmValue::Aggregate {
                fields: (0..length as usize)
                    .map(|index| immediate(env, element, image, offset + index * stride, span))
                    .collect::<CHLResult<_>>()?,
            }
        }
        HMIRTypeKind::Struct { fields } => {
            let layouts = self::record_layout(env, ty, span)?.0;
            let mut items = Vec::with_capacity(fields.len());
            for (field, layout) in fields.iter().zip(layouts) {
                let MIRFieldLayout::Bitfield {
                    offset: unit,
                    bit_offset,
                    bit_width,
                    ..
                } = layout
                else {
                    items.push(immediate(
                        env,
                        *field.ty(),
                        image,
                        offset + layout.offset(),
                        span,
                    )?);
                    continue;
                };
                let (window, bitfield) = bitfield_window(HMIRBitfield {
                    offset: bit_offset,
                    width: bit_width,
                });
                let base = offset + unit;
                let bytes = image
                    .bytes(base + window.start..base + window.end)
                    .map_err(fault)?;
                let int = types::int_type(env, *field.ty());
                items.push(HMIRImmValue::Int(bits(bytes, int::signed(int), bitfield)));
            }
            HMIRImmValue::Aggregate { fields: items }
        }
        HMIRTypeKind::Union { fields } => {
            let mut members = Vec::with_capacity(fields.len());
            for (member, field) in fields.iter().enumerate() {
                members.push((layout(env, *field.ty(), span)?.size(), member));
            }
            members.sort_by_key(|(size, _)| std::cmp::Reverse(*size));
            let mut failure = None;
            for (_, variant) in members {
                match immediate(env, *fields[variant].ty(), image, offset, span) {
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
            let variant = discriminant(env, ty, image, offset, span)?;
            let payload = Box::new(immediate(env, *fields[variant].ty(), image, offset, span)?);
            HMIRImmValue::Union { variant, payload }
        }
        kind => {
            let size = layout(env, ty, span)?.size();
            if let Some(value) = image.cell(offset, size, opaque_type(env, ty)) {
                return match value {
                    HMIRValueKind::Immediate(value) => Ok(value.clone()),
                    _ => Err(fault(Fault::Nested)),
                };
            }
            if types::opaque_slot(env, ty) {
                return Err(fault(Fault::Uninitialized));
            }
            let bytes = image.bytes(offset..offset + size).map_err(fault)?;
            match kind {
                HMIRTypeKind::PointerTo(_) if bytes.iter().all(|byte| *byte == 0) => {
                    HMIRImmValue::Int(0)
                }
                _ => return Err(fault(Fault::Forged)),
            }
        }
    })
}

fn integer(image: &Image, offset: usize, ty: HMIRIntType) -> Result<i128, Fault> {
    let bytes = image.bytes(offset..offset + ty.width().bytes())?;
    let mut buffer = [0; 16];
    buffer[..bytes.len()].copy_from_slice(bytes);
    Ok(int::normalize(i128::from_le_bytes(buffer), ty))
}
