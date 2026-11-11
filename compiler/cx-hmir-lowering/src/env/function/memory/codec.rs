use cx_hmir::ty::{
    HMIRTypeID, HMIRTypeKind,
    aggregate::{HMIRBitfield, HMIRFieldDef},
    context::HMIRTypeContext,
    intrinsic::{HMIRFloatWidth, HMIRIntType},
};
use cx_log::{CXResult, catalogue::mir};
use cx_mir::ty::layout::{
    MIRFieldLayout, MIRTypeLayout, calculate_field_layouts, calculate_type_layout,
};
use cx_tokens::TokenRange;
use cx_util::unsafe_float::FloatWrapper;

use super::{Fault, Image};
use crate::{
    emit::comptime::int,
    env::HMIREnvironment,
    eval::types,
    log::hmir_error,
    lower::types::lower_type,
    value::{HMIRValueKind, immediate::HMIRImmediate},
};

pub(crate) fn layout(
    env: &mut HMIREnvironment,
    ty: HMIRTypeID,
    span: &TokenRange,
) -> CXResult<MIRTypeLayout> {
    match env.resolve_kind(ty).clone() {
        _ if types::comptime_only(env, ty) => {
            let architecture = env.architecture();
            Ok(MIRTypeLayout::new(
                architecture.pointer_size(),
                architecture.pointer_alignment(),
            ))
        }
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
        _ => {
            let lowered = lower_type(env, ty, span)?;
            Ok(calculate_type_layout(env.module(), lowered))
        }
    }
}

fn fields(
    env: &mut HMIREnvironment,
    ty: HMIRTypeID,
    span: &TokenRange,
) -> CXResult<Vec<MIRFieldLayout>> {
    let lowered = lower_type(env, ty, span)?;
    Ok(calculate_field_layouts(env.module(), lowered).expect("an aggregate has field layouts"))
}

pub(crate) fn field_offset(
    env: &mut HMIREnvironment,
    ty: HMIRTypeID,
    index: usize,
    span: &TokenRange,
) -> CXResult<usize> {
    match env.resolve_kind(ty) {
        HMIRTypeKind::Struct { .. } => Ok(fields(env, ty, span)?.swap_remove(index).offset()),
        HMIRTypeKind::Array { element, .. } => {
            let element = *element;
            Ok(index * layout(env, element, span)?.size())
        }
        _ => Ok(0),
    }
}

pub(crate) fn placement(
    env: &mut HMIREnvironment,
    ty: HMIRTypeID,
    index: usize,
    span: &TokenRange,
) -> CXResult<Option<HMIRBitfield>> {
    let declared = match env.resolve_kind(ty) {
        HMIRTypeKind::Struct { fields } | HMIRTypeKind::Union { fields } => {
            fields.get(index).and_then(HMIRFieldDef::bit_width)
        }
        _ => None,
    };
    if declared.is_none() {
        return Ok(None);
    }
    let MIRFieldLayout::Bitfield {
        bit_offset,
        bit_width,
        ..
    } = fields(env, ty, span)?.swap_remove(index)
    else {
        unreachable!("a declared bitfield is laid out as one")
    };
    Ok(Some(HMIRBitfield {
        offset: bit_offset,
        width: bit_width,
    }))
}

pub(super) fn extract(
    env: &HMIREnvironment,
    ty: HMIRTypeID,
    unit: &Image,
    bitfield: HMIRBitfield,
    span: &TokenRange,
) -> CXResult<HMIRValueKind> {
    let int = types::int_type(env, ty);
    let word = unit
        .bytes(0..unit.len())
        .map_err(|fault| fault.error(span))?;
    Ok(HMIRImmediate::Int(bits(word, int::signed(int), bitfield)).into())
}

pub(super) fn insert(unit: &Image, value: &HMIRValueKind, bitfield: HMIRBitfield) -> Image {
    let HMIRValueKind::Immediate(HMIRImmediate::Int(value)) = value else {
        unreachable!("a bitfield holds an integer")
    };
    let mut image = Image::zeroed(unit.len());
    image.bytes.copy_from_slice(&unit.bytes);
    set_bits(&mut image.bytes, *value, bitfield);
    image
}

fn tag_offset(
    env: &mut HMIREnvironment,
    variants: &[HMIRFieldDef<HMIRTypeID>],
    span: &TokenRange,
) -> CXResult<usize> {
    let (mut size, mut alignment) = (0, 1);
    for variant in variants {
        let layout = layout(env, *variant.ty(), span)?;
        size = size.max(layout.size());
        alignment = alignment.max(layout.alignment());
    }
    Ok(size.div_ceil(alignment) * alignment)
}

// The variant a tagged union at 'offset' holds
pub(super) fn discriminant(
    env: &mut HMIREnvironment,
    variants: &[HMIRFieldDef<HMIRTypeID>],
    image: &Image,
    offset: usize,
    span: &TokenRange,
) -> CXResult<usize> {
    let tag = offset + tag_offset(env, variants, span)?;
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
    let (word, mask) = word(bytes, bitfield);
    let bits = ((word & mask) >> bitfield.offset) as i128;
    let shift = 128 - bitfield.width;
    match signed {
        true => bits << shift >> shift,
        false => bits,
    }
}

fn set_bits(bytes: &mut [u8], value: i128, bitfield: HMIRBitfield) {
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
) -> CXResult<Image> {
    let size = layout(env, ty, span)?.size();
    let mut image = Image::zeroed(size);
    match value {
        HMIRValueKind::Immediate(value) => write(env, ty, value, &mut image, 0, span)?,
        value => image.put_cell(0, size, value.clone()),
    }
    Ok(image)
}

fn write(
    env: &mut HMIREnvironment,
    ty: HMIRTypeID,
    value: &HMIRImmediate,
    image: &mut Image,
    offset: usize,
    span: &TokenRange,
) -> CXResult<()> {
    match (env.resolve_kind(ty).clone(), value) {
        (HMIRTypeKind::Int(int), HMIRImmediate::Int(value)) => {
            image.put(offset, &value.to_le_bytes()[..int.width().bytes()]);
        }
        (HMIRTypeKind::Float(float), HMIRImmediate::Float(value)) => {
            let value = f64::from(value);
            match float.width() {
                HMIRFloatWidth::F32 => image.put(offset, &(value as f32).to_le_bytes()),
                HMIRFloatWidth::F64 => image.put(offset, &value.to_le_bytes()),
            }
        }
        (_, HMIRImmediate::Unit) | (HMIRTypeKind::PointerTo(_), HMIRImmediate::Int(0)) => {}
        (HMIRTypeKind::Array { element, .. }, HMIRImmediate::Aggregate { fields: items }) => {
            let stride = layout(env, element, span)?.size();
            for (index, item) in items.iter().enumerate() {
                write(env, element, item, image, offset + index * stride, span)?;
            }
        }
        (HMIRTypeKind::Struct { fields }, HMIRImmediate::Aggregate { fields: items }) => {
            let layouts = self::fields(env, ty, span)?;
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
                let (HMIRTypeKind::Int(int), HMIRImmediate::Int(value)) =
                    (env.resolve_kind(*field.ty()), item)
                else {
                    unreachable!("a bitfield holds an integer")
                };
                let range = offset + unit..offset + unit + int.width().bytes();
                let bitfield = HMIRBitfield {
                    offset: bit_offset,
                    width: bit_width,
                };
                set_bits(&mut image.bytes[range], *value, bitfield);
            }
        }
        (HMIRTypeKind::Union { fields }, HMIRImmediate::Union { variant, payload }) => {
            write(env, *fields[*variant].ty(), payload, image, offset, span)?;
        }
        (HMIRTypeKind::TaggedUnion { fields }, HMIRImmediate::Union { variant, payload }) => {
            write(env, *fields[*variant].ty(), payload, image, offset, span)?;
            let tag = offset + tag_offset(env, &fields, span)?;
            image.put(tag, &[*variant as u8]);
        }
        (_, value) => {
            let size = layout(env, ty, span)?.size();
            image.put_cell(offset, size, value.clone().into());
        }
    }
    Ok(())
}

pub(super) fn decode(
    env: &mut HMIREnvironment,
    ty: HMIRTypeID,
    image: &Image,
    span: &TokenRange,
) -> CXResult<HMIRValueKind> {
    let size = layout(env, ty, span)?.size();
    if let Some(value) = image.cell(0, size).filter(|_| opaque(env, ty)) {
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
            | HMIRTypeKind::Struct { .. }
            | HMIRTypeKind::Union { .. }
            | HMIRTypeKind::TaggedUnion { .. }
    )
}

fn cell(image: &Image, offset: usize, size: usize) -> Option<Result<HMIRImmediate, Fault>> {
    image.cell(offset, size).map(|value| match value {
        HMIRValueKind::Immediate(value) => Ok(value.clone()),
        _ => Err(Fault::Nested),
    })
}

fn immediate(
    env: &mut HMIREnvironment,
    ty: HMIRTypeID,
    image: &Image,
    offset: usize,
    span: &TokenRange,
) -> CXResult<HMIRImmediate> {
    let fault = |fault: Fault| fault.error(span);
    Ok(match env.resolve_kind(ty).clone() {
        HMIRTypeKind::Int(int) => HMIRImmediate::Int(integer(image, offset, int).map_err(fault)?),
        HMIRTypeKind::Float(float) => {
            let bytes = image
                .bytes(offset..offset + float.width().bytes())
                .map_err(fault)?;
            let value = match float.width() {
                HMIRFloatWidth::F32 => f32::from_le_bytes(bytes.try_into().unwrap()) as f64,
                HMIRFloatWidth::F64 => f64::from_le_bytes(bytes.try_into().unwrap()),
            };
            HMIRImmediate::Float(FloatWrapper::from(value))
        }
        HMIRTypeKind::Void | HMIRTypeKind::Unreachable => HMIRImmediate::Unit,
        HMIRTypeKind::Array { element, length } => {
            let stride = layout(env, element, span)?.size();
            HMIRImmediate::Aggregate {
                fields: (0..length as usize)
                    .map(|index| immediate(env, element, image, offset + index * stride, span))
                    .collect::<CXResult<_>>()?,
            }
        }
        HMIRTypeKind::Struct { fields } => {
            let layouts = self::fields(env, ty, span)?;
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
                let int = types::int_type(env, *field.ty());
                let range = offset + unit..offset + unit + int.width().bytes();
                let bitfield = HMIRBitfield {
                    offset: bit_offset,
                    width: bit_width,
                };
                let bytes = image.bytes(range).map_err(fault)?;
                items.push(HMIRImmediate::Int(bits(bytes, int::signed(int), bitfield)));
            }
            HMIRImmediate::Aggregate { fields: items }
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
                        return Ok(HMIRImmediate::Union { variant, payload });
                    }
                    Err(error) => {
                        failure.get_or_insert(error);
                    }
                }
            }
            return Err(failure.unwrap_or_else(|| fault(Fault::Uninitialized)));
        }
        HMIRTypeKind::TaggedUnion { fields } => {
            let variant = discriminant(env, &fields, image, offset, span)?;
            let payload = Box::new(immediate(env, *fields[variant].ty(), image, offset, span)?);
            HMIRImmediate::Union { variant, payload }
        }
        kind => {
            let size = layout(env, ty, span)?.size();
            if let Some(value) = cell(image, offset, size) {
                return value.map_err(fault);
            }
            let bytes = image.bytes(offset..offset + size).map_err(fault)?;
            match kind {
                HMIRTypeKind::PointerTo(_) if bytes.iter().all(|byte| *byte == 0) => {
                    HMIRImmediate::Int(0)
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
