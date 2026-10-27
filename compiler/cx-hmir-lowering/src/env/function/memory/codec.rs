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
    env::HMIREnvironment,
    eval::types,
    lower::types::lower_type,
    log::hmir_error,
    value::{constant::HMIRConstant, scalar::int},
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
        HMIRTypeKind::Array {
            element,
            length: Some(length),
        } => {
            let element = layout(env, element, span)?;
            let size = usize::try_from(length)
                .ok()
                .and_then(|length| element.size().checked_mul(length))
                .ok_or_else(|| hmir_error(span, &mir::SIZE_OVERFLOW, ()))?;
            Ok(MIRTypeLayout::new(size, element.alignment()))
        }
        HMIRTypeKind::Array { length: None, .. } => Err(hmir_error(
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
) -> CXResult<HMIRConstant> {
    let HMIRTypeKind::Int(int) = *env.resolve_kind(ty) else {
        unreachable!("a bitfield holds an integer")
    };
    let word = unit.bytes(0..unit.len()).map_err(|fault| fault.error(span))?;
    Ok(HMIRConstant::Int(bits(word, int::signed(int), bitfield)))
}

pub(super) fn insert(unit: &Image, value: &HMIRConstant, bitfield: HMIRBitfield) -> Image {
    let HMIRConstant::Int(value) = value else {
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

pub(super) fn encode(
    env: &mut HMIREnvironment,
    ty: HMIRTypeID,
    value: &HMIRConstant,
    span: &TokenRange,
) -> CXResult<Image> {
    let mut image = Image::zeroed(layout(env, ty, span)?.size());
    write(env, ty, value, &mut image, 0, span)?;
    Ok(image)
}

fn write(
    env: &mut HMIREnvironment,
    ty: HMIRTypeID,
    value: &HMIRConstant,
    image: &mut Image,
    offset: usize,
    span: &TokenRange,
) -> CXResult<()> {
    match (env.resolve_kind(ty).clone(), value) {
        (HMIRTypeKind::Int(int), HMIRConstant::Int(value)) => {
            image.put(offset, &value.to_le_bytes()[..int.width().bytes()]);
        }
        (HMIRTypeKind::Float(float), HMIRConstant::Float(value)) => {
            let value = f64::from(value);
            match float.width() {
                HMIRFloatWidth::F32 => image.put(offset, &(value as f32).to_le_bytes()),
                HMIRFloatWidth::F64 => image.put(offset, &value.to_le_bytes()),
            }
        }
        (_, HMIRConstant::Null | HMIRConstant::Unit) => {}
        (HMIRTypeKind::Array { element, .. }, HMIRConstant::Aggregate(items)) => {
            let stride = layout(env, element, span)?.size();
            for (index, item) in items.iter().enumerate() {
                write(env, element, item, image, offset + index * stride, span)?;
            }
        }
        (HMIRTypeKind::Struct { fields }, HMIRConstant::Aggregate(items)) => {
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
                let (HMIRTypeKind::Int(int), HMIRConstant::Int(value)) =
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
        (HMIRTypeKind::Union { fields }, HMIRConstant::Union { member, value }) => {
            write(env, *fields[*member].ty(), value, image, offset, span)?;
        }
        (HMIRTypeKind::TaggedUnion { fields }, HMIRConstant::TaggedUnion { variant, payload }) => {
            write(env, *fields[*variant].ty(), payload, image, offset, span)?;
            let tag = offset + tag_offset(env, &fields, span)?;
            image.put(tag, &[*variant as u8]);
        }
        (_, value) => {
            let size = layout(env, ty, span)?.size();
            image.put_cell(offset, size, value.clone());
        }
    }
    Ok(())
}

pub(super) fn decode(
    env: &mut HMIREnvironment,
    ty: HMIRTypeID,
    image: &Image,
    offset: usize,
    span: &TokenRange,
) -> CXResult<HMIRConstant> {
    let fault = |fault: Fault| fault.error(span);
    Ok(match env.resolve_kind(ty).clone() {
        HMIRTypeKind::Int(int) => HMIRConstant::Int(integer(image, offset, int).map_err(fault)?),
        HMIRTypeKind::Float(float) => {
            let bytes = image
                .bytes(offset..offset + float.width().bytes())
                .map_err(fault)?;
            let value = match float.width() {
                HMIRFloatWidth::F32 => f32::from_le_bytes(bytes.try_into().unwrap()) as f64,
                HMIRFloatWidth::F64 => f64::from_le_bytes(bytes.try_into().unwrap()),
            };
            HMIRConstant::Float(FloatWrapper::from(value))
        }
        HMIRTypeKind::Void | HMIRTypeKind::Unreachable => HMIRConstant::Unit,
        HMIRTypeKind::Array {
            element,
            length: Some(length),
        } => {
            let stride = layout(env, element, span)?.size();
            HMIRConstant::Aggregate(
                (0..length as usize)
                    .map(|index| decode(env, element, image, offset + index * stride, span))
                    .collect::<CXResult<_>>()?,
            )
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
                    items.push(decode(
                        env,
                        *field.ty(),
                        image,
                        offset + layout.offset(),
                        span,
                    )?);
                    continue;
                };
                let HMIRTypeKind::Int(int) = *env.resolve_kind(*field.ty()) else {
                    unreachable!("a bitfield holds an integer")
                };
                let range = offset + unit..offset + unit + int.width().bytes();
                let bitfield = HMIRBitfield {
                    offset: bit_offset,
                    width: bit_width,
                };
                let bytes = image.bytes(range).map_err(fault)?;
                items.push(HMIRConstant::Int(bits(bytes, int::signed(int), bitfield)));
            }
            HMIRConstant::Aggregate(items)
        }
        HMIRTypeKind::Union { fields } => {
            let mut members = Vec::with_capacity(fields.len());
            for (member, field) in fields.iter().enumerate() {
                members.push((layout(env, *field.ty(), span)?.size(), member));
            }
            members.sort_by_key(|(size, _)| std::cmp::Reverse(*size));
            let mut failure = None;
            for (_, member) in members {
                match decode(env, *fields[member].ty(), image, offset, span) {
                    Ok(value) => {
                        let value = Box::new(value);
                        return Ok(HMIRConstant::Union { member, value });
                    }
                    Err(error) => {
                        failure.get_or_insert(error);
                    }
                }
            }
            return Err(failure.unwrap_or_else(|| fault(Fault::Uninitialized)));
        }
        HMIRTypeKind::TaggedUnion { fields } => {
            let tag = offset + tag_offset(env, &fields, span)?;
            let variant = image.bytes(tag..tag + 1).map_err(fault)?[0] as usize;
            let Some(field) = fields.get(variant) else {
                return Err(fault(Fault::Discriminant));
            };
            let payload = Box::new(decode(env, *field.ty(), image, offset, span)?);
            HMIRConstant::TaggedUnion { variant, payload }
        }
        kind => {
            let size = layout(env, ty, span)?.size();
            if let Some(value) = image.cell(offset, size) {
                return Ok(value.clone());
            }
            let bytes = image.bytes(offset..offset + size).map_err(fault)?;
            match kind {
                HMIRTypeKind::PointerTo(_) if bytes.iter().all(|byte| *byte == 0) => {
                    HMIRConstant::Null
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
