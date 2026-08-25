use std::collections::HashSet;

use crate::{MIRField, MIRTypeID, MIRTypeKind, ty::interface::MTRegistry};

pub(crate) fn same_type_inner<T: MTRegistry>(
    registry: &T,
    compared: &mut HashSet<(MIRTypeID, MIRTypeID)>,
    left: MIRTypeID,
    right: MIRTypeID,
) -> bool {
    if left == right {
        return true;
    }

    if !compared.insert((left, right)) {
        return true;
    }

    let (Some(left), Some(right)) = (registry.definition(left), registry.definition(right)) else {
        return false;
    };

    left.layout == right.layout && same_kind(registry, compared, &left.kind, &right.kind)
}

fn same_kind<T: MTRegistry>(
    registry: &T,
    compared: &mut HashSet<(MIRTypeID, MIRTypeID)>,
    left: &MIRTypeKind,
    right: &MIRTypeKind,
) -> bool {
    match (left, right) {
        (MIRTypeKind::Void, MIRTypeKind::Void)
        | (MIRTypeKind::Undefined, MIRTypeKind::Undefined)
        | (MIRTypeKind::Str, MIRTypeKind::Str) => true,
        (
            MIRTypeKind::Integer {
                ty: left_ty,
                signed: left_signed,
            },
            MIRTypeKind::Integer {
                ty: right_ty,
                signed: right_signed,
            },
        ) => left_ty == right_ty && left_signed == right_signed,
        (MIRTypeKind::Float { ty: left }, MIRTypeKind::Float { ty: right }) => left == right,
        (MIRTypeKind::Structured { fields: left }, MIRTypeKind::Structured { fields: right })
        | (MIRTypeKind::Union { variants: left }, MIRTypeKind::Union { variants: right })
        | (
            MIRTypeKind::TaggedUnion { variants: left },
            MIRTypeKind::TaggedUnion { variants: right },
        ) => {
            left.len() == right.len()
                && left
                    .iter()
                    .zip(right)
                    .all(|(left, right)| same_field(registry, compared, left, right))
        }
        (MIRTypeKind::PointerTo { inner: left }, MIRTypeKind::PointerTo { inner: right }) => {
            same_type_inner(registry, compared, *left, *right)
        }
        (
            MIRTypeKind::MemoryReference {
                inner: left_inner,
                bitfield: left_bitfield,
            },
            MIRTypeKind::MemoryReference {
                inner: right_inner,
                bitfield: right_bitfield,
            },
        ) => {
            same_type_inner(registry, compared, *left_inner, *right_inner)
                && match (left_bitfield, right_bitfield) {
                    (None, None) => true,
                    (Some(left), Some(right)) => {
                        left.bit_offset == right.bit_offset
                            && left.bit_width == right.bit_width
                            && left.signed == right.signed
                    }
                    _ => false,
                }
        }
        (
            MIRTypeKind::Array {
                length: left_length,
                inner: left_inner,
            },
            MIRTypeKind::Array {
                length: right_length,
                inner: right_inner,
            },
        ) => {
            left_length == right_length
                && same_type_inner(registry, compared, *left_inner, *right_inner)
        }
        (MIRTypeKind::Function { signature: left }, MIRTypeKind::Function { signature: right }) => {
            left.variadic == right.variadic
                && same_type_inner(registry, compared, left.return_type, right.return_type)
                && left.params.len() == right.params.len()
                && left
                    .params
                    .iter()
                    .zip(&right.params)
                    .all(|(left, right)| same_type_inner(registry, compared, *left, *right))
        }
        (
            MIRTypeKind::Opaque {
                size: left_size,
                alignment: left_alignment,
            },
            MIRTypeKind::Opaque {
                size: right_size,
                alignment: right_alignment,
            },
        ) => left_size == right_size && left_alignment == right_alignment,
        _ => false,
    }
}

fn same_field<T: MTRegistry>(
    registry: &T,
    compared: &mut HashSet<(MIRTypeID, MIRTypeID)>,
    left: &MIRField,
    right: &MIRField,
) -> bool {
    match (left, right) {
        (
            MIRField::Bitfield {
                integer_type_id: left_itd,
                width: left_width,
                ..
            },
            MIRField::Bitfield {
                integer_type_id: right_itd,
                width: right_width,
                ..
            },
        ) => {
            left_width == right_width && same_type_inner(registry, compared, *left_itd, *right_itd)
        }

        (
            MIRField::Standard {
                type_id: left_type, ..
            },
            MIRField::Standard {
                type_id: right_type,
                ..
            },
        ) => same_type_inner(registry, compared, *left_type, *right_type),

        _ => false,
    }
}
