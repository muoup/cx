fn same_kind(
    left: &MIRTypeKind,
    right: &MIRTypeKind,
    mut same_id: impl FnMut(MIRTypeID, MIRTypeID) -> bool,
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
        ) => same_fields(left, right, &mut same_id),
        (MIRTypeKind::PointerTo { inner: left }, MIRTypeKind::PointerTo { inner: right }) => {
            same_id(*left, *right)
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
            same_id(*left_inner, *right_inner)
                && same_bitfield(
                    left_bitfield.as_ref(),
                    right_bitfield.as_ref(),
                    &mut same_id,
                )
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
        ) => left_length == right_length && same_id(*left_inner, *right_inner),
        (MIRTypeKind::Function { signature: left }, MIRTypeKind::Function { signature: right }) => {
            same_function_type(left, right, &mut same_id)
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

fn same_bitfield(
    left: Option<&MIRBitfieldAccess>,
    right: Option<&MIRBitfieldAccess>,
    same_id: &mut impl FnMut(MIRTypeID, MIRTypeID) -> bool,
) -> bool {
    match (left, right) {
        (None, None) => true,
        (Some(left), Some(right)) => {
            same_id(left.storage_type, right.storage_type)
                && left.bit_offset == right.bit_offset
                && left.bit_width == right.bit_width
                && left.signed == right.signed
        }
        _ => false,
    }
}

fn same_function_type(
    left: &MIRFunctionType,
    right: &MIRFunctionType,
    same_id: &mut impl FnMut(MIRTypeID, MIRTypeID) -> bool,
) -> bool {
    left.variadic == right.variadic
        && same_id(left.return_type, right.return_type)
        && left.params.len() == right.params.len()
        && left
            .params
            .iter()
            .zip(&right.params)
            .all(|(left, right)| same_id(*left, *right))
}

fn same_fields(
    left: &[MIRField],
    right: &[MIRField],
    same_id: &mut impl FnMut(MIRTypeID, MIRTypeID) -> bool,
) -> bool {
    left.len() == right.len()
        && left
            .iter()
            .zip(right)
            .all(|(left, right)| match (left, right) {
                (
                    MIRField::Standard {
                        name: left_name,
                        type_id: left,
                    },
                    MIRField::Standard {
                        name: right_name,
                        type_id: right,
                    },
                ) => left_name == right_name && same_id(*left, *right),
                (
                    MIRField::Bitfield {
                        name: left_name,
                        integer_type_id: left_type,
                        width: left_width,
                    },
                    MIRField::Bitfield {
                        name: right_name,
                        integer_type_id: right_type,
                        width: right_width,
                    },
                ) => {
                    left_name == right_name
                        && left_width == right_width
                        && same_id(*left_type, *right_type)
                }
                _ => false,
            })
}