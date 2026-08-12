use cx_thir::thir::r#type::{THIRField, THIRType, THIRTypeKind};

/// A semantic type carried into MIR from THIR.
///
/// MIR deliberately keeps the THIR type instead of introducing target layout or
/// calling-convention details. Those belong to later lowering stages.
#[derive(Debug, Clone)]
pub struct MIRType(pub THIRType);

impl MIRType {
    pub fn new(ty: THIRType) -> Self {
        Self(ty)
    }

    pub fn from_kind(kind: THIRTypeKind) -> Self {
        Self(kind.into())
    }

    pub fn as_thir(&self) -> &THIRType {
        &self.0
    }

    pub fn as_thir_mut(&mut self) -> &mut THIRType {
        &mut self.0
    }

    pub fn into_thir(self) -> THIRType {
        self.0
    }

    /// Compares the semantic identity available in ABI-agnostic MIR.
    ///
    /// Named types retain nominal identity. Anonymous types compare their
    /// structure and the THIR type IDs referenced by that structure.
    pub fn same_as(&self, other: &Self) -> bool {
        match (
            self.0.strong_identifier.as_deref(),
            other.0.strong_identifier.as_deref(),
        ) {
            (Some(left), Some(right)) => return left == right,
            (Some(_), None) | (None, Some(_)) => return false,
            (None, None) => {}
        }
        same_kind(&self.0.kind, &other.0.kind)
    }
}

impl Default for MIRType {
    fn default() -> Self {
        Self(THIRType::default())
    }
}

impl From<THIRType> for MIRType {
    fn from(value: THIRType) -> Self {
        Self(value)
    }
}

impl From<MIRType> for THIRType {
    fn from(value: MIRType) -> Self {
        value.0
    }
}

fn same_kind(left: &THIRTypeKind, right: &THIRTypeKind) -> bool {
    match (left, right) {
        (THIRTypeKind::Unit, THIRTypeKind::Unit)
        | (THIRTypeKind::Undefined, THIRTypeKind::Undefined)
        | (THIRTypeKind::Str, THIRTypeKind::Str) => true,
        (
            THIRTypeKind::Integer {
                _type: left_type,
                signed: left_signed,
            },
            THIRTypeKind::Integer {
                _type: right_type,
                signed: right_signed,
            },
        ) => left_type == right_type && left_signed == right_signed,
        (THIRTypeKind::Float { _type: left }, THIRTypeKind::Float { _type: right }) => {
            left == right
        }
        (THIRTypeKind::Structured { fields: left }, THIRTypeKind::Structured { fields: right })
        | (THIRTypeKind::Union { variants: left }, THIRTypeKind::Union { variants: right })
        | (
            THIRTypeKind::TaggedUnion { variants: left },
            THIRTypeKind::TaggedUnion { variants: right },
        ) => same_fields(left, right),
        (
            THIRTypeKind::PointerTo { inner_type: left },
            THIRTypeKind::PointerTo { inner_type: right },
        ) => left == right,
        (
            THIRTypeKind::MemoryReference {
                inner_type: left_type,
                bitfield: left_bitfield,
            },
            THIRTypeKind::MemoryReference {
                inner_type: right_type,
                bitfield: right_bitfield,
            },
        ) => left_type == right_type && left_bitfield == right_bitfield,
        (
            THIRTypeKind::Array {
                length: left_length,
                inner_type: left_type,
            },
            THIRTypeKind::Array {
                length: right_length,
                inner_type: right_type,
            },
        ) => left_length == right_length && left_type == right_type,
        (
            THIRTypeKind::Function { signature: left },
            THIRTypeKind::Function { signature: right },
        ) => {
            left.var_args == right.var_args
                && MIRType::new(left.return_type.clone())
                    .same_as(&MIRType::new(right.return_type.clone()))
                && left.params.len() == right.params.len()
                && left.params.iter().zip(&right.params).all(|(left, right)| {
                    MIRType::new(left._type.clone()).same_as(&MIRType::new(right._type.clone()))
                })
        }
        (
            THIRTypeKind::Opaque {
                size: left_size,
                alignment: left_alignment,
            },
            THIRTypeKind::Opaque {
                size: right_size,
                alignment: right_alignment,
            },
        ) => left_size == right_size && left_alignment == right_alignment,
        _ => false,
    }
}

fn same_fields(left: &[THIRField], right: &[THIRField]) -> bool {
    left.len() == right.len()
        && left
            .iter()
            .zip(right)
            .all(|(left, right)| match (left, right) {
                (
                    THIRField::Standard {
                        name: left_name,
                        type_id: left_type,
                    },
                    THIRField::Standard {
                        name: right_name,
                        type_id: right_type,
                    },
                ) => left_name == right_name && left_type == right_type,
                (
                    THIRField::Bitfield {
                        name: left_name,
                        integer_type_id: left_type,
                        width: left_width,
                    },
                    THIRField::Bitfield {
                        name: right_name,
                        integer_type_id: right_type,
                        width: right_width,
                    },
                ) => {
                    left_name == right_name && left_type == right_type && left_width == right_width
                }
                _ => false,
            })
}
