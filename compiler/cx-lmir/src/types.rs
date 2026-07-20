use cx_target::ArchitectureConfig;

#[derive(Debug, Clone, Hash, PartialEq, Eq)]
pub struct LMIRType {
    pub kind: LMIRTypeKind,
    pub alignment: u8,
}

impl LMIRType {
    pub fn new(kind: LMIRTypeKind, alignment: u8) -> Self {
        LMIRType { kind, alignment }
    }

    pub fn with_implicit_abi(target: &ArchitectureConfig, kind: LMIRTypeKind) -> Self {
        let alignment = usize::from(kind.implicit_size())
            .next_power_of_two()
            .min(target.pointer_size()) as u8;

        LMIRType { kind, alignment }
    }

    pub fn unit() -> Self {
        LMIRType {
            kind: LMIRTypeKind::Unit,
            alignment: 1,
        }
    }

    pub fn bool() -> Self {
        LMIRType {
            kind: LMIRTypeKind::Integer(LMIRIntegerType::I1),
            alignment: 1,
        }
    }

    pub fn default_pointer() -> Self {
        LMIRType {
            kind: LMIRTypeKind::Pointer {
                nullable: false,
                dereferenceable: 0,
            },
            alignment: 8,
        }
    }
}

#[derive(Debug, Clone, Copy, Hash, PartialEq, Eq)]
pub enum LMIRIntegerType {
    I1,
    I8,
    I16,
    I32,
    I64,
    I128,
}

#[derive(Debug, Clone, Copy, Hash, PartialEq, Eq)]
pub enum LMIRFloatType {
    F32,
    F64,
}

#[derive(Debug, Clone, Hash, PartialEq, Eq)]
pub enum LMIRTypeKind {
    Opaque {
        bytes: usize,
    },

    Integer(LMIRIntegerType),
    Float(LMIRFloatType),

    Pointer {
        nullable: bool,
        dereferenceable: u32,
    },

    Vector {
        element: LMIRFloatType,
        count: usize,
    },

    Array {
        element: Box<LMIRType>,
        size: usize,
    },
    Struct {
        name: String,
        fields: Vec<(String, LMIRType)>,
    },

    Unit,
}

#[derive(Debug, Clone, Copy, PartialEq, Eq, PartialOrd, Ord)]
pub struct TypeSize(usize);

impl From<TypeSize> for usize {
    fn from(s: TypeSize) -> usize {
        s.0
    }
}

impl From<usize> for TypeSize {
    fn from(s: usize) -> TypeSize {
        TypeSize(s)
    }
}

// impl From<LMIRTypeKind> for LMIRType {
//     fn from(kind: LMIRTypeKind) -> Self {
//         LMIRType {
//             kind,
//             alignment: None,
//         }
//     }
// }

impl LMIRTypeKind {
    pub fn implicit_size(&self) -> TypeSize {
        TypeSize(match &self {
            LMIRTypeKind::Opaque { bytes } => *bytes,
            LMIRTypeKind::Integer(_type) => _type.bytes() as usize,
            LMIRTypeKind::Float(_type) => _type.bytes() as usize,
            LMIRTypeKind::Pointer { .. } => 8, // TODO: make this configurable
            LMIRTypeKind::Vector { element, count } => element.bytes() as usize * count,
            LMIRTypeKind::Array { element, size } => usize::from(element.size()) * size,
            LMIRTypeKind::Struct { fields, .. } => {
                let mut current_size = 0;

                for (_, field_type) in fields {
                    let field_size = field_type.size();
                    let field_alignment = field_type.alignment();

                    // Align current size to the field's alignment
                    if current_size % field_alignment as usize != 0 {
                        current_size +=
                            field_alignment as usize - (current_size % field_alignment as usize);
                    }

                    current_size += usize::from(field_size);
                }

                current_size
            }

            LMIRTypeKind::Unit => 0,
        })
    }
}

impl LMIRType {
    pub fn size(&self) -> TypeSize {
        let mut implicit_size = self.kind.implicit_size();

        if implicit_size.0 % self.alignment as usize != 0 {
            implicit_size.0 +=
                self.alignment as usize - (implicit_size.0 % self.alignment as usize);
        }

        return implicit_size;
    }

    pub fn alignment(&self) -> u8 {
        self.alignment
    }

    #[inline]
    pub fn is_void(&self) -> bool {
        matches!(self.kind, LMIRTypeKind::Unit)
    }

    #[inline]
    pub fn is_structure(&self) -> bool {
        matches!(self.kind, LMIRTypeKind::Struct { .. })
    }

    pub fn is_memory_resident(&self) -> bool {
        match self.kind {
            LMIRTypeKind::Opaque { .. } => true,
            LMIRTypeKind::Integer(_) => false,
            LMIRTypeKind::Float(_) => false,
            LMIRTypeKind::Pointer { .. } => false,
            LMIRTypeKind::Vector { .. } => false,
            LMIRTypeKind::Array { .. } => true,
            LMIRTypeKind::Struct { .. } => true,
            LMIRTypeKind::Unit => false,
        }
    }
}

impl LMIRIntegerType {
    pub fn bytes(&self) -> u8 {
        match self {
            LMIRIntegerType::I1 => 1,
            LMIRIntegerType::I8 => 1,
            LMIRIntegerType::I16 => 2,
            LMIRIntegerType::I32 => 4,
            LMIRIntegerType::I64 => 8,
            LMIRIntegerType::I128 => 16,
        }
    }
}

impl LMIRFloatType {
    pub fn bytes(&self) -> u8 {
        match self {
            LMIRFloatType::F32 => 4,
            LMIRFloatType::F64 => 8,
        }
    }
}
