#[derive(Debug, Clone, Hash, PartialEq, Eq)]
pub struct LMIRType {
    pub kind: LMIRTypeKind,
}

impl LMIRType {
    pub fn unit() -> Self {
        LMIRType {
            kind: LMIRTypeKind::Unit,
        }
    }
    
    pub fn bool() -> Self {
        LMIRType {
            kind: LMIRTypeKind::Integer(LMIRIntegerType::I1),
        }
    }

    pub fn default_pointer() -> Self {
        LMIRType {
            kind: LMIRTypeKind::Pointer {
                nullable: false,
                dereferenceable: 0,
            },
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

    Array {
        element: Box<LMIRType>,
        size: usize,
    },
    Struct {
        name: String,
        fields: Vec<(String, LMIRType)>,
    },
    Union {
        name: String,
        fields: Vec<(String, LMIRType)>,
    },
    
    Unit,
}

impl From<LMIRTypeKind> for LMIRType {
    fn from(kind: LMIRTypeKind) -> Self {
        LMIRType { kind }
    }
}

impl LMIRType {
    pub fn size(&self) -> usize {
        match &self.kind {
            LMIRTypeKind::Opaque { bytes } => *bytes,
            LMIRTypeKind::Integer(_type) => _type.bytes() as usize,
            LMIRTypeKind::Float(_type) => _type.bytes() as usize,
            LMIRTypeKind::Pointer { .. } => 8, // TODO: make this configurable
            LMIRTypeKind::Array { element, size } => element.size() * size,
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

                    current_size += field_size;
                }

                current_size
            }
            LMIRTypeKind::Union { fields, .. } => fields
                .iter()
                .map(|(_, field)| field.size())
                .max()
                .unwrap(),

            LMIRTypeKind::Unit => 0,
        }
    }

    pub fn alignment(&self) -> u8 {
        match &self.kind {
            LMIRTypeKind::Opaque { bytes } => (*bytes).min(8) as u8,
            LMIRTypeKind::Integer(_type) => _type.bytes().min(8),
            LMIRTypeKind::Float(_type) => _type.bytes().min(8),
            LMIRTypeKind::Pointer { .. } => 8, // TODO: make this configurable
            LMIRTypeKind::Array { element, .. } => element.alignment(),
            LMIRTypeKind::Struct { fields, .. } => fields
                .iter()
                .map(|(_, field)| field.alignment())
                .max()
                .unwrap_or(8),
            LMIRTypeKind::Union { fields, .. } => fields
                .iter()
                .map(|(_, field)| field.alignment())
                .max()
                .unwrap_or(8),
            LMIRTypeKind::Unit => 1,
        }
    }

    #[inline]
    pub fn is_void(&self) -> bool {
        matches!(self.kind, LMIRTypeKind::Unit)
    }

    #[inline]
    pub fn is_structure(&self) -> bool {
        matches!(self.kind, LMIRTypeKind::Struct { .. })
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