#[derive(Debug, Clone, Hash, PartialEq, Eq)]
pub struct BCType {
    pub kind: BCTypeKind,
}

impl BCType {
    pub fn unit() -> Self {
        BCType {
            kind: BCTypeKind::Unit,
        }
    }

    pub fn default_pointer() -> Self {
        BCType {
            kind: BCTypeKind::Pointer {
                nullable: false,
                dereferenceable: 0,
            },
        }
    }
}

#[derive(Debug, Clone, Copy, Hash, PartialEq, Eq)]
pub enum BCIntegerType {
    I8,
    I16,
    I32,
    I64,
    I128,
}

#[derive(Debug, Clone, Copy, Hash, PartialEq, Eq)]
pub enum BCFloatType {
    F32,
    F64,
}

#[derive(Debug, Clone, Hash, PartialEq, Eq)]
pub enum BCTypeKind {
    Opaque {
        bytes: usize,
    },
    Bool,
    
    Integer(BCIntegerType),
    Float(BCFloatType),
    
    Pointer {
        nullable: bool,
        dereferenceable: u32,
    },

    Array {
        element: Box<BCType>,
        size: usize,
    },
    Struct {
        name: String,
        fields: Vec<(String, BCType)>,
    },
    Union {
        name: String,
        fields: Vec<(String, BCType)>,
    },
    
    Unit,
}

impl From<BCTypeKind> for BCType {
    fn from(kind: BCTypeKind) -> Self {
        BCType { kind }
    }
}

impl BCType {
    pub fn size(&self) -> usize {
        match &self.kind {
            BCTypeKind::Opaque { bytes } => *bytes,
            BCTypeKind::Integer(_type) => _type.bytes() as usize,
            BCTypeKind::Float(_type) => _type.bytes() as usize,
            BCTypeKind::Pointer { .. } => 8, // TODO: make this configurable
            BCTypeKind::Array { element, size } => element.size() * size,
            BCTypeKind::Struct { fields, .. } => {
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
            BCTypeKind::Union { fields, .. } => fields
                .iter()
                .map(|(_, field)| field.size())
                .max()
                .unwrap(),

            BCTypeKind::Bool => 1,
            BCTypeKind::Unit => 0,
        }
    }

    pub fn alignment(&self) -> u8 {
        match &self.kind {
            BCTypeKind::Opaque { bytes } => (*bytes).min(8) as u8,
            BCTypeKind::Bool => 1,
            BCTypeKind::Integer(_type) => _type.bytes().min(8),
            BCTypeKind::Float(_type) => _type.bytes().min(8),
            BCTypeKind::Pointer { .. } => 8, // TODO: make this configurable
            BCTypeKind::Array { element, .. } => element.alignment(),
            BCTypeKind::Struct { fields, .. } => fields
                .iter()
                .map(|(_, field)| field.alignment())
                .max()
                .unwrap_or(8),
            BCTypeKind::Union { fields, .. } => fields
                .iter()
                .map(|(_, field)| field.alignment())
                .max()
                .unwrap_or(8),
            BCTypeKind::Unit => 1,
        }
    }

    #[inline]
    pub fn is_void(&self) -> bool {
        matches!(self.kind, BCTypeKind::Unit)
    }

    #[inline]
    pub fn is_structure(&self) -> bool {
        matches!(self.kind, BCTypeKind::Struct { .. })
    }
}

impl BCIntegerType {
    pub fn bytes(&self) -> u8 {
        match self {
            BCIntegerType::I8 => 1,
            BCIntegerType::I16 => 2,
            BCIntegerType::I32 => 4,
            BCIntegerType::I64 => 8,
            BCIntegerType::I128 => 16,
        }
    }
}

impl BCFloatType {
    pub fn bytes(&self) -> u8 {
        match self {
            BCFloatType::F32 => 4,
            BCFloatType::F64 => 8,
        }
    }
}