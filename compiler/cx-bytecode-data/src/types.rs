use crate::BCValue;

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

#[derive(Debug, Clone, Hash, PartialEq, Eq)]
pub enum BCIntegerType {
    I8,
    I16,
    I32,
    I64,
    I128,
}

#[derive(Debug, Clone, Hash, PartialEq, Eq)]
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

    VariableSized {
        size: Box<BCValue>,
        alignment: u8,
    },

    Unit,
}

#[derive(Debug, Clone, PartialEq, Eq)]
pub enum MIRTypeSize {
    Fixed(usize),
    Variable(BCValue),
}

impl MIRTypeSize {
    pub fn assert_fixed(self, msg: &str) -> usize {
        match self {
            MIRTypeSize::Fixed(size) => size,
            MIRTypeSize::Variable(_) => panic!("{msg}: expected fixed size, got variable"),
        }
    }
}

impl From<BCTypeKind> for BCType {
    fn from(kind: BCTypeKind) -> Self {
        BCType { kind }
    }
}

impl BCType {
    pub fn size(&self) -> MIRTypeSize {
        match &self.kind {
            BCTypeKind::VariableSized { size, .. } => MIRTypeSize::Variable(*size.clone()),
            _ => MIRTypeSize::Fixed(self.fixed_size()),
        }
    }

    pub fn fixed_size(&self) -> usize {
        match &self.kind {
            BCTypeKind::Opaque { bytes } => *bytes,
            BCTypeKind::Integer(_type) => _type.bytes() as usize,
            BCTypeKind::Float(_type) => _type.bytes() as usize,
            BCTypeKind::Pointer { .. } => 8, // TODO: make this configurable
            BCTypeKind::Array { element, size } => element.fixed_size() * size,
            BCTypeKind::Struct { fields, .. } => {
                let mut current_size = 0;

                for (_, field_type) in fields {
                    let field_size = field_type.fixed_size();
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
                .map(|(_, field)| field.fixed_size())
                .max()
                .unwrap(),

            BCTypeKind::Bool => 1,
            BCTypeKind::Unit => 0,

            _ => panic!("Invalid type for fixed size: {:?}", self.kind),
        }
    }

    pub fn alignment(&self) -> u8 {
        match &self.kind {
            BCTypeKind::Opaque { bytes } => *bytes as u8,
            BCTypeKind::Bool => 1,
            BCTypeKind::Integer(_type) => _type.bytes(),
            BCTypeKind::Float(_type) => _type.bytes(),
            BCTypeKind::Pointer { .. } => 8, // TODO: make this configurable
            BCTypeKind::Array { element, .. } => element.alignment(),
            BCTypeKind::Struct { fields, .. } => fields
                .iter()
                .map(|(_, field)| field.alignment())
                .max()
                .unwrap_or(1),
            BCTypeKind::Union { fields, .. } => fields
                .iter()
                .map(|(_, field)| field.alignment())
                .max()
                .unwrap_or(1),
            BCTypeKind::Unit => 1,
            BCTypeKind::VariableSized { alignment, .. } => *alignment,
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