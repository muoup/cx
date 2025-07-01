use crate::ValueID;

#[derive(Debug, Clone)]
pub struct BCType {
    pub kind: BCTypeKind
}

#[derive(Debug, Clone)]
pub enum BCTypeKind {
    Opaque { bytes: usize },
    Signed { bytes: u8 },
    Unsigned { bytes: u8 },
    Bool,
    Float { bytes: u8 },
    Pointer,

    Array { element: Box<BCType>, size: usize },
    Struct { name: String, fields: Vec<(String, BCType)> },
    Union { name: String, fields: Vec<(String, BCType)> },
    
    VariableSized { size: ValueID, alignment: u8 },

    Unit
}

#[derive(Debug, Clone, PartialEq, Eq)]
pub enum BCTypeSize {
    Fixed(usize),
    Variable(ValueID),
}

impl BCTypeSize {
    pub fn assert_fixed(self, msg: &str) -> usize {
        match self {
            BCTypeSize::Fixed(size) => size,
            BCTypeSize::Variable(_) => panic!("{msg}: expected fixed size, got variable"),
        }
    }
}

impl From<BCTypeKind> for BCType {
    fn from(kind: BCTypeKind) -> Self {
        BCType { kind }
    }
}

impl BCType {
    pub fn size(&self) -> BCTypeSize {
        match &self.kind {
            BCTypeKind::VariableSized { size, .. } => BCTypeSize::Variable(*size),
            _ => BCTypeSize::Fixed(self.fixed_size()),
        }
    }
    
    pub fn fixed_size(&self) -> usize {
        match &self.kind {
            BCTypeKind::Opaque { bytes } => *bytes,
            BCTypeKind::Signed { bytes } => *bytes as usize,
            BCTypeKind::Unsigned { bytes } => *bytes as usize,
            BCTypeKind::Float { bytes } => *bytes as usize,
            BCTypeKind::Pointer => 8, // TODO: make this configurable
            BCTypeKind::Array { element, size } =>
                element.fixed_size() * size,
            BCTypeKind::Struct { fields, .. }
                => fields.iter()
                        .map(|(_, field)| field.fixed_size())
                        .sum(),
            BCTypeKind::Union { fields, .. }
                => fields.iter()
                        .map(|(_, field)| field.fixed_size())
                        .max()
                        .unwrap(),
            BCTypeKind::Unit => 0,
            
            _ => panic!("Invalid type for fixed size: {:?}", self.kind),
        }
    }
    
    pub fn alignment(&self) -> u8 {
        match &self.kind {
            BCTypeKind::Opaque { bytes } => *bytes as u8,
            BCTypeKind::Signed { bytes } => *bytes,
            BCTypeKind::Unsigned { bytes } => *bytes,
            BCTypeKind::Bool => 1,
            BCTypeKind::Float { bytes } => *bytes,
            BCTypeKind::Pointer => 8, // TODO: make this configurable
            BCTypeKind::Array { element, .. } => element.alignment(),
            BCTypeKind::Struct { fields, .. }
                => fields.iter().map(|(_, field)| field.alignment()).max().unwrap_or(1),
            BCTypeKind::Union { fields, .. }
                => fields.iter().map(|(_, field)| field.alignment()).max().unwrap_or(1),
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