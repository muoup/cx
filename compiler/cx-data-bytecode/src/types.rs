use crate::MIRValue;

#[derive(Debug, Clone, Hash, PartialEq, Eq)]
pub struct BCType {
    pub kind: BCTypeKind
}

impl BCType {
    pub fn unit() -> Self {
        BCType {
            kind: BCTypeKind::Unit
        }
    }
    
    pub fn default_pointer() -> Self {
        BCType {
            kind: BCTypeKind::Pointer { nullable: false, dereferenceable: 0 }
        }
    }
}

#[derive(Debug, Clone, Hash, PartialEq, Eq)]
pub enum BCTypeKind {
    Opaque { bytes: usize },
    Signed { bytes: u8 },
    Unsigned { bytes: u8 },
    Bool,
    Float { bytes: u8 },
    Pointer { nullable: bool, dereferenceable: u32 },

    Array { element: Box<BCType>, size: usize },
    Struct { name: String, fields: Vec<(String, BCType)> },
    Union { name: String, fields: Vec<(String, BCType)> },
    
    VariableSized { size: Box<MIRValue>, alignment: u8 },

    Unit
}

#[derive(Debug, Clone, PartialEq, Eq)]
pub enum BCTypeSize {
    Fixed(usize),
    Variable(MIRValue),
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
            BCTypeKind::VariableSized { size, .. } => BCTypeSize::Variable(*size.clone()),
            _ => BCTypeSize::Fixed(self.fixed_size()),
        }
    }
    
    pub fn fixed_size(&self) -> usize {
        match &self.kind {
            BCTypeKind::Opaque { bytes } => *bytes,
            BCTypeKind::Signed { bytes } => *bytes as usize,
            BCTypeKind::Unsigned { bytes } => *bytes as usize,
            BCTypeKind::Float { bytes } => *bytes as usize,
            BCTypeKind::Pointer { .. } => 8, // TODO: make this configurable
            BCTypeKind::Array { element, size } =>
                element.fixed_size() * size,
            BCTypeKind::Struct { fields, .. }
                => {
                let mut current_size = 0;
                
                for (_, field_type) in fields {
                    let field_size = field_type.fixed_size();
                    let field_alignment = field_type.alignment();
                    
                    // Align current size to the field's alignment
                    if current_size % field_alignment as usize != 0 {
                        current_size += field_alignment as usize - (current_size % field_alignment as usize);
                    }
                    
                    current_size += field_size;
                }
                
                current_size
            },
            BCTypeKind::Union { fields, .. }
                => fields.iter()
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
            BCTypeKind::Signed { bytes } => *bytes,
            BCTypeKind::Unsigned { bytes } => *bytes,
            BCTypeKind::Bool => 1,
            BCTypeKind::Float { bytes } => *bytes,
            BCTypeKind::Pointer { .. } => 8, // TODO: make this configurable
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