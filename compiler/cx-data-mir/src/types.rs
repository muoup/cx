use crate::MIRValue;

#[derive(Debug, Clone, Hash, PartialEq, Eq)]
pub struct MIRType {
    pub kind: MIRTypeKind
}

impl MIRType {
    pub fn unit() -> Self {
        MIRType {
            kind: MIRTypeKind::Unit
        }
    }
    
    pub fn default_pointer() -> Self {
        MIRType {
            kind: MIRTypeKind::Pointer { nullable: false, dereferenceable: 0 }
        }
    }
}

#[derive(Debug, Clone, Hash, PartialEq, Eq)]
pub enum MIRTypeKind {
    Opaque { bytes: usize },
    Signed { bytes: u8 },
    Unsigned { bytes: u8 },
    Bool,
    Float { bytes: u8 },
    Pointer { nullable: bool, dereferenceable: u32 },

    Array { element: Box<MIRType>, size: usize },
    Struct { name: String, fields: Vec<(String, MIRType)> },
    Union { name: String, fields: Vec<(String, MIRType)> },
    
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

impl From<MIRTypeKind> for MIRType {
    fn from(kind: MIRTypeKind) -> Self {
        MIRType { kind }
    }
}

impl MIRType {
    pub fn size(&self) -> BCTypeSize {
        match &self.kind {
            MIRTypeKind::VariableSized { size, .. } => BCTypeSize::Variable(*size.clone()),
            _ => BCTypeSize::Fixed(self.fixed_size()),
        }
    }
    
    pub fn fixed_size(&self) -> usize {
        match &self.kind {
            MIRTypeKind::Opaque { bytes } => *bytes,
            MIRTypeKind::Signed { bytes } => *bytes as usize,
            MIRTypeKind::Unsigned { bytes } => *bytes as usize,
            MIRTypeKind::Float { bytes } => *bytes as usize,
            MIRTypeKind::Pointer { .. } => 8, // TODO: make this configurable
            MIRTypeKind::Array { element, size } =>
                element.fixed_size() * size,
            MIRTypeKind::Struct { fields, .. }
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
            MIRTypeKind::Union { fields, .. }
                => fields.iter()
                        .map(|(_, field)| field.fixed_size())
                        .max()
                        .unwrap(),
            
            MIRTypeKind::Bool => 1,
            MIRTypeKind::Unit => 0,
            
            _ => panic!("Invalid type for fixed size: {:?}", self.kind),
        }
    }
    
    pub fn alignment(&self) -> u8 {
        match &self.kind {
            MIRTypeKind::Opaque { bytes } => *bytes as u8,
            MIRTypeKind::Signed { bytes } => *bytes,
            MIRTypeKind::Unsigned { bytes } => *bytes,
            MIRTypeKind::Bool => 1,
            MIRTypeKind::Float { bytes } => *bytes,
            MIRTypeKind::Pointer { .. } => 8, // TODO: make this configurable
            MIRTypeKind::Array { element, .. } => element.alignment(),
            MIRTypeKind::Struct { fields, .. }
                => fields.iter().map(|(_, field)| field.alignment()).max().unwrap_or(1),
            MIRTypeKind::Union { fields, .. }
                => fields.iter().map(|(_, field)| field.alignment()).max().unwrap_or(1),
            MIRTypeKind::Unit => 1,
            MIRTypeKind::VariableSized { alignment, .. } => *alignment,
        }
    }
    
    #[inline]
    pub fn is_void(&self) -> bool {
        matches!(self.kind, MIRTypeKind::Unit)
    }
    
    #[inline]
    pub fn is_structure(&self) -> bool {
        matches!(self.kind, MIRTypeKind::Struct { .. })
    }
}