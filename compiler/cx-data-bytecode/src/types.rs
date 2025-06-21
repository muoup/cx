use std::thread::Builder;
use cx_data_ast::parse::ast::{CXFunctionPrototype, CXTypeMap};
use cx_data_ast::parse::value_type::{CXType, CXTypeKind};
use crate::{BCFunctionPrototype, BCParameter};

#[derive(Debug, Clone)]
pub struct BCType {
    pub kind: BCTypeKind
}

#[derive(Debug, Clone)]
pub enum BCTypeKind {
    Opaque { bytes: usize },
    Signed { bytes: u8 },
    Unsigned { bytes: u8 },
    Float { bytes: u8 },
    Pointer,
    
    Array { size: usize, _type: Box<BCType> },
    Struct { name: String, fields: Vec<(String, BCType)> },
    Union { name: String, fields: Vec<(String, BCType)> },

    Unit
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
            BCTypeKind::Signed { bytes } => *bytes as usize,
            BCTypeKind::Unsigned { bytes } => *bytes as usize,
            BCTypeKind::Float { bytes } => *bytes as usize,
            BCTypeKind::Pointer => 8, // TODO: make this configurable
            BCTypeKind::Array { size, _type } 
                => size * _type.size(),
            BCTypeKind::Struct { fields, .. } 
                => fields.iter().map(|(_, field)| field.size()).sum(),
            BCTypeKind::Union { fields, .. }
                => fields.iter().map(|(_, field)| field.size()).max().unwrap(),
            BCTypeKind::Unit => 0,
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