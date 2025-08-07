use std::hash::{Hash, Hasher};
use cx_util::log_error;
use speedy::{Readable, Writable};
use uuid::Uuid;
use crate::parse::ast::{CXExpr, CXFunctionPrototype};
use crate::parse::identifier::CXIdent;
use crate::parse::maps::CXTypeMap;
use crate::parse::parser::VisibilityMode;
use crate::parse::template::CXTemplateInput;
use crate::preparse::pp_type::{CXTypeSpecifier, PredeclarationType};

#[derive(Debug, Clone, Readable, Writable)]
pub struct CXType {
    pub uuid: u64,
    pub visibility_mode: VisibilityMode,
    pub specifiers: CXTypeSpecifier,
    pub kind: CXTypeKind,
}

impl PartialEq<Self> for CXType {
    fn eq(&self, other: &Self) -> bool {
        self.uuid == other.uuid
    }
}

impl Eq for CXType {}

impl Hash for CXType {
    fn hash<H: Hasher>(&self, state: &mut H) {
        state.write_u64(self.uuid)
    }
}

impl Default for CXType {
    fn default() -> Self {
        CXType {
            uuid: 0,
            visibility_mode: VisibilityMode::Private,
            specifiers: 0,
            kind: CXTypeKind::Unit
        }
    }
}

#[derive(Debug, Clone, Readable, Writable)]
pub enum CXTypeKind {
    Integer { bytes: u8, signed: bool },
    Float { bytes: u8 },
    Bool,
    Structured {
        name: Option<CXIdent>,
        fields: Vec<(String, CXType)>,
        has_destructor: bool
    },
    Union {
        name: Option<CXIdent>,
        fields: Vec<(String, CXType)>
    },
    Unit,

    StrongPointer { 
        inner: Box<CXType>,
        is_array: bool,
    },

    PointerTo {
        inner_type: Box<CXType>,
        
        sizeless_array: bool,
        weak: bool,
        nullable: bool
    },
    MemoryReference(Box<CXType>),
    Array {
        size: usize,
        inner_type: Box<CXType>
    },
    VariableLengthArray {
        size: Box<CXExpr>,
        _type: Box<CXType>,
    },
    Opaque {
        name: String,
        size: usize
    },
    Function {
        prototype: Box<CXFunctionPrototype>,
    },
}


impl CXType {
    pub fn unit() -> Self {
        CXType {
            uuid: 0,
            specifiers: 0,
            visibility_mode: VisibilityMode::Private,
            kind: CXTypeKind::Unit
        }
    }

    pub fn new(specifiers: CXTypeSpecifier, underlying_type: CXTypeKind) -> Self {
        CXType {
            uuid: Uuid::new_v4().as_u128() as u64,
            visibility_mode: VisibilityMode::Private, 
            specifiers,
            kind: underlying_type
        }
    }
    
    pub fn set_visibility_mode(&mut self, visibility: VisibilityMode) -> &mut Self {
        self.visibility_mode = visibility;
        self
    }

    pub fn add_specifier(&self, specifier: CXTypeSpecifier) -> Self {
        CXType {
            uuid: Uuid::new_v4().as_u128() as u64,
            visibility_mode: self.visibility_mode,
            specifiers: self.specifiers | specifier,
            kind: self.kind.clone()
        }
    }

    pub fn remove_specifier(&self, specifier: CXTypeSpecifier) -> Self {
        CXType {
            uuid: Uuid::new_v4().as_u128() as u64,
            visibility_mode: self.visibility_mode,
            specifiers: self.specifiers & !specifier,
            kind: self.kind.clone()
        }
    }

    pub fn get_specifier(&self, specifier: CXTypeSpecifier) -> bool {
        self.specifiers & specifier == specifier
    }
    
    pub fn intrinsic_type<'a>(&'a self, type_map: &'a CXTypeMap) -> Option<&'a CXType> {
        Some(&self)
    }

    pub fn intrinsic_type_kind<'a>(&'a self, type_map: &'a CXTypeMap) -> Option<&'a CXTypeKind> {
        Some(&self.kind)
    }
    
    pub fn has_destructor(&self, type_map: &CXTypeMap) -> bool {
        matches!(self.intrinsic_type_kind(type_map), Some(CXTypeKind::Structured { has_destructor: true, .. }))
    }
    
    pub fn get_destructor<'a>(&'a self, type_map: &'a CXTypeMap) -> Option<&'a str> {
        match self.intrinsic_type_kind(type_map)? {
            CXTypeKind::Structured { has_destructor: true, name, .. } 
                => Some(name.as_ref()?.as_str()),
            
            _ => None,
        }
    }
    
    pub fn get_structure_ref(&self, type_map: &CXTypeMap) -> Option<CXTypeKind> {
        let Some(CXTypeKind::MemoryReference(inner)) = self.intrinsic_type_kind(type_map) else {
            return None;
        };

        let intrin = inner.intrinsic_type_kind(type_map);
        if matches!(intrin, Some(CXTypeKind::Structured { .. })) {
            intrin.cloned()
        } else {
            panic!("Expected a structured type, found: {:?}", inner.kind);
        }
    }
    
    pub fn mem_ref_inner(&self, type_map: &CXTypeMap) -> Option<CXTypeKind> {
        let Some(CXTypeKind::MemoryReference(inner)) = self.intrinsic_type_kind(type_map) else {
            return None;
        };
        
        inner.intrinsic_type_kind(type_map).cloned()
    }

    pub fn pointer_to(self) -> Self {
        CXType {
            uuid: Uuid::new_v4().as_u128() as u64,
            specifiers: 0,
            visibility_mode: VisibilityMode::Private,
            kind: CXTypeKind::PointerTo {
                inner_type: Box::new(self),
                
                sizeless_array: false,
                weak: false,
                nullable: true
            }
        }
    }

    pub fn is_pointer(&self) -> bool {
        matches!(self.kind, CXTypeKind::PointerTo { .. })
    }

    pub fn is_strong_pointer(&self) -> bool {
        matches!(self.kind, CXTypeKind::StrongPointer { .. })
    }

    pub fn is_array(&self) -> bool {
        matches!(self.kind, CXTypeKind::Array { .. } | CXTypeKind::VariableLengthArray { .. })
    }

    pub fn is_structured(&self) -> bool {
        matches!(self.kind, CXTypeKind::Structured { .. } | CXTypeKind::Union { .. })
    }

    pub fn is_opaque(&self) -> bool {
        matches!(self.kind, CXTypeKind::Opaque { .. })
    }

    pub fn is_function(&self) -> bool {
        matches!(self.kind, CXTypeKind::Function { .. })
    }

    pub fn is_integer(&self) -> bool {
        matches!(self.kind, CXTypeKind::Integer { .. })
    }

    pub fn is_float(&self) -> bool {
        matches!(self.kind, CXTypeKind::Float { .. })
    }

    pub fn is_bool(&self) -> bool {
        matches!(self.kind, CXTypeKind::Bool)
    }

    pub fn is_unit(&self) -> bool {
        matches!(self.kind, CXTypeKind::Unit)
    }

    pub fn is_memory_reference(&self) -> bool {
        matches!(self.kind, CXTypeKind::MemoryReference(_))
    }
}

impl From<CXTypeKind> for CXType {
    fn from(kind: CXTypeKind) -> Self {
        CXType {
            uuid: Uuid::new_v4().as_u128() as u64,
            visibility_mode: VisibilityMode::Private,
            specifiers: 0,
            kind,
        }
    }
}

pub fn same_type(type_map: &CXTypeMap, t1: &CXType, t2: &CXType) -> bool {
    match (&t1.kind, &t2.kind) {
        (CXTypeKind::Array { inner_type: t1_type, .. },
         CXTypeKind::Array { inner_type: t2_type, .. }) =>
            same_type(type_map, t1_type, t2_type),

        (CXTypeKind::PointerTo { inner_type: t1_type, .. },
         CXTypeKind::PointerTo { inner_type: t2_type, .. }) =>
            same_type(type_map, t1_type, t2_type),

        (CXTypeKind::StrongPointer { inner: t1_inner, .. },
         CXTypeKind::StrongPointer { inner: t2_inner, .. }) =>
            same_type(type_map, t1_inner, t2_inner),

        (CXTypeKind::Structured { fields: t1_fields, .. },
         CXTypeKind::Structured { fields: t2_fields, .. }) => {
            t1_fields.iter().zip(t2_fields.iter())
                .all(|(f1, f2)|
                    same_type(type_map, &f1.1, &f2.1))
        },

        (CXTypeKind::Function { prototype: p1 },
         CXTypeKind::Function { prototype: p2 }) =>
            same_type(type_map, &p1.return_type, &p2.return_type) &&
                p1.params.iter().zip(p2.params.iter())
                    .all(|(a1, a2)| same_type(type_map, &a1._type, &a2._type)),

        (CXTypeKind::Integer { bytes: t1_bytes, signed: t1_signed },
            CXTypeKind::Integer { bytes: t2_bytes, signed: t2_signed }) =>
            t1_bytes == t2_bytes && t1_signed == t2_signed,

        (CXTypeKind::Float { bytes: t1_bytes },
            CXTypeKind::Float { bytes: t2_bytes }) =>
            t1_bytes == t2_bytes,

        (CXTypeKind::Bool, CXTypeKind::Bool) |
        (CXTypeKind::Unit, CXTypeKind::Unit) =>
            true,

        _ => false,
    }
}

pub fn struct_field_type(
    type_map: &CXTypeMap,
    type_: &CXType,
    field: &str
) -> Option<CXType> {
    let Some(CXTypeKind::Structured { fields, .. }) = type_.get_structure_ref(type_map) else {
        log_error!("Cannot access field {field} of non-structured type {type_}");
    };

    fields.iter()
        .find(|(name, _)| name == field)
        .map(|(_, ty)| ty.clone())
}
