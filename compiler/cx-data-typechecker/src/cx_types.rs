use std::hash::{Hash, Hasher};
use speedy::{Readable, Writable};
use cx_data_ast::parse::identifier::CXIdent;
use cx_data_ast::parse::parser::VisibilityMode;
use cx_data_ast::preparse::naive_types::CXTypeSpecifier;
use uuid::Uuid;
use cx_util::mangling::{mangle_destructor, mangle_member_function};
use crate::ast::TCExpr;

#[derive(Debug, Clone, Readable, Writable)]
pub struct CXType {
    pub uuid: u64,
    pub visibility_mode: VisibilityMode,
    pub specifiers: CXTypeSpecifier,
    pub kind: CXTypeKind,
}

#[derive(Debug, Clone, Readable, Writable)]
pub struct CXParameter {
    pub name: Option<CXIdent>,
    pub _type: CXType,
}

#[derive(Debug, Clone, Default, Readable, Writable)]
pub struct CXFunctionPrototype {
    pub name: CXIdent,
    pub return_type: CXType,
    pub params: Vec<CXParameter>,

    pub needs_buffer: bool,
    pub var_args: bool
}

#[derive(Debug, Clone, PartialEq, Eq, Hash, Readable, Writable)]
pub struct CXTemplateInput {
    pub args: Vec<CXType>,
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
    },
    Union {
        name: Option<CXIdent>,
        fields: Vec<(String, CXType)>
    },
    Unit,

    StrongPointer { 
        inner_type: Box<CXType>,
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
        size: Box<TCExpr>,
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
    
    pub fn mem_ref_inner(&self) -> Option<&CXType> {
        let CXTypeKind::MemoryReference(inner) = &self.kind else {
            return None;
        };
        
        Some(inner.as_ref())
    }

    pub fn ptr_inner(&self) -> Option<&CXType> {
        let CXTypeKind::PointerTo { inner_type, .. } = &self.kind else {
            return None;
        };

        Some(inner_type.as_ref())
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

    pub fn mem_ref_to(self) -> Self {
        CXType {
            uuid: Uuid::new_v4().as_u128() as u64,
            specifiers: 0,
            visibility_mode: VisibilityMode::Private,
            kind: CXTypeKind::MemoryReference(Box::new(self))
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
        matches!(self.kind, CXTypeKind::Integer { .. } | CXTypeKind::Bool)
    }

    pub fn is_float(&self) -> bool {
        matches!(self.kind, CXTypeKind::Float { .. })
    }

    pub fn is_unit(&self) -> bool {
        matches!(self.kind, CXTypeKind::Unit)
    }

    pub fn is_memory_reference(&self) -> bool {
        matches!(self.kind, CXTypeKind::MemoryReference(_))
    }
    
    pub fn get_name(&self) -> Option<&str> {
        match &self.kind {
            CXTypeKind::Structured { name, .. } |
            CXTypeKind::Union { name, .. } => name.as_ref().map(|n| n.as_str()),
            CXTypeKind::Opaque { name, .. } => Some(name),
            
            _ => None,
        }
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

pub fn same_type(t1: &CXType, t2: &CXType) -> bool {
    if t1.uuid == t2.uuid {
        return true;
    }

    match (&t1.kind, &t2.kind) {
        (CXTypeKind::Array { inner_type: t1_type, .. },
         CXTypeKind::Array { inner_type: t2_type, .. }) =>
            same_type(t1_type, t2_type),

        (CXTypeKind::PointerTo { inner_type: t1_type, .. },
         CXTypeKind::PointerTo { inner_type: t2_type, .. }) =>
            same_type(t1_type, t2_type),

        (CXTypeKind::StrongPointer { inner_type: t1_inner, .. },
         CXTypeKind::StrongPointer { inner_type: t2_inner, .. }) =>
            same_type(t1_inner, t2_inner),

        (CXTypeKind::Structured { fields: t1_fields, .. },
         CXTypeKind::Structured { fields: t2_fields, .. }) => {
            t1_fields.iter().zip(t2_fields.iter())
                .all(|(f1, f2)|
                    same_type(&f1.1, &f2.1))
        },

        (CXTypeKind::Function { prototype: p1 },
         CXTypeKind::Function { prototype: p2 }) =>
            same_type(&p1.return_type, &p2.return_type) &&
                p1.params.iter().zip(p2.params.iter())
                    .all(|(a1, a2)| same_type(&a1._type, &a2._type)),

        (CXTypeKind::Integer { bytes: t1_bytes, signed: t1_signed },
         CXTypeKind::Integer { bytes: t2_bytes, signed: t2_signed }) =>
            *t1_bytes == *t2_bytes && *t1_signed == *t2_signed,

        (CXTypeKind::Float { bytes: t1_bytes },
         CXTypeKind::Float { bytes: t2_bytes }) =>
            *t1_bytes == *t2_bytes,

        (CXTypeKind::Bool, CXTypeKind::Bool) |
        (CXTypeKind::Unit, CXTypeKind::Unit) =>
            true,

        _ => false,
    }
}

#[derive(Debug, Clone, PartialEq, Eq, Readable, Writable)]
pub enum CXFunctionIdentifier {
    Standard(CXIdent),
    MemberFunction {
        _type: CXType,
        function_name: CXIdent
    },
    Destructor(CXType)
}

impl CXFunctionIdentifier {
    pub fn as_ident(&self) -> CXIdent {
        CXIdent::from(self.as_string())
    }

    pub fn as_string(&self) -> String {
        match self {
            CXFunctionIdentifier::Standard(name) => name.to_string(),
            CXFunctionIdentifier::MemberFunction { _type, function_name, .. } => {
                let Some(name) = _type.get_name() else {
                    unreachable!("Member function's type must have a name");
                };

                mangle_member_function(name.to_string(), function_name.as_str())
            },
            CXFunctionIdentifier::Destructor(_type) => {
                let Some(name) = _type.get_name() else {
                    unreachable!("Destructor's type must have a name");
                };

                mangle_destructor(name)
            }
        }
    }
}
