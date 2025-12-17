use std::hash::{Hash, Hasher};

use crate::format::type_mangle;
use crate::function_map::CXFunctionIdentifier;
use cx_parsing_data::ast::VisibilityMode;
use cx_parsing_data::data::{CXFunctionContract, CXTypeSpecifier};
use cx_util::identifier::CXIdent;
use speedy::{Readable, Writable};

#[derive(Debug, Clone, Readable, Writable)]
pub struct CXType {
    pub visibility: VisibilityMode,
    pub specifiers: CXTypeSpecifier,

    pub kind: CXTypeKind,
}

impl CXType {
    pub fn mangle(&self) -> String {
        type_mangle(self)
    }
}

impl Hash for CXType {
    fn hash<H: Hasher>(&self, state: &mut H) {
        self.visibility.hash(state);
        self.specifiers.hash(state);
        state.write(&self.mangle().into_bytes());
    }
}

#[derive(Debug, Clone, Readable, Writable)]
pub struct TCParameter {
    pub name: Option<CXIdent>,
    pub _type: CXType,
}

#[derive(Debug, Clone, Default, Readable, Writable)]
pub struct CXFunctionPrototype {
    pub name: CXFunctionIdentifier,
    pub return_type: CXType,
    pub params: Vec<TCParameter>,
    pub var_args: bool,
    pub contract: CXFunctionContract,
}

#[derive(Debug, Clone, Hash, PartialEq, Eq, Readable, Writable)]
pub struct CXTemplateInput {
    pub args: Vec<CXType>,
}

impl PartialEq<Self> for CXType {
    fn eq(&self, other: &Self) -> bool {
        same_type(self, other)
    }
}

impl Eq for CXType {}

impl Default for CXType {
    fn default() -> Self {
        CXType {
            visibility: VisibilityMode::Private,
            specifiers: 0,

            kind: CXTypeKind::Unit,
        }
    }
}

#[derive(Debug, Clone, Readable, Writable)]
pub enum CXTypeKind {
    Integer {
        _type: CXIntegerType,
        signed: bool,
    },
    Float {
        _type: CXFloatType,
    },
    Bool,
    Structured {
        name: Option<CXIdent>,
        base_identifier: Option<CXIdent>,
        fields: Vec<(String, CXType)>,

        copyable: bool,
    },
    Union {
        name: Option<CXIdent>,
        variants: Vec<(String, CXType)>,
    },
    TaggedUnion {
        name: CXIdent,
        variants: Vec<(String, CXType)>,
    },
    Unit,

    PointerTo {
        inner_type: Box<CXType>,

        sizeless_array: bool,
        weak: bool,
        nullable: bool,
    },
    MemoryReference(Box<CXType>),
    Array {
        size: usize,
        inner_type: Box<CXType>,
    },
    Opaque {
        name: String,
        size: usize,
    },
    Function {
        prototype: Box<CXFunctionPrototype>,
    },
}

#[derive(Debug, Clone, Copy, PartialEq, Eq, Readable, Writable)]
pub enum CXIntegerType {
    I8,
    I16,
    I32,
    I64,
    I128,
}

#[derive(Debug, Clone, Copy, PartialEq, Eq, Readable, Writable)]
pub enum CXFloatType {
    F32,
    F64,
}

impl CXIntegerType {
    pub const fn bytes(&self) -> usize {
        match self {
            CXIntegerType::I8 => 1,
            CXIntegerType::I16 => 2,
            CXIntegerType::I32 => 4,
            CXIntegerType::I64 => 8,
            CXIntegerType::I128 => 16,
        }
    }

    pub const fn from_bytes(bytes: u8) -> Option<Self> {
        match bytes {
            1 => Some(CXIntegerType::I8),
            2 => Some(CXIntegerType::I16),
            4 => Some(CXIntegerType::I32),
            8 => Some(CXIntegerType::I64),
            16 => Some(CXIntegerType::I128),
            _ => None,
        }
    }
}

impl CXFloatType {
    pub const fn bytes(&self) -> usize {
        match self {
            CXFloatType::F32 => 4,
            CXFloatType::F64 => 8,
        }
    }

    pub const fn from_bytes(bytes: u8) -> Option<Self> {
        match bytes {
            4 => Some(CXFloatType::F32),
            8 => Some(CXFloatType::F64),
            _ => None,
        }
    }
}

impl CXType {
    pub fn unit() -> Self {
        CXType {
            specifiers: 0,
            visibility: VisibilityMode::Private,

            kind: CXTypeKind::Unit,
        }
    }

    pub fn new(specifiers: CXTypeSpecifier, underlying_type: CXTypeKind) -> Self {
        CXType {
            visibility: VisibilityMode::Private,
            specifiers,

            kind: underlying_type,
        }
    }

    pub fn set_visibility_mode(&mut self, visibility: VisibilityMode) -> &mut Self {
        self.visibility = visibility;
        self
    }

    pub fn add_specifier(&self, specifier: CXTypeSpecifier) -> Self {
        let mut clone = self.clone();
        clone.specifiers |= specifier;
        clone
    }

    pub fn remove_specifier(&self, specifier: CXTypeSpecifier) -> Self {
        let mut clone = self.clone();
        clone.specifiers &= !specifier;
        clone
    }

    pub fn get_specifier(&self, specifier: CXTypeSpecifier) -> bool {
        self.specifiers & specifier == specifier
    }
    
    pub fn memory_resident_type(&self) -> &CXType {
        self.mem_ref_inner()
            .or_else(|| self.ptr_inner())
            .unwrap_or(self)
    }

    pub fn mem_ref_inner(&self) -> Option<&CXType> {
        let CXTypeKind::MemoryReference(inner) = &self.kind else {
            return None;
        };

        Some(inner.as_ref())
    }
    
    pub fn array_inner(&self) -> Option<&CXType> {
        let CXTypeKind::Array { inner_type, .. } = &self.kind else {
            return None;
        };

        Some(inner_type.as_ref())
    }

    pub fn ptr_inner(&self) -> Option<&CXType> {
        let CXTypeKind::PointerTo { inner_type, .. } = &self.kind else {
            return None;
        };

        Some(inner_type.as_ref())
    }
    
    pub fn pointer_to(self) -> Self {
        CXType {
            specifiers: 0,
            visibility: VisibilityMode::Private,

            kind: CXTypeKind::PointerTo {
                inner_type: Box::new(self),

                sizeless_array: false,
                weak: false,
                nullable: true,
            },
        }
    }

    pub fn mem_ref_to(self) -> Self {
        CXType {
            specifiers: 0,
            visibility: VisibilityMode::Private,
            kind: CXTypeKind::MemoryReference(Box::new(self)),
        }
    }

    pub fn is_pointer(&self) -> bool {
        matches!(self.kind, CXTypeKind::PointerTo { .. })
    }

    pub fn is_array(&self) -> bool {
        matches!(self.kind, CXTypeKind::Array { .. })
    }

    pub fn is_memory_resident(&self) -> bool {
        matches!(
            self.kind,
            CXTypeKind::Structured { .. }
                | CXTypeKind::Union { .. }
                | CXTypeKind::TaggedUnion { .. }
                | CXTypeKind::Array { .. }
        )
    }

    pub fn is_opaque(&self) -> bool {
        matches!(self.kind, CXTypeKind::Opaque { .. })
    }
    
    pub fn is_tagged_union(&self) -> bool {
        matches!(self.kind, CXTypeKind::TaggedUnion { .. })
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
    
    pub fn is_structure(&self) -> bool {
        matches!(self.kind, CXTypeKind::Structured { .. })
    }

    pub fn is_memory_reference(&self) -> bool {
        matches!(self.kind, CXTypeKind::MemoryReference(_))
    }

    pub fn was_template_instantiated(&self) -> bool {
        match &self.kind {
            CXTypeKind::Structured {
                base_identifier, ..
            } => {
                let Some(base_name) = base_identifier else {
                    return false;
                };

                let Some(name) = self.get_name() else {
                    return false;
                };

                base_name.as_str() != name
            }

            _ => false,
        }
    }

    pub fn get_name(&self) -> Option<&str> {
        match &self.kind {
            CXTypeKind::Structured { name, .. } | CXTypeKind::Union { name, .. } => {
                name.as_ref().map(|n| n.as_str())
            }
            CXTypeKind::Opaque { name, .. } => Some(name),
            CXTypeKind::TaggedUnion { name, .. } => Some(name.as_str()),

            _ => None,
        }
    }

    pub fn get_fn_ident(&self) -> Option<&CXFunctionIdentifier> {
        match &self.kind {
            CXTypeKind::Function { prototype } => Some(&prototype.name),
            _ => None,
        }
    }

    pub fn get_identifier(&self) -> Option<&CXIdent> {
        match &self.kind {
            CXTypeKind::Structured {
                base_identifier, ..
            } => base_identifier.as_ref(),

            _ => None,
        }
    }

    pub fn set_name(&mut self, name: CXIdent) {
        match &mut self.kind {
            CXTypeKind::Structured {
                name: n,
                base_identifier,
                ..
            } => {
                *base_identifier = n.clone();
                *n = Some(name)
            }
            CXTypeKind::Union { name: n, .. } => *n = Some(name),
            _ => {}
        }
    }

    pub fn map_name(&mut self, f: impl FnOnce(&str) -> String) {
        if let Some(name) = self.get_name() {
            let new_name = f(name);
            self.set_name(CXIdent::new(new_name));
        }
    }

    pub fn copyable(&self) -> bool {
        match &self.kind {
            CXTypeKind::Structured { copyable, .. } => *copyable,
            CXTypeKind::TaggedUnion { variants, .. } => variants.iter().all(|(_, t)| t.copyable()),

            _ => true,
        }
    }

    pub fn type_size(&self) -> usize {
        match &self.kind {
            CXTypeKind::Integer { _type, .. } => _type.bytes(),
            CXTypeKind::Float { _type } => _type.bytes(),
            CXTypeKind::Bool => 1,
            CXTypeKind::Unit => 0,
            CXTypeKind::Opaque { size, .. } => *size,
            CXTypeKind::MemoryReference(_)
            | CXTypeKind::PointerTo { .. } => std::mem::size_of::<usize>(),

            CXTypeKind::Structured { fields, .. } => {
                let mut offset = 0;
                
                for (_, t) in fields {
                    let align = t.type_alignment();
                    offset = (offset * align + align - 1) / align;
                    offset += t.type_size();
                }
                
                offset
            }
            
            CXTypeKind::Union { variants, .. } => variants
                .iter()
                .map(|(_, t)| t.type_size())
                .max()
                .unwrap_or(0),

            CXTypeKind::Array { size, inner_type } => size * inner_type.type_size(),

            CXTypeKind::TaggedUnion { variants, .. } => {
                variants
                    .iter()
                    .map(|(_, t)| t.type_size())
                    .max()
                    .unwrap_or(0)
                    + 1
            }

            CXTypeKind::Function { .. } => unreachable!(),
        }
    }
    
    pub fn padded_size(&self) -> u64 {
        let size = self.type_size() as u64;
        let align = self.type_alignment() as u64;
        (size + align - 1) / align * align
    }
    
    pub fn type_alignment(&self) -> usize {
        match &self.kind {
            CXTypeKind::Integer { _type, .. } => _type.bytes().min(8),
            CXTypeKind::Float { _type } => _type.bytes().min(8),
            CXTypeKind::Bool => 1,
            CXTypeKind::Unit => 1,
            CXTypeKind::Opaque { size, .. } => (*size).min(8),
            CXTypeKind::MemoryReference(_)
            | CXTypeKind::PointerTo { .. } => std::mem::size_of::<usize>(),

            CXTypeKind::Structured { fields, .. } => fields
                .iter()
                .map(|(_, t)| t.type_alignment())
                .max()
                .unwrap_or(8),
            CXTypeKind::Union { variants, .. } => variants
                .iter()
                .map(|(_, t)| t.type_alignment())
                .max()
                .unwrap_or(8),

            CXTypeKind::Array { inner_type, .. } => inner_type.type_alignment(),

            CXTypeKind::TaggedUnion { variants, .. } => variants
                .iter()
                .map(|(_, t)| t.type_alignment())
                .max()
                .unwrap_or(8),

            CXTypeKind::Function { .. } => unreachable!(),
        }
    }

    /*
     *   With things like type constructors, they are not per-se function pointers as they
     *   cannot be called via a function pointer syntax, so this serves as a shortcut to avoid
     *   having to write out a full type when it is not used.
     */
    pub fn internal_function() -> Self {
        CXType::new(
            0,
            CXTypeKind::Function {
                prototype: Box::new(CXFunctionPrototype {
                    name: CXFunctionIdentifier::default(),
                    return_type: CXType::unit(),
                    params: vec![],
                    var_args: false,
                    contract: CXFunctionContract::default(),
                }),
            },
        )
    }
}

impl From<CXTypeKind> for CXType {
    fn from(kind: CXTypeKind) -> Self {
        CXType {
            visibility: VisibilityMode::Private,
            specifiers: 0,
            kind,
        }
    }
}

pub fn same_type(t1: &CXType, t2: &CXType) -> bool {
    let t1_name = t1.get_name();
    let t2_name = t2.get_name();

    if t1_name.is_some() && t2_name.is_some() {
        return t1_name == t2_name;
    }

    match (&t1.kind, &t2.kind) {
        (
            CXTypeKind::Array {
                inner_type: t1_type,
                ..
            },
            CXTypeKind::Array {
                inner_type: t2_type,
                ..
            },
        ) => same_type(t1_type, t2_type),

        (
            CXTypeKind::PointerTo {
                inner_type: t1_type,
                ..
            },
            CXTypeKind::PointerTo {
                inner_type: t2_type,
                ..
            },
        ) => same_type(t1_type, t2_type),

        (
            CXTypeKind::Structured {
                fields: t1_fields, copyable: c1, ..
            },
            CXTypeKind::Structured {
                fields: t2_fields, copyable: c2, ..
            },
        ) => c1 == c2 && t1_fields
            .iter()
            .zip(t2_fields.iter())
            .all(|(f1, f2)| same_type(&f1.1, &f2.1)),

        (CXTypeKind::Function { prototype: p1 }, CXTypeKind::Function { prototype: p2 }) => {
            same_type(&p1.return_type, &p2.return_type)
                && p1
                    .params
                    .iter()
                    .zip(p2.params.iter())
                    .all(|(a1, a2)| same_type(&a1._type, &a2._type))
        }

        (
            CXTypeKind::Integer {
                _type: t1_type,
                signed: t1_signed,
            },
            CXTypeKind::Integer {
                _type: t2_type,
                signed: t2_signed,
            },
        ) => *t1_type == *t2_type && *t1_signed == *t2_signed,

        (CXTypeKind::Float { _type: t1_type }, CXTypeKind::Float { _type: t2_type }) => {
            *t1_type == *t2_type
        }

        (CXTypeKind::Bool, CXTypeKind::Bool) | (CXTypeKind::Unit, CXTypeKind::Unit) => true,

        _ => false,
    }
}

impl CXFunctionPrototype {
    pub fn mangle_name(&self) -> String {
        self.name.mangle()
    }

    pub fn apply_template_mangling(&mut self) {
        self.name
            .template_mangle2(&self.return_type, self.params.iter().map(|p| &p._type));
    }
}
