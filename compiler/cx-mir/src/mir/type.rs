use std::hash::{Hash, Hasher};

use cx_ast::{ast::VisibilityMode, data::CXTypeSpecifier};
use cx_util::identifier::CXIdent;
use speedy::{Readable, Writable};

use crate::mir::{data::{MIRParameter, MIRTypeContext}, name_mangling::type_mangle};

#[derive(Debug, Clone, Readable, Writable)]
pub struct MIRTypeContext {
    pub type_identifiers: Vec<(CXIdent, MIRTypeId)>,
}

#[derive(Debug, Clone, Copy, Hash, PartialEq, Eq, PartialOrd, Ord, Readable, Writable)]
pub struct MIRTypeId(pub u64);

#[derive(Debug, Clone, Readable, Writable)]
pub struct MIRType {
    pub visibility: VisibilityMode,
    pub specifiers: CXTypeSpecifier,
    pub move_attributes: MIRMoveAttributes,
    pub strong_identifier: Option<CXIdent>,

    pub kind: MIRTypeKind,
}

#[derive(Debug, Default, Clone, Copy, PartialEq, Eq, Hash, Readable, Writable)]
pub enum MIRMoveAttributes {
    #[default]
    TriviallyCopyable,
    NoCopy,
    NoDrop
}

#[derive(Debug, Clone, Readable, Writable)]
pub enum MIRTypeKind {
    Unit,
    Integer {
        _type: MIRIntegerType,
        signed: bool,
    },
    Float {
        _type: MIRFloatType,
    },
    Structured {
        fields: Vec<(String, MIRTypeId)>,
    },
    Union {
        variants: Vec<(String, MIRTypeId)>,
    },
    TaggedUnion {
        variants: Vec<(String, MIRTypeId)>,
    },
    PointerTo {
        inner_type: Box<MIRTypeId>,
    },
    MemoryReference {
        inner_type: Box<MIRTypeId>,
    },
    Array {
        size: usize,
        inner_type: Box<MIRTypeId>,
    },
    Function {
        return_type: Box<MIRTypeId>,
        params: Vec<MIRParameter>,
        var_args: bool,
    },
    Opaque {
        size: usize,
    },
    Undefined,
    Str,
}

#[derive(Debug, Clone, Copy, PartialOrd, Ord, PartialEq, Eq, Readable, Writable)]
pub enum MIRIntegerType {
    I1,
    I8,
    I16,
    I32,
    I64,
    I128,
}

#[derive(Debug, Clone, Copy, PartialOrd, Ord, PartialEq, Eq, Readable, Writable)]
pub enum MIRFloatType {
    F32,
    F64,
}

impl MIRIntegerType {
    pub const fn bytes(&self) -> usize {
        match self {
            MIRIntegerType::I1 => 1,
            MIRIntegerType::I8 => 1,
            MIRIntegerType::I16 => 2,
            MIRIntegerType::I32 => 4,
            MIRIntegerType::I64 => 8,
            MIRIntegerType::I128 => 16,
        }
    }

    pub const fn from_bytes(bytes: u8) -> Option<Self> {
        match bytes {
            1 => Some(MIRIntegerType::I8),
            2 => Some(MIRIntegerType::I16),
            4 => Some(MIRIntegerType::I32),
            8 => Some(MIRIntegerType::I64),
            16 => Some(MIRIntegerType::I128),
            _ => None,
        }
    }
}

impl MIRFloatType {
    pub const fn bytes(&self) -> usize {
        match self {
            MIRFloatType::F32 => 4,
            MIRFloatType::F64 => 8,
        }
    }

    pub const fn from_bytes(bytes: u8) -> Option<Self> {
        match bytes {
            4 => Some(MIRFloatType::F32),
            8 => Some(MIRFloatType::F64),
            _ => None,
        }
    }
}

impl Hash for MIRType {
    fn hash<H: Hasher>(&self, state: &mut H) {
        self.visibility.hash(state);
        self.specifiers.hash(state);
        state.write(&self.mangle().into_bytes());
    }
}

impl PartialEq<Self> for MIRType {
    fn eq(&self, other: &Self) -> bool {
        same_type(self, other)
    }
}

impl Eq for MIRType {}

impl Default for MIRType {
    fn default() -> Self {
        MIRType {
            kind: MIRTypeKind::Unit,
        
            visibility: VisibilityMode::Private,
            specifiers: CXTypeSpecifier::default(),
            move_attributes: MIRMoveAttributes::default(),
            strong_identifier: None,
        }
    }
}

impl MIRType {
    pub fn mangle(&self) -> String {
        type_mangle(self)
    }

    pub fn unit() -> Self {
        Self::default()
    }

    pub fn bool() -> Self {
        MIRType {
            kind: MIRTypeKind::Integer {
                _type: MIRIntegerType::I1,
                signed: false,
            },
            
            ..Default::default()
        }
    }
    
    pub fn with_name(
        mut self,
        name: CXIdent,
    ) -> MIRType {
        self.strong_identifier = Some(name);
        self
    }

    pub fn set_visibility_mode(&mut self, visibility: VisibilityMode) -> &mut Self {
        self.visibility = visibility;
        self
    }

    pub fn add_specifier(mut self, specifier: CXTypeSpecifier) -> Self {
        self.specifiers |= specifier;
        self
    }

    pub fn with_specifier(&self, specifier: CXTypeSpecifier) -> Self {
        self.clone().add_specifier(specifier)
    }

    pub fn remove_specifier(&mut self, specifier: CXTypeSpecifier) -> &mut Self {
        self.specifiers &= !specifier;
        self
    }

    pub fn without_specifier(&self, specifier: CXTypeSpecifier) -> Self {
        let mut clone = self.clone();
        clone.remove_specifier(specifier);
        clone
    }

    pub fn get_specifier(&self, specifier: CXTypeSpecifier) -> bool {
        self.specifiers & specifier == specifier
    }

    pub fn mem_ref_inner(&self) -> Option<&MIRTypeId> {
        let MIRTypeKind::MemoryReference { inner_type, .. } = &self.kind else {
            return None;
        };

        Some(inner_type.as_ref())
    }

    pub fn array_inner(&self) -> Option<&MIRTypeId> {
        let MIRTypeKind::Array { inner_type, .. } = &self.kind else {
            return None;
        };

        Some(inner_type.as_ref())
    }

    pub fn ptr_inner(&self) -> Option<&MIRTypeId> {
        let MIRTypeKind::PointerTo { inner_type, .. } = &self.kind else {
            return None;
        };

        Some(inner_type.as_ref())
    }

    pub fn is_pointer(&self) -> bool {
        matches!(self.kind, MIRTypeKind::PointerTo { .. })
    }

    pub fn is_array(&self) -> bool {
        matches!(self.kind, MIRTypeKind::Array { .. })
    }

    pub fn is_memory_resident(&self) -> bool {
        matches!(
            self.kind,
            MIRTypeKind::Structured { .. }
                | MIRTypeKind::Union { .. }
                | MIRTypeKind::TaggedUnion { .. }
                | MIRTypeKind::Array { .. }
        )
    }

    pub fn is_opaque(&self) -> bool {
        matches!(self.kind, MIRTypeKind::Opaque { .. })
    }

    pub fn is_tagged_union(&self) -> bool {
        matches!(self.kind, MIRTypeKind::TaggedUnion { .. })
    }

    pub fn is_str(&self) -> bool {
        matches!(self.kind, MIRTypeKind::Str)
    }

    pub fn is_function(&self) -> bool {
        matches!(self.kind, MIRTypeKind::Function { .. })
    }

    pub fn is_integer(&self) -> bool {
        matches!(self.kind, MIRTypeKind::Integer { .. })
    }

    pub fn is_float(&self) -> bool {
        matches!(self.kind, MIRTypeKind::Float { .. })
    }

    pub fn is_unit(&self) -> bool {
        matches!(self.kind, MIRTypeKind::Unit)
    }

    pub fn is_structure(&self) -> bool {
        matches!(self.kind, MIRTypeKind::Structured { .. })
    }

    pub fn struct_attributes(&self) -> Option<MIRMoveAttributes> {
        Some(self.move_attributes)
    }

    pub fn is_memory_reference(&self) -> bool {
        matches!(self.kind, MIRTypeKind::MemoryReference { .. })
    }
    
    pub fn get_name(&self) -> Option<&CXIdent> {
        self.strong_identifier.as_ref()
    }

    pub fn set_name(&mut self, new_name: CXIdent) {
        self.strong_identifier = Some(new_name);
    }

    pub fn type_size(&self, definitions: &MIRTypeContext) -> usize {
        match &self.kind {
            MIRTypeKind::Integer { _type, .. } => _type.bytes(),
            MIRTypeKind::Float { _type } => _type.bytes(),
            MIRTypeKind::Unit => 0,
            MIRTypeKind::Opaque { size, .. } => *size,
            MIRTypeKind::MemoryReference { .. } | MIRTypeKind::PointerTo { .. } => {
                std::mem::size_of::<usize>()
            }

            MIRTypeKind::Structured { .. } => {
                let Some(fields) = self.aggregate_fields(definitions) else {
                    panic!("Incomplete structured type has no size: {}", self);
                };

                let mut offset: usize = 0;

                for (_, t) in fields {
                    let align = t.type_alignment(definitions);
                    offset = offset.div_ceil(align) * align;
                    offset += t.type_size(definitions);
                }

                offset
            }

            MIRTypeKind::Union { variants, .. } => variants
                .iter()
                .map(|(_, t)| t.type_size(definitions))
                .max()
                .unwrap_or(0),

            MIRTypeKind::Array { size, inner_type } => size * inner_type.padded_size(definitions),

            MIRTypeKind::TaggedUnion { .. } => {
                self.aggregate_fields(definitions)
                    .unwrap_or_else(|| panic!("Incomplete tagged union type has no size: {}", self))
                    .iter()
                    .map(|(_, t)| t.type_size(definitions))
                    .max()
                    .unwrap_or(0)
                    + 1
            }

            MIRTypeKind::Undefined { .. } => {
                unreachable!("Incomplete type has no type_size")
            }
            MIRTypeKind::Str => unreachable!("str is unsized and has no type_size"),
            MIRTypeKind::Function { .. } => unreachable!(),
        }
    }

    pub fn padded_size(&self, definitions: &MIRTypeContext) -> usize {
        let size = self.type_size(definitions);
        let align = self.type_alignment(definitions);
        size.div_ceil(align) * align
    }

    pub fn type_alignment(&self, definitions: &MIRTypeContext) -> usize {
        match &self.kind {
            MIRTypeKind::Integer { _type, .. } => _type.bytes().min(8),
            MIRTypeKind::Float { _type } => _type.bytes().min(8),
            MIRTypeKind::Unit => 1,
            MIRTypeKind::Opaque { size, .. } => (*size).min(8),
            MIRTypeKind::MemoryReference { .. } | MIRTypeKind::PointerTo { .. } => {
                std::mem::size_of::<usize>()
            }

            MIRTypeKind::Structured { fields } => fields
                .iter()
                .map(|(_, t)| t.type_alignment(definitions))
                .max()
                .unwrap_or(8),
            MIRTypeKind::Union { .. } => self
                .aggregate_fields(definitions)
                .unwrap_or_else(|| panic!("Incomplete union type has no alignment: {}", self))
                .iter()
                .map(|(_, t)| t.type_alignment(definitions))
                .max()
                .unwrap_or(8),

            MIRTypeKind::Array { inner_type, .. } => inner_type.type_alignment(definitions),

            MIRTypeKind::TaggedUnion { .. } => self
                .aggregate_fields(definitions)
                .unwrap_or_else(|| {
                    panic!("Incomplete tagged union type has no alignment: {}", self)
                })
                .iter()
                .map(|(_, t)| t.type_alignment(definitions))
                .max()
                .unwrap_or(8),

            MIRTypeKind::Undefined { .. } => {
                unreachable!("Incomplete type has no type_alignment")
            }
            MIRTypeKind::Str => unreachable!("str is unsized and has no type_alignment"),
            MIRTypeKind::Function { .. } => unreachable!(),
        }
    }
}

impl From<MIRTypeKind> for MIRType {
    fn from(kind: MIRTypeKind) -> Self {
        MIRType {
            kind,
 
            ..Default::default()
        }
    }
}

pub fn same_types(t1: impl Iterator<Item = MIRType>, t2: impl Iterator<Item = MIRType>) -> bool {
    t1.zip(t2).all(|(t1, t2)| same_type(&t1, &t2))
}

pub fn same_type(t1: &MIRType, t2: &MIRType) -> bool {
    match (&t1.kind, &t2.kind) {
        (
            MIRTypeKind::Array {
                inner_type: t1_type,
                size: t1_size,
            },
            MIRTypeKind::Array {
                inner_type: t2_type,
                size: t2_size,
            },
        ) => t1_size == t2_size && same_type(t1_type, t2_type),

        (
            MIRTypeKind::PointerTo {
                inner_type: t1_type,
            },
            MIRTypeKind::PointerTo {
                inner_type: t2_type,
            },
        ) => t1_type == t2_type,

        (
            MIRTypeKind::Structured {
                fields: t1_fields,
                ..
            },
            MIRTypeKind::Structured {
                fields: t2_fields,
                ..
            },
        ) => t1_fields.iter().zip(t2_fields.zip()).all(|x, y| x == y),

        (
            MIRTypeKind::Union {
                variants: t1_fields,
                ..
            },
            MIRTypeKind::Union {
                variants: t2_fields,
                ..
            },
        ) => t1_fields.iter().zip(t2_fields.zip()).all(|x, y| x == y),

        (
            MIRTypeKind::TaggedUnion {
                variants: t1_variants,
                ..
            },
            MIRTypeKind::TaggedUnion {
                variants: t2_variants,
                ..
            },
        ) => *a1 == *a2 && t1_variants.iter().zip(t2_variants.zip()).all(|x, y| x == y),

        (MIRTypeKind::Function { return_type, params, var_args }, MIRTypeKind::Function { return_type: r2, params: p2, var_args: v2 }) => {
            var_args == v2 && same_type(return_type, r2) && params.len() == p2.len() && params.iter().zip(p2.iter()).all(|(p1, p2)| p1.name == p2.name && same_type(&p1._type, &p2._type))
        }

        (
            MIRTypeKind::Integer {
                _type: t1_type,
                signed: t1_signed,
            },
            MIRTypeKind::Integer {
                _type: t2_type,
                signed: t2_signed,
            },
        ) => *t1_type == *t2_type && *t1_signed == *t2_signed,

        (MIRTypeKind::Float { _type: t1_type }, MIRTypeKind::Float { _type: t2_type }) => {
            *t1_type == *t2_type
        }

        (
            MIRTypeKind::MemoryReference {
                inner_type: t1_type,
            },
            MIRTypeKind::MemoryReference {
                inner_type: t2_type,
            },
        ) => same_type(t1_type, t2_type),

        (
            MIRTypeKind::Opaque { name: n1, size: s1 },
            MIRTypeKind::Opaque { name: n2, size: s2 },
        ) => n1 == n2 && s1 == s2,

        (MIRTypeKind::Undefined { name: n1 }, MIRTypeKind::Undefined { name: n2 }) => n1 == n2,

        (MIRTypeKind::Unit, MIRTypeKind::Unit) => true,

        (MIRTypeKind::Str, MIRTypeKind::Str) => true,

        _ => false,
    }
}