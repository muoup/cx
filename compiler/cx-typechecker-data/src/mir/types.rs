use std::hash::{Hash, Hasher};

use cx_parsing_data::ast::VisibilityMode;
use cx_parsing_data::data::{CXFunctionContract, CXTypeSpecifier};
use cx_util::identifier::CXIdent;
use speedy::{Readable, Writable};

use crate::mir::name_mangling::type_mangle;

#[derive(Debug, Clone, Readable, Writable)]
pub struct MIRType {
    pub visibility: VisibilityMode,
    pub specifiers: CXTypeSpecifier,

    pub kind: MIRTypeKind,
}

impl MIRType {
    pub fn mangle(&self) -> String {
        type_mangle(self)
    }
}

impl Hash for MIRType {
    fn hash<H: Hasher>(&self, state: &mut H) {
        self.visibility.hash(state);
        self.specifiers.hash(state);
        state.write(&self.mangle().into_bytes());
    }
}

#[derive(Debug, Clone, Readable, Writable)]
pub struct MIRParameter {
    pub name: Option<CXIdent>,
    pub _type: MIRType,
}

#[derive(Debug, Clone, Default, Readable, Writable)]
pub struct MIRFunctionPrototype {
    pub name: CXIdent,
    pub return_type: MIRType,
    pub params: Vec<MIRParameter>,
    pub var_args: bool,
    pub contract: CXFunctionContract,
}

#[derive(Debug, Clone, Hash, PartialEq, Eq, Readable, Writable)]
pub struct CXTemplateInput {
    pub args: Vec<MIRType>,
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
            visibility: VisibilityMode::Private,
            specifiers: 0,

            kind: MIRTypeKind::Unit,
        }
    }
}

#[derive(Debug, Clone, Readable, Writable)]
pub struct TemplateInstantiationInformation {
    base_name: CXIdent,
    template_input: CXTemplateInput,
}

#[derive(Debug, Clone, Readable, Writable)]
pub enum MIRTypeKind {
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
        // Boxed for size reasons
        template_info: Option<Box<TemplateInstantiationInformation>>,
        fields: Vec<(String, MIRType)>,

        copyable: bool,
    },
    Union {
        name: Option<CXIdent>,
        variants: Vec<(String, MIRType)>,
    },
    TaggedUnion {
        name: CXIdent,
        variants: Vec<(String, MIRType)>,
    },
    Unit,

    PointerTo {
        inner_type: Box<MIRType>,

        sizeless_array: bool,
        weak: bool,
        nullable: bool,
    },
    MemoryReference(Box<MIRType>),
    Array {
        size: usize,
        inner_type: Box<MIRType>,
    },
    Opaque {
        name: CXIdent,
        size: usize,
    },
    Function {
        prototype: Box<MIRFunctionPrototype>,
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

impl MIRType {
    pub fn unit() -> Self {
        MIRType {
            specifiers: 0,
            visibility: VisibilityMode::Private,

            kind: MIRTypeKind::Unit,
        }
    }

    pub fn new(specifiers: CXTypeSpecifier, underlying_type: MIRTypeKind) -> Self {
        MIRType {
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

    pub fn memory_resident_type(&self) -> &MIRType {
        self.mem_ref_inner()
            .or_else(|| self.ptr_inner())
            .unwrap_or(self)
    }

    pub fn mem_ref_inner(&self) -> Option<&MIRType> {
        let MIRTypeKind::MemoryReference(inner) = &self.kind else {
            return None;
        };

        Some(inner.as_ref())
    }

    pub fn array_inner(&self) -> Option<&MIRType> {
        let MIRTypeKind::Array { inner_type, .. } = &self.kind else {
            return None;
        };

        Some(inner_type.as_ref())
    }

    pub fn ptr_inner(&self) -> Option<&MIRType> {
        let MIRTypeKind::PointerTo { inner_type, .. } = &self.kind else {
            return None;
        };

        Some(inner_type.as_ref())
    }

    pub fn pointer_to(self) -> Self {
        MIRType {
            specifiers: 0,
            visibility: VisibilityMode::Private,

            kind: MIRTypeKind::PointerTo {
                inner_type: Box::new(self),

                sizeless_array: false,
                weak: false,
                nullable: true,
            },
        }
    }

    pub fn mem_ref_to(self) -> Self {
        MIRType {
            specifiers: 0,
            visibility: VisibilityMode::Private,
            kind: MIRTypeKind::MemoryReference(Box::new(self)),
        }
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

    pub fn is_function(&self) -> bool {
        matches!(self.kind, MIRTypeKind::Function { .. })
    }

    pub fn is_integer(&self) -> bool {
        matches!(self.kind, MIRTypeKind::Integer { .. } | MIRTypeKind::Bool)
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

    pub fn is_memory_reference(&self) -> bool {
        matches!(self.kind, MIRTypeKind::MemoryReference(_))
    }

    pub fn was_template_instantiated(&self) -> bool {
        match &self.kind {
            MIRTypeKind::Structured { template_info, .. } => template_info.is_some(),

            _ => false,
        }
    }

    pub fn get_name(&self) -> Option<&CXIdent> {
        match &self.kind {
            MIRTypeKind::Structured { name, .. } | MIRTypeKind::Union { name, .. } => name.as_ref(),
            MIRTypeKind::Opaque { name, .. } => Some(name),
            MIRTypeKind::TaggedUnion { name, .. } => Some(name),

            _ => None,
        }
    }

    pub fn get_fn_name(&self) -> Option<&CXIdent> {
        match &self.kind {
            MIRTypeKind::Function { prototype } => Some(&prototype.name),
            _ => None,
        }
    }

    pub fn get_base_identifier(&self) -> Option<&CXIdent> {
        match &self.kind {
            MIRTypeKind::Structured {
                name,
                template_info,
                ..
            } => template_info
                .as_ref()
                .map(|info| &info.base_name)
                .or_else(|| name.as_ref()),

            _ => None,
        }
    }

    pub fn add_template_info(&mut self, new_name: CXIdent, template_input: CXTemplateInput) {
        match &mut self.kind {
            MIRTypeKind::Structured {
                name: n,
                template_info,
                ..
            } => {
                let old_name = std::mem::take(n).expect("Templated types cannot be nameless");

                *n = Some(new_name.clone());
                *template_info = Some(Box::new(TemplateInstantiationInformation {
                    base_name: old_name,
                    template_input,
                }));
            }
            MIRTypeKind::Union { name: n, .. } => *n = Some(new_name),
            _ => {}
        }
    }

    pub fn copyable(&self) -> bool {
        match &self.kind {
            MIRTypeKind::Structured { copyable, .. } => *copyable,
            MIRTypeKind::TaggedUnion { variants, .. } => variants.iter().all(|(_, t)| t.copyable()),

            _ => true,
        }
    }

    pub fn type_size(&self) -> usize {
        match &self.kind {
            MIRTypeKind::Integer { _type, .. } => _type.bytes(),
            MIRTypeKind::Float { _type } => _type.bytes(),
            MIRTypeKind::Bool => 1,
            MIRTypeKind::Unit => 0,
            MIRTypeKind::Opaque { size, .. } => *size,
            MIRTypeKind::MemoryReference(_) | MIRTypeKind::PointerTo { .. } => {
                std::mem::size_of::<usize>()
            }

            MIRTypeKind::Structured { fields, .. } => {
                let mut offset = 0;

                for (_, t) in fields {
                    let align = t.type_alignment();
                    offset = (offset * align).div_ceil(align);
                    offset += t.type_size();
                }

                offset
            }

            MIRTypeKind::Union { variants, .. } => variants
                .iter()
                .map(|(_, t)| t.type_size())
                .max()
                .unwrap_or(0),

            MIRTypeKind::Array { size, inner_type } => size * inner_type.type_size(),

            MIRTypeKind::TaggedUnion { variants, .. } => {
                variants
                    .iter()
                    .map(|(_, t)| t.type_size())
                    .max()
                    .unwrap_or(0)
                    + 1
            }

            MIRTypeKind::Function { .. } => unreachable!(),
        }
    }

    pub fn padded_size(&self) -> u64 {
        let size = self.type_size() as u64;
        let align = self.type_alignment() as u64;
        size.div_ceil(align) * align
    }

    pub fn type_alignment(&self) -> usize {
        match &self.kind {
            MIRTypeKind::Integer { _type, .. } => _type.bytes().min(8),
            MIRTypeKind::Float { _type } => _type.bytes().min(8),
            MIRTypeKind::Bool => 1,
            MIRTypeKind::Unit => 1,
            MIRTypeKind::Opaque { size, .. } => (*size).min(8),
            MIRTypeKind::MemoryReference(_) | MIRTypeKind::PointerTo { .. } => {
                std::mem::size_of::<usize>()
            }

            MIRTypeKind::Structured { fields, .. } => fields
                .iter()
                .map(|(_, t)| t.type_alignment())
                .max()
                .unwrap_or(8),
            MIRTypeKind::Union { variants, .. } => variants
                .iter()
                .map(|(_, t)| t.type_alignment())
                .max()
                .unwrap_or(8),

            MIRTypeKind::Array { inner_type, .. } => inner_type.type_alignment(),

            MIRTypeKind::TaggedUnion { variants, .. } => variants
                .iter()
                .map(|(_, t)| t.type_alignment())
                .max()
                .unwrap_or(8),

            MIRTypeKind::Function { .. } => unreachable!(),
        }
    }

    /*
     *   With things like type constructors, they are not per-se function pointers as they
     *   cannot be called via a function pointer syntax, so this serves as a shortcut to avoid
     *   having to write out a full type when it is not used.
     */
    pub fn internal_function() -> Self {
        MIRType::new(
            0,
            MIRTypeKind::Function {
                prototype: Box::new(MIRFunctionPrototype {
                    name: CXIdent::from("__internal_function"),
                    return_type: MIRType::unit(),
                    params: vec![],
                    var_args: false,
                    contract: CXFunctionContract::default(),
                }),
            },
        )
    }
}

impl From<MIRTypeKind> for MIRType {
    fn from(kind: MIRTypeKind) -> Self {
        MIRType {
            visibility: VisibilityMode::Private,
            specifiers: 0,
            kind,
        }
    }
}

pub fn same_type(t1: &MIRType, t2: &MIRType) -> bool {
    let t1_name = t1.get_name();
    let t2_name = t2.get_name();

    if t1_name.is_some() && t2_name.is_some() {
        return t1_name == t2_name;
    }

    match (&t1.kind, &t2.kind) {
        (
            MIRTypeKind::Array {
                inner_type: t1_type,
                ..
            },
            MIRTypeKind::Array {
                inner_type: t2_type,
                ..
            },
        ) => same_type(t1_type, t2_type),

        (
            MIRTypeKind::PointerTo {
                inner_type: t1_type,
                ..
            },
            MIRTypeKind::PointerTo {
                inner_type: t2_type,
                ..
            },
        ) => same_type(t1_type, t2_type),

        (
            MIRTypeKind::Structured {
                fields: t1_fields,
                copyable: c1,
                ..
            },
            MIRTypeKind::Structured {
                fields: t2_fields,
                copyable: c2,
                ..
            },
        ) => {
            c1 == c2
                && t1_fields
                    .iter()
                    .zip(t2_fields.iter())
                    .all(|(f1, f2)| same_type(&f1.1, &f2.1))
        }

        (MIRTypeKind::Function { prototype: p1 }, MIRTypeKind::Function { prototype: p2 }) => {
            same_type(&p1.return_type, &p2.return_type)
                && p1
                    .params
                    .iter()
                    .zip(p2.params.iter())
                    .all(|(a1, a2)| same_type(&a1._type, &a2._type))
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

        (MIRTypeKind::Bool, MIRTypeKind::Bool) | (MIRTypeKind::Unit, MIRTypeKind::Unit) => true,

        _ => false,
    }
}
