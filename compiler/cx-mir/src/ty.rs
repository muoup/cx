pub mod comparison;
pub mod interface;
pub mod layout;
pub mod registry;

use cx_util::dense_id;

pub use layout::{MIRFieldLayout, MIRTypeLayout};

use crate::unit::function::MIRFnSignature;

dense_id!(MIRTypeID, "t");

#[derive(Debug, Clone, PartialEq, Eq, Hash)]
pub struct MIRType {
    kind: MIRTypeKind,
    metadata: MIRTypeMetadata,
}

impl MIRType {
    pub fn new(kind: MIRTypeKind) -> Self {
        Self::qualified(kind, MIRTypeMetadata::NONE)
    }

    pub fn qualified(kind: MIRTypeKind, metadata: MIRTypeMetadata) -> Self {
        Self { kind, metadata }
    }

    pub fn kind(&self) -> &MIRTypeKind {
        &self.kind
    }

    pub fn metadata(&self) -> MIRTypeMetadata {
        self.metadata
    }
}

// Descriptive only: never read for semantics, and ignored by structural type comparison
#[derive(Debug, Clone, Copy, Default, PartialEq, Eq, Hash)]
pub struct MIRTypeMetadata(u8);

impl MIRTypeMetadata {
    pub const NONE: Self = Self(0);
    pub const CONST: Self = Self(1);
    pub const VOLATILE: Self = Self(1 << 1);
    pub const RESTRICT: Self = Self(1 << 2);

    pub const fn with(self, other: Self) -> Self {
        Self(self.0 | other.0)
    }

    pub const fn contains(self, other: Self) -> bool {
        self.0 & other.0 == other.0
    }

    pub const fn is_empty(self) -> bool {
        self.0 == 0
    }
}

// A declared type that was never defined; interacting with it requires an operation that asserts what it cannot
// provide, and its tag only lets headers rebuild its declaration. A flexible array is the trailing member of a
// struct, and the only undefined type with a layout: it occupies no storage but is aligned as its element
#[derive(Debug, Clone, Copy, PartialEq, Eq, Hash)]
pub enum MIRUndefined {
    Struct,
    Union,
    TaggedUnion,
    Enum,
    Array { element: MIRTypeID, flexible: bool },
}

#[derive(Debug, Clone, Copy, PartialEq, Eq, PartialOrd, Ord, Hash)]
pub enum MIRIntType {
    I1,
    I8,
    I16,
    I32,
    I64,
    I128,
}

impl MIRIntType {
    pub const fn bytes(self) -> usize {
        match self {
            Self::I1 | Self::I8 => 1,
            Self::I16 => 2,
            Self::I32 => 4,
            Self::I64 => 8,
            Self::I128 => 16,
        }
    }

    pub const fn bits(self) -> usize {
        match self {
            Self::I1 => 1,
            _ => self.bytes() * 8,
        }
    }

    pub const fn decode(self, value: i128, signed: bool) -> i128 {
        let shift = 128 - self.bits();
        if signed {
            (value << shift) >> shift
        } else {
            (((value as u128) << shift) >> shift) as i128
        }
    }

    pub const fn from_bytes(bytes: u8) -> Option<Self> {
        match bytes {
            1 => Some(Self::I8),
            2 => Some(Self::I16),
            4 => Some(Self::I32),
            8 => Some(Self::I64),
            16 => Some(Self::I128),
            _ => None,
        }
    }
}

#[derive(Debug, Clone, Copy, PartialEq, Eq, PartialOrd, Ord, Hash)]
pub enum MIRFloatType {
    F32,
    F64,
}

impl MIRFloatType {
    pub const fn bytes(self) -> usize {
        match self {
            Self::F32 => 4,
            Self::F64 => 8,
        }
    }
}

#[derive(Debug, Clone, PartialEq, Eq, Hash)]
pub enum MIRField {
    Standard {
        name: Option<String>,
        type_id: MIRTypeID,
    },
    Bitfield {
        name: Option<String>,
        integer_type_id: MIRTypeID,
        width: usize,
    },
}

impl MIRField {
    pub const fn standard(type_id: MIRTypeID) -> Self {
        Self::Standard {
            name: None,
            type_id,
        }
    }

    pub fn named(name: String, type_id: MIRTypeID) -> Self {
        Self::Standard {
            name: Some(name),
            type_id,
        }
    }

    pub fn name(&self) -> Option<&str> {
        match self {
            Self::Standard { name, .. } => name.as_deref(),
            Self::Bitfield { name, .. } => name.as_deref(),
        }
    }

    pub const fn ty(&self) -> MIRTypeID {
        match self {
            Self::Standard { type_id, .. }
            | Self::Bitfield {
                integer_type_id: type_id,
                ..
            } => *type_id,
        }
    }
}

/// The location of a bitfield within its storage unit, as consumed by a bitfield-aware store.
#[derive(Debug, Clone, Copy, PartialEq, Eq)]
pub struct MIRBitfieldAccess {
    pub bit_offset: usize,
    pub bit_width: usize,
    pub signed: bool,
}

#[derive(Debug, Clone, PartialEq, Eq, Hash)]
pub enum MIRTypeKind {
    Void,

    Integer { ty: MIRIntType },
    Float { ty: MIRFloatType },

    Structured { fields: Vec<MIRField> },
    Union { variants: Vec<MIRField> },
    TaggedUnion { variants: Vec<MIRField> },

    PointerTo { inner: MIRTypeID },
    MemoryReference { inner: MIRTypeID },
    Array { length: usize, inner: MIRTypeID },
    Function { signature: MIRFnSignature },

    // A complete type whose structure only intrinsics interpret
    Builtin { name: String, layout: MIRTypeLayout },
    Undefined(MIRUndefined),

    Str,
}
