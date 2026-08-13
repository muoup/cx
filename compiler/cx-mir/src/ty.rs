mod layout;
mod registry;

pub use layout::{MIRFieldLayout, MIRLayoutError, MIRTypeLayout};
pub use registry::MIRTypeRegistry;

#[derive(Debug, Clone, Copy, PartialEq, Eq, PartialOrd, Ord, Hash)]
pub struct MIRTypeID(pub u64);

impl MIRTypeID {
    pub const fn new(index: usize) -> Self {
        Self(index as u64)
    }

    pub const fn from_raw(id: u64) -> Self {
        Self(id)
    }

    pub const fn index(self) -> usize {
        self.0 as usize
    }

    pub const fn raw(self) -> u64 {
        self.0
    }
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
        type_id: MIRTypeID,
    },
    Bitfield {
        integer_type_id: MIRTypeID,
        width: usize,
    },
}

impl MIRField {
    pub const fn standard(type_id: MIRTypeID) -> Self {
        Self::Standard { type_id }
    }

    pub const fn ty(&self) -> MIRTypeID {
        match self {
            Self::Standard { type_id }
            | Self::Bitfield {
                integer_type_id: type_id,
                ..
            } => *type_id,
        }
    }
}

#[derive(Debug, Clone, PartialEq, Eq, Hash)]
pub struct MIRBitfieldAccess {
    pub storage_type: MIRTypeID,
    pub bit_offset: usize,
    pub bit_width: usize,
    pub signed: bool,
}

#[derive(Debug, Clone, PartialEq, Eq, Hash)]
pub struct MIRFunctionType {
    pub params: Vec<MIRTypeID>,
    pub return_type: MIRTypeID,
    pub variadic: bool,
}

#[derive(Debug, Clone, PartialEq, Eq, Hash)]
pub enum MIRTypeKind {
    Unit,
    Integer {
        ty: MIRIntType,
        signed: bool,
    },
    Float {
        ty: MIRFloatType,
    },
    Structured {
        fields: Vec<MIRField>,
    },
    Union {
        variants: Vec<MIRField>,
    },
    TaggedUnion {
        variants: Vec<MIRField>,
    },
    PointerTo {
        inner: MIRTypeID,
    },
    MemoryReference {
        inner: MIRTypeID,
        bitfield: Option<MIRBitfieldAccess>,
    },
    Array {
        length: usize,
        inner: MIRTypeID,
    },
    Function {
        signature: MIRFunctionType,
    },
    Opaque {
        size: usize,
        alignment: usize,
    },
    Undefined,
    Str,
}

#[derive(Debug, Clone, PartialEq, Eq, Hash)]
pub struct MIRTypeDefinition {
    pub kind: MIRTypeKind,
    pub minimum_alignment: Option<usize>,
}

impl MIRTypeDefinition {
    pub fn new(kind: MIRTypeKind) -> Self {
        Self {
            kind,
            minimum_alignment: None,
        }
    }
}
