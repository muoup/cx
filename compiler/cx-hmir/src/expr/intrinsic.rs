use cx_intrinsics::Intrinsic;
use cx_util::identifier::CXIdent;

use crate::{
    binding::HMIRObjLocalID,
    expr::{aggregate::HMIRPattern, meta::HMIRMetaID, obj::HMIRObjID},
};

#[derive(Debug, Clone)]
pub enum HMIRObjIntrinsic {
    Native(Intrinsic<HMIRObjID, HMIRMetaID>),
    Memory(HMIRMemoryIntrinsic),
    Access(HMIRAccessIntrinsic),
    Variant(HMIRVariantIntrinsic),
    Control(HMIRControlIntrinsic),
}

#[derive(Debug, Clone)]
pub enum HMIRMemoryIntrinsic {
    Load(HMIRObjID),
    Store { target: HMIRObjID, value: HMIRObjID },
}

#[derive(Debug, Clone)]
pub enum HMIRAccessIntrinsic {
    Member {
        base: HMIRObjID,
        name: CXIdent,
    },
    Field {
        base: HMIRObjID,
        index: usize,
        aggregate: HMIRMetaID,
    },
    Index {
        base: HMIRObjID,
        index: HMIRObjID,
        element: HMIRMetaID,
    },
    Unpack {
        value: HMIRObjID,
        bindings: Vec<(usize, HMIRObjLocalID)>,
    },
}

#[derive(Debug, Clone)]
pub enum HMIRVariantIntrinsic {
    Tag {
        value: HMIRObjID,
        sum: HMIRMetaID,
    },
    Set {
        target: HMIRObjID,
        index: usize,
        value: HMIRObjID,
        sum: HMIRMetaID,
    },
    Is {
        value: HMIRObjID,
        pattern: HMIRPattern,
    },
}

#[derive(Debug, Clone)]
pub enum HMIRControlIntrinsic {
    Branch(HMIRMetaID),
    Defer(HMIRObjID),
    Leak(HMIRObjID),
    Unsafe(HMIRObjID),
    Unreachable,
}

#[derive(Debug, Clone)]
pub enum HMIRMetaIntrinsic {
    Native(Intrinsic<HMIRMetaID, HMIRMetaID>),
    /// Break, Continue and Label resolve against the staging scope stack to a branch target,
    /// failing when no enclosing scope matches.
    Break,
    Continue,
    Label(CXIdent),
    Branch(HMIRMetaID),
    CompileError(HMIRMetaID),
}

impl HMIRObjIntrinsic {
    pub fn path(&self) -> &'static str {
        match self {
            Self::Native(intrinsic) => intrinsic.path(),
            Self::Memory(intrinsic) => intrinsic.path(),
            Self::Access(intrinsic) => intrinsic.path(),
            Self::Variant(intrinsic) => intrinsic.path(),
            Self::Control(intrinsic) => intrinsic.path(),
        }
    }
}

impl HMIRMemoryIntrinsic {
    pub fn path(&self) -> &'static str {
        match self {
            Self::Load(_) => "load",
            Self::Store { .. } => "store",
        }
    }
}

impl HMIRAccessIntrinsic {
    pub fn path(&self) -> &'static str {
        match self {
            Self::Member { .. } => "member",
            Self::Field { .. } => "field",
            Self::Index { .. } => "index",
            Self::Unpack { .. } => "unpack",
        }
    }
}

impl HMIRVariantIntrinsic {
    pub fn path(&self) -> &'static str {
        match self {
            Self::Tag { .. } => "variant.tag",
            Self::Set { .. } => "variant.set",
            Self::Is { .. } => "variant.is",
        }
    }
}

impl HMIRControlIntrinsic {
    pub fn path(&self) -> &'static str {
        match self {
            Self::Branch(_) => "branch",
            Self::Defer(_) => "defer",
            Self::Leak(_) => "leak",
            Self::Unsafe(_) => "unsafe",
            Self::Unreachable => "unreachable",
        }
    }
}

impl HMIRMetaIntrinsic {
    pub fn path(&self) -> &'static str {
        match self {
            Self::Native(intrinsic) => intrinsic.path(),
            Self::Break => "meta.break",
            Self::Continue => "meta.continue",
            Self::Label(_) => "meta.label",
            Self::Branch(_) => "meta.branch",
            Self::CompileError(_) => "meta.compile_error",
        }
    }
}

impl From<Intrinsic<HMIRObjID, HMIRMetaID>> for HMIRObjIntrinsic {
    fn from(intrinsic: Intrinsic<HMIRObjID, HMIRMetaID>) -> Self {
        Self::Native(intrinsic)
    }
}

impl From<HMIRMemoryIntrinsic> for HMIRObjIntrinsic {
    fn from(intrinsic: HMIRMemoryIntrinsic) -> Self {
        Self::Memory(intrinsic)
    }
}

impl From<HMIRAccessIntrinsic> for HMIRObjIntrinsic {
    fn from(intrinsic: HMIRAccessIntrinsic) -> Self {
        Self::Access(intrinsic)
    }
}

impl From<HMIRVariantIntrinsic> for HMIRObjIntrinsic {
    fn from(intrinsic: HMIRVariantIntrinsic) -> Self {
        Self::Variant(intrinsic)
    }
}

impl From<HMIRControlIntrinsic> for HMIRObjIntrinsic {
    fn from(intrinsic: HMIRControlIntrinsic) -> Self {
        Self::Control(intrinsic)
    }
}

impl From<Intrinsic<HMIRMetaID, HMIRMetaID>> for HMIRMetaIntrinsic {
    fn from(intrinsic: Intrinsic<HMIRMetaID, HMIRMetaID>) -> Self {
        Self::Native(intrinsic)
    }
}
