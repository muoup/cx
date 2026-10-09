use cx_target::ArchitectureConfig;

#[derive(Debug, Clone, Copy, PartialEq, Eq, PartialOrd, Ord, Hash)]
pub struct HMIRIntType {
    width: HMIRIntWidth,
    signed: bool,
}

#[derive(Debug, Clone, Copy, PartialEq, Eq, PartialOrd, Ord, Hash)]
pub enum HMIRIntWidth {
    I1,
    I8,
    I16,
    I32,
    I64,
    I128,
}

impl HMIRIntWidth {
    pub fn pointer_width(arch: &ArchitectureConfig) -> Self {
        Self::from_bytes(arch.pointer_size() as usize)
            .expect("ArchitectureConfig guarantees a supported pointer size")
    }
    
    pub const fn bytes(self) -> usize {
        match self {
            Self::I1 => 1,
            Self::I8 => 1,
            Self::I16 => 2,
            Self::I32 => 4,
            Self::I64 => 8,
            Self::I128 => 16,
        }
    }

    pub const fn from_bytes(bytes: usize) -> Option<Self> {
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

impl HMIRIntType {
    pub const fn new(width: HMIRIntWidth, signed: bool) -> Self {
        Self { width, signed }
    }

    pub const fn width(&self) -> HMIRIntWidth {
        self.width
    }

    pub const fn signed(&self) -> bool {
        self.signed
    }
}

#[derive(Debug, Clone, Copy, PartialEq, Eq, PartialOrd, Ord, Hash)]
pub struct HMIRFloatType {
    width: HMIRFloatWidth,
}

#[derive(Debug, Clone, Copy, PartialEq, Eq, PartialOrd, Ord, Hash)]
pub enum HMIRFloatWidth {
    F32,
    F64,
}

impl HMIRFloatWidth {
    pub const fn bytes(self) -> usize {
        match self {
            Self::F32 => 4,
            Self::F64 => 8,
        }
    }

    pub const fn from_bytes(bytes: usize) -> Option<Self> {
        match bytes {
            4 => Some(Self::F32),
            8 => Some(Self::F64),
            _ => None,
        }
    }
}

impl HMIRFloatType {
    pub const fn new(width: HMIRFloatWidth) -> Self {
        Self { width }
    }

    pub const fn width(&self) -> HMIRFloatWidth {
        self.width
    }
}
