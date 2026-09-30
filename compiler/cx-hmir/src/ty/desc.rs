use crate::ids::{HMIRNominalID, HMIRTypeID};

#[derive(Debug, Clone, Copy, PartialEq, Eq, PartialOrd, Ord, Hash)]
pub enum HMIRIntWidth {
    I1,
    I8,
    I16,
    I32,
    I64,
    I128,
}

#[derive(Debug, Clone, Copy, PartialEq, Eq, PartialOrd, Ord, Hash)]
pub enum HMIRFloatWidth {
    F32,
    F64,
}

#[derive(Debug, Clone, PartialEq, Eq, Hash)]
pub struct HMIRFnTypeDesc {
    params: Vec<HMIRTypeID>,
    ret: HMIRTypeID,
    variadic: bool,
}

#[derive(Debug, Clone, PartialEq, Eq, Hash)]
pub enum HMIRTypeDesc {
    Void,
    Unreachable,
    Type,
    Str,
    Int {
        width: HMIRIntWidth,
        signed: bool,
    },
    Float {
        width: HMIRFloatWidth,
    },
    Pointer(HMIRTypeID),
    Reference(HMIRTypeID),
    Array {
        element: HMIRTypeID,
        length: Option<u64>,
    },
    Function(HMIRFnTypeDesc),
    Expr {
        params: Vec<HMIRTypeID>,
        result: HMIRTypeID,
    },
    Nominal(HMIRNominalID),
    Opaque {
        size: usize,
        alignment: usize,
    },
}

impl HMIRIntWidth {
    pub const fn bits(self) -> usize {
        match self {
            Self::I1 => 1,
            Self::I8 => 8,
            Self::I16 => 16,
            Self::I32 => 32,
            Self::I64 => 64,
            Self::I128 => 128,
        }
    }
}

impl HMIRFloatWidth {
    pub const fn bits(self) -> usize {
        match self {
            Self::F32 => 32,
            Self::F64 => 64,
        }
    }
}

impl HMIRFnTypeDesc {
    pub fn new(params: Vec<HMIRTypeID>, ret: HMIRTypeID, variadic: bool) -> Self {
        Self {
            params,
            ret,
            variadic,
        }
    }

    pub fn params(&self) -> &[HMIRTypeID] {
        &self.params
    }

    pub fn ret(&self) -> HMIRTypeID {
        self.ret
    }

    pub fn is_variadic(&self) -> bool {
        self.variadic
    }
}
