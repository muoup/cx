use crate::{
    MIRIntType,
    ty::{MIRFloatType, MIRTypeID},
    unit::function::MIRFunctionID,
    value::{MIRGlobalRef, MIRPlaceID, MIRTarget, MIRValue},
};

#[derive(Debug, Clone)]
pub enum MIRIntrinsic {
    Int(MIRIntIntrinsic),
    Float(MIRFloatIntrinsic),
    Pointer(MIRPtrIntrinsic),
    Aggregate(MIRAggregateIntrinsic),

    Internal(MIRInternalIntrinsic),

    VA(MIRVAIntrinsic),
}

#[derive(Debug, Clone)]
pub enum MIRIntIntrinsic {
    Neg {
        out: MIRTarget,
        value: MIRValue,
    },
    LNot {
        out: MIRTarget,
        value: MIRValue,
    },
    BNot {
        out: MIRTarget,
        value: MIRValue,
    },

    ToFloat {
        out: MIRTarget,
        value: MIRValue,
        target: MIRFloatType,
        signed: bool,
    },
    IntCast {
        out: MIRTarget,
        value: MIRValue,
        target: MIRIntType,
        sign_extend: bool,
    },
    ToPtr {
        out: MIRTarget,
        value: MIRValue,
        sign_extend: bool,
    },

    Add {
        out: MIRTarget,
        lhs: MIRValue,
        rhs: MIRValue,
    },
    Sub {
        out: MIRTarget,
        lhs: MIRValue,
        rhs: MIRValue,
    },

    UMul {
        out: MIRTarget,
        lhs: MIRValue,
        rhs: MIRValue,
    },
    SMul {
        out: MIRTarget,
        lhs: MIRValue,
        rhs: MIRValue,
    },
    UDiv {
        out: MIRTarget,
        lhs: MIRValue,
        rhs: MIRValue,
    },
    SDiv {
        out: MIRTarget,
        lhs: MIRValue,
        rhs: MIRValue,
    },
    UMod {
        out: MIRTarget,
        lhs: MIRValue,
        rhs: MIRValue,
    },
    SMod {
        out: MIRTarget,
        lhs: MIRValue,
        rhs: MIRValue,
    },

    Eq {
        out: MIRTarget,
        lhs: MIRValue,
        rhs: MIRValue,
    },
    Neq {
        out: MIRTarget,
        lhs: MIRValue,
        rhs: MIRValue,
    },
    ULt {
        out: MIRTarget,
        lhs: MIRValue,
        rhs: MIRValue,
    },
    SLt {
        out: MIRTarget,
        lhs: MIRValue,
        rhs: MIRValue,
    },
    ULe {
        out: MIRTarget,
        lhs: MIRValue,
        rhs: MIRValue,
    },
    SLe {
        out: MIRTarget,
        lhs: MIRValue,
        rhs: MIRValue,
    },
    UGt {
        out: MIRTarget,
        lhs: MIRValue,
        rhs: MIRValue,
    },
    SGt {
        out: MIRTarget,
        lhs: MIRValue,
        rhs: MIRValue,
    },
    UGe {
        out: MIRTarget,
        lhs: MIRValue,
        rhs: MIRValue,
    },
    SGe {
        out: MIRTarget,
        lhs: MIRValue,
        rhs: MIRValue,
    },

    LAnd {
        out: MIRTarget,
        lhs: MIRValue,
        rhs: MIRValue,
    },
    LOr {
        out: MIRTarget,
        lhs: MIRValue,
        rhs: MIRValue,
    },
    BAnd {
        out: MIRTarget,
        lhs: MIRValue,
        rhs: MIRValue,
    },
    BOr {
        out: MIRTarget,
        lhs: MIRValue,
        rhs: MIRValue,
    },
    BXor {
        out: MIRTarget,
        lhs: MIRValue,
        rhs: MIRValue,
    },

    LShift {
        out: MIRTarget,
        lhs: MIRValue,
        rhs: MIRValue,
    },
    ARShift {
        out: MIRTarget,
        lhs: MIRValue,
        rhs: MIRValue,
    },
    LRShift {
        out: MIRTarget,
        lhs: MIRValue,
        rhs: MIRValue,
    },
}

#[derive(Debug, Clone)]
pub enum MIRFloatIntrinsic {
    Neg {
        out: MIRTarget,
        value: MIRValue,
    },
    ToInt {
        out: MIRTarget,
        value: MIRValue,
        target_ty: MIRTypeID,
        signed: bool,
    },
    FloatCast {
        out: MIRTarget,
        value: MIRValue,
        float_ty: MIRFloatType,
    },
    Add {
        out: MIRTarget,
        lhs: MIRValue,
        rhs: MIRValue,
    },
    Sub {
        out: MIRTarget,
        lhs: MIRValue,
        rhs: MIRValue,
    },
    Mul {
        out: MIRTarget,
        lhs: MIRValue,
        rhs: MIRValue,
    },
    Div {
        out: MIRTarget,
        lhs: MIRValue,
        rhs: MIRValue,
    },

    Eq {
        out: MIRTarget,
        lhs: MIRValue,
        rhs: MIRValue,
    },
    Neq {
        out: MIRTarget,
        lhs: MIRValue,
        rhs: MIRValue,
    },
    Lt {
        out: MIRTarget,
        lhs: MIRValue,
        rhs: MIRValue,
    },
    Le {
        out: MIRTarget,
        lhs: MIRValue,
        rhs: MIRValue,
    },
    Gt {
        out: MIRTarget,
        lhs: MIRValue,
        rhs: MIRValue,
    },
    Geq {
        out: MIRTarget,
        lhs: MIRValue,
        rhs: MIRValue,
    },
}

#[derive(Debug, Clone)]
pub enum MIRPtrIntrinsic {
    ToInt {
        out: MIRTarget,
        ptr: MIRValue,
        target_ty: MIRTypeID,
    },

    Add {
        out: MIRTarget,
        ptr: MIRValue,
        offset: MIRValue,
    },
    Sub {
        out: MIRTarget,
        ptr: MIRValue,
        offset: MIRValue,
    },

    Diff {
        out: MIRTarget,
        lhs: MIRValue,
        rhs: MIRValue,
        element_ty: MIRTypeID,
    },
    Eq {
        out: MIRTarget,
        lhs: MIRValue,
        rhs: MIRValue,
    },
    Neq {
        out: MIRTarget,
        lhs: MIRValue,
        rhs: MIRValue,
    },
    Lt {
        out: MIRTarget,
        lhs: MIRValue,
        rhs: MIRValue,
    },
    Leq {
        out: MIRTarget,
        lhs: MIRValue,
        rhs: MIRValue,
    },
    Gt {
        out: MIRTarget,
        lhs: MIRValue,
        rhs: MIRValue,
    },
    Geq {
        out: MIRTarget,
        lhs: MIRValue,
        rhs: MIRValue,
    },
}

#[derive(Debug, Clone)]
pub enum MIRAggregateIntrinsic {
    SumIndex {
        out: MIRTarget,
        value: MIRValue,
        sum_ty: MIRTypeID,
    },
    SumVariant {
        out: MIRTarget,
        base: MIRValue,
        variant: usize,
        sum_ty: MIRTypeID,
    },
    SumVariantL {
        out: MIRPlaceID,
        source: MIRValue,
        variant: usize,
        sum_ty: MIRTypeID,
    },

    AggregateInit {
        out: MIRTarget,
        ty: MIRTypeID,
        fields: Vec<(usize, MIRValue)>,
    },
    StructField {
        out: MIRTarget,
        base: MIRValue,
        field: usize,
        struct_ty: MIRTypeID,
    },

    ArrayIndex {
        out: MIRTarget,
        base: MIRValue,
        index: MIRValue,
        element_ty: MIRTypeID,
    },
}

#[derive(Debug, Clone)]
pub enum MIRInternalIntrinsic {
    AdoptPlace {
        place: MIRPlaceID,
        address: MIRValue,
    },
    PlaceAddress {
        out: MIRTarget,
        place: MIRPlaceID,
    },
    GlobalAddress {
        out: MIRTarget,
        global: MIRGlobalRef,
    },
    ReferenceAddress {
        out: MIRTarget,
        reference: MIRValue,
    },
    ArrayAddress {
        out: MIRTarget,
        array: MIRValue,
    },
    StringAddress {
        out: MIRTarget,
        string: String,
    },
    GetFnPtr {
        out: MIRTarget,
        fn_id: MIRFunctionID,
    },
    Bitcast {
        out: MIRTarget,
        value: MIRValue,
        target_ty: MIRTypeID,
    },

    Assert {
        condition: MIRValue,
        message: Option<String>,
    },
    Assume {
        condition: MIRValue,
    },
}

#[derive(Debug, Clone)]
pub enum MIRVAIntrinsic {
    VaStart {
        list: MIRValue,
        last: MIRValue,
    },
    VaEnd {
        list: MIRValue,
    },
    VaArg {
        out: MIRTarget,
        list: MIRValue,
        ty: MIRTypeID,
    },
}

impl From<MIRIntIntrinsic> for MIRIntrinsic {
    fn from(value: MIRIntIntrinsic) -> Self {
        MIRIntrinsic::Int(value)
    }
}

impl From<MIRFloatIntrinsic> for MIRIntrinsic {
    fn from(value: MIRFloatIntrinsic) -> Self {
        MIRIntrinsic::Float(value)
    }
}

impl From<MIRPtrIntrinsic> for MIRIntrinsic {
    fn from(value: MIRPtrIntrinsic) -> Self {
        MIRIntrinsic::Pointer(value)
    }
}

impl From<MIRAggregateIntrinsic> for MIRIntrinsic {
    fn from(value: MIRAggregateIntrinsic) -> Self {
        MIRIntrinsic::Aggregate(value)
    }
}

impl From<MIRInternalIntrinsic> for MIRIntrinsic {
    fn from(value: MIRInternalIntrinsic) -> Self {
        MIRIntrinsic::Internal(value)
    }
}

impl From<MIRVAIntrinsic> for MIRIntrinsic {
    fn from(value: MIRVAIntrinsic) -> Self {
        MIRIntrinsic::VA(value)
    }
}

impl MIRIntrinsic {
    pub fn output_target(&self) -> Option<MIRTarget> {
        match self {
            Self::Int(op) => Some(match op {
                MIRIntIntrinsic::Neg { out, .. }
                | MIRIntIntrinsic::LNot { out, .. }
                | MIRIntIntrinsic::BNot { out, .. }
                | MIRIntIntrinsic::ToFloat { out, .. }
                | MIRIntIntrinsic::IntCast { out, .. }
                | MIRIntIntrinsic::ToPtr { out, .. }
                | MIRIntIntrinsic::Add { out, .. }
                | MIRIntIntrinsic::Sub { out, .. }
                | MIRIntIntrinsic::UMul { out, .. }
                | MIRIntIntrinsic::SMul { out, .. }
                | MIRIntIntrinsic::UDiv { out, .. }
                | MIRIntIntrinsic::SDiv { out, .. }
                | MIRIntIntrinsic::UMod { out, .. }
                | MIRIntIntrinsic::SMod { out, .. }
                | MIRIntIntrinsic::Eq { out, .. }
                | MIRIntIntrinsic::Neq { out, .. }
                | MIRIntIntrinsic::ULt { out, .. }
                | MIRIntIntrinsic::SLt { out, .. }
                | MIRIntIntrinsic::ULe { out, .. }
                | MIRIntIntrinsic::SLe { out, .. }
                | MIRIntIntrinsic::UGt { out, .. }
                | MIRIntIntrinsic::SGt { out, .. }
                | MIRIntIntrinsic::UGe { out, .. }
                | MIRIntIntrinsic::SGe { out, .. }
                | MIRIntIntrinsic::LAnd { out, .. }
                | MIRIntIntrinsic::LOr { out, .. }
                | MIRIntIntrinsic::BAnd { out, .. }
                | MIRIntIntrinsic::BOr { out, .. }
                | MIRIntIntrinsic::BXor { out, .. }
                | MIRIntIntrinsic::LShift { out, .. }
                | MIRIntIntrinsic::ARShift { out, .. }
                | MIRIntIntrinsic::LRShift { out, .. } => *out,
            }),
            Self::Float(op) => Some(match op {
                MIRFloatIntrinsic::Neg { out, .. }
                | MIRFloatIntrinsic::ToInt { out, .. }
                | MIRFloatIntrinsic::FloatCast { out, .. }
                | MIRFloatIntrinsic::Add { out, .. }
                | MIRFloatIntrinsic::Sub { out, .. }
                | MIRFloatIntrinsic::Mul { out, .. }
                | MIRFloatIntrinsic::Div { out, .. }
                | MIRFloatIntrinsic::Eq { out, .. }
                | MIRFloatIntrinsic::Neq { out, .. }
                | MIRFloatIntrinsic::Lt { out, .. }
                | MIRFloatIntrinsic::Le { out, .. }
                | MIRFloatIntrinsic::Gt { out, .. }
                | MIRFloatIntrinsic::Geq { out, .. } => *out,
            }),
            Self::Pointer(op) => Some(match op {
                MIRPtrIntrinsic::ToInt { out, .. }
                | MIRPtrIntrinsic::Add { out, .. }
                | MIRPtrIntrinsic::Sub { out, .. }
                | MIRPtrIntrinsic::Diff { out, .. }
                | MIRPtrIntrinsic::Eq { out, .. }
                | MIRPtrIntrinsic::Neq { out, .. }
                | MIRPtrIntrinsic::Lt { out, .. }
                | MIRPtrIntrinsic::Leq { out, .. }
                | MIRPtrIntrinsic::Gt { out, .. }
                | MIRPtrIntrinsic::Geq { out, .. } => *out,
            }),
            Self::Aggregate(op) => match op {
                MIRAggregateIntrinsic::SumVariantL { .. } => None,
                MIRAggregateIntrinsic::SumIndex { out, .. }
                | MIRAggregateIntrinsic::SumVariant { out, .. }
                | MIRAggregateIntrinsic::AggregateInit { out, .. }
                | MIRAggregateIntrinsic::StructField { out, .. }
                | MIRAggregateIntrinsic::ArrayIndex { out, .. } => Some(*out),
            },
            Self::Internal(op) => match op {
                MIRInternalIntrinsic::AdoptPlace { .. } => None,
                MIRInternalIntrinsic::PlaceAddress { out, .. }
                | MIRInternalIntrinsic::GlobalAddress { out, .. }
                | MIRInternalIntrinsic::ReferenceAddress { out, .. }
                | MIRInternalIntrinsic::ArrayAddress { out, .. }
                | MIRInternalIntrinsic::StringAddress { out, .. }
                | MIRInternalIntrinsic::GetFnPtr { out, .. }
                | MIRInternalIntrinsic::Bitcast { out, .. } => Some(*out),
                MIRInternalIntrinsic::Assert { .. } | MIRInternalIntrinsic::Assume { .. } => None,
            },
            Self::VA(op) => match op {
                MIRVAIntrinsic::VaArg { out, .. } => Some(*out),
                MIRVAIntrinsic::VaStart { .. } | MIRVAIntrinsic::VaEnd { .. } => None,
            },
        }
    }
}
