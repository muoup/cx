use cx_util::{identifier::CXIdent, unsafe_float::FloatWrapper};

use crate::{
    binding::HMIRObjLocalID,
    expr::{meta::HMIRMetaID, obj::HMIRObjID},
};

#[derive(Debug, Clone)]
pub enum HMIRInitializer {
    Struct(Vec<(usize, HMIRObjID)>),
    Array(Vec<HMIRObjID>),
    Variant { index: usize, value: HMIRObjID },
    Designated(Vec<(Option<CXIdent>, HMIRObjID)>),
}

#[derive(Debug, Clone)]
pub enum HMIRPattern {
    Binding(HMIRObjLocalID),
    Integer(i64),
    Float(FloatWrapper),
    Variant {
        sum: HMIRMetaID,
        index: usize,
        inner: Option<HMIRObjLocalID>,
    },
}
