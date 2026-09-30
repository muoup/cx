use cx_util::unsafe_float::FloatWrapper;

use crate::ids::{HMIRMetaID, HMIRObjLocalID};

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
