use cx_util::identifier::CXIdent;

use crate::ids::HMIRObjID;

#[derive(Debug, Clone)]
pub enum HMIRInitializer {
    Struct(Vec<(usize, HMIRObjID)>),
    Array(Vec<HMIRObjID>),
    Variant { index: usize, value: HMIRObjID },
    Designated(Vec<(Option<CXIdent>, HMIRObjID)>),
}
