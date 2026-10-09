use cx_util::unsafe_float::FloatWrapper;

use crate::ty::HMIRTypeID;

#[derive(Debug, Clone, PartialEq, Eq, Hash)]
pub enum HMIRConstant {
    Unit,
    Bool(bool),
    Int { value: i128, ty: HMIRTypeID },
    Float { value: FloatWrapper, ty: HMIRTypeID },
    Str(String),
    Null(HMIRTypeID),
    Type(HMIRTypeID),
}