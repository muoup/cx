pub mod aggregate;
pub mod kind;
pub mod native_op;
pub mod type_op;

use cx_util::unsafe_float::FloatWrapper;

use crate::ty::desc::HMIRTypeID;

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
