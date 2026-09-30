pub mod aggregate;
pub mod intrinsic;
pub mod meta;
pub mod obj;
pub mod operator;
pub mod type_op;

use cx_util::unsafe_float::FloatWrapper;

use crate::{
    expr::{meta::HMIRMetaID, obj::HMIRObjID},
    ty::desc::HMIRTypeID,
};

#[derive(Debug, Clone, Copy, PartialEq, Eq, Hash)]
pub enum HMIRExprID {
    Meta(HMIRMetaID),
    Obj(HMIRObjID),
}

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
