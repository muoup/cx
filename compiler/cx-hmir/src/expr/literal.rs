use cx_util::unsafe_float::FloatWrapper;

use crate::ty::HMIRTypeID;

// Literal syntax as written, typed by the unit's type table; evaluation produces the value
#[derive(Debug, Clone)]
pub enum HMIRLiteral {
    Unit,
    Int { value: i128, ty: HMIRTypeID },
    Float { value: FloatWrapper, ty: HMIRTypeID },
    Str(String),
}
