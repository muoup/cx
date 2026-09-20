use crate::{ty::{MIRFloatType, MIRTypeID}, unit::function::MIRFunctionID, value::{MIRPlaceID, MIRTarget, MIRValue}};

#[derive(Debug, Clone)]
pub enum MIRIntrinsic {
    Int(MIRIntIntrinsic),
    Float(MIRFloatIntrinsic),
    Pointer(MIRPtrIntrinsic),

    Internal(MIRInternalIntrinsic),

    VA(MIRVAIntrinsic),
}

#[derive(Debug, Clone)]
pub enum MIRIntIntrinsic {
    Neg         { out: MIRTarget, value: MIRValue },
    LNot        { out: MIRTarget, value: MIRValue },
    BNot        { out: MIRTarget, value: MIRValue },

    ToFloat     { out: MIRTarget, value: MIRValue, target_ty: MIRTypeID },
    ToPtr       { out: MIRTarget, value: MIRValue, target_ty: MIRTypeID },
    IntCast     { out: MIRTarget, value: MIRValue, target_ty: MIRTypeID, sign_extend: bool },
    
    Add         { out: MIRTarget, lhs: MIRValue, rhs: MIRValue },
    Sub         { out: MIRTarget, lhs: MIRValue, rhs: MIRValue },

    UMul        { out: MIRTarget, lhs: MIRValue, rhs: MIRValue },
    SMul        { out: MIRTarget, lhs: MIRValue, rhs: MIRValue },
    UDiv        { out: MIRTarget, lhs: MIRValue, rhs: MIRValue },
    SDiv        { out: MIRTarget, lhs: MIRValue, rhs: MIRValue },
    UMod        { out: MIRTarget, lhs: MIRValue, rhs: MIRValue },
    SMod        { out: MIRTarget, lhs: MIRValue, rhs: MIRValue },
    
    Eq          { out: MIRTarget, lhs: MIRValue, rhs: MIRValue },
    Neq         { out: MIRTarget, lhs: MIRValue, rhs: MIRValue },
    ULt         { out: MIRTarget, lhs: MIRValue, rhs: MIRValue },
    SLt         { out: MIRTarget, lhs: MIRValue, rhs: MIRValue },
    ULe         { out: MIRTarget, lhs: MIRValue, rhs: MIRValue },
    SLe         { out: MIRTarget, lhs: MIRValue, rhs: MIRValue },
    UGt         { out: MIRTarget, lhs: MIRValue, rhs: MIRValue },
    SGt         { out: MIRTarget, lhs: MIRValue, rhs: MIRValue },
    UGe         { out: MIRTarget, lhs: MIRValue, rhs: MIRValue },
    SGe         { out: MIRTarget, lhs: MIRValue, rhs: MIRValue },

    LAnd        { out: MIRTarget, lhs: MIRValue, rhs: MIRValue },
    LOr         { out: MIRTarget, lhs: MIRValue, rhs: MIRValue },
    BAnd        { out: MIRTarget, lhs: MIRValue, rhs: MIRValue },
    BOr         { out: MIRTarget, lhs: MIRValue, rhs: MIRValue },
    BXor        { out: MIRTarget, lhs: MIRValue, rhs: MIRValue },

    LShift      { out: MIRTarget, lhs: MIRValue, rhs: MIRValue },
    ARShift     { out: MIRTarget, lhs: MIRValue, rhs: MIRValue },
    LRShift     { out: MIRTarget, lhs: MIRValue, rhs: MIRValue },
}

#[derive(Debug, Clone)]
pub enum MIRFloatIntrinsic {
    Neg         { out: MIRTarget, value: MIRValue },
    ToInt       { out: MIRTarget, value: MIRValue, target_ty: MIRTypeID },
    FloatCast   { out: MIRTarget, value: MIRValue, float_ty: MIRFloatType },
    
    Eq          { out: MIRTarget, lhs: MIRValue, rhs: MIRValue },
    Neq         { out: MIRTarget, lhs: MIRValue, rhs: MIRValue },
    Lt          { out: MIRTarget, lhs: MIRValue, rhs: MIRValue },
    Le          { out: MIRTarget, lhs: MIRValue, rhs: MIRValue },
    Gt          { out: MIRTarget, lhs: MIRValue, rhs: MIRValue },
    Geq         { out: MIRTarget, lhs: MIRValue, rhs: MIRValue },
}

#[derive(Debug, Clone)]
pub enum MIRPtrIntrinsic {
    ToInt       { out: MIRTarget, ptr: MIRValue, target_ty: MIRTypeID },

    Add         { out: MIRTarget, ptr: MIRValue, offset: MIRValue },
    Sub         { out: MIRTarget, ptr: MIRValue, offset: MIRValue },

    Diff        { out: MIRTarget, lhs: MIRValue, rhs: MIRValue },
    Eq          { out: MIRTarget, lhs: MIRValue, rhs: MIRValue },
    Neq         { out: MIRTarget, lhs: MIRValue, rhs: MIRValue },
    Lt          { out: MIRTarget, lhs: MIRValue, rhs: MIRValue },
    Leq         { out: MIRTarget, lhs: MIRValue, rhs: MIRValue },
    Gt          { out: MIRTarget, lhs: MIRValue, rhs: MIRValue },
    Geq         { out: MIRTarget, lhs: MIRValue, rhs: MIRValue },
}

pub enum MIRAggregateIntrinsic {
    SumIndex    { out: MIRTarget, value: MIRValue, sum_ty: MIRTypeID },
    SumVariant  { out: MIRTarget, base: MIRPlaceID, variant: usize, sum_ty: MIRTypeID },
    SumVariantL { out: MIRTarget, base: MIRValue, variant: usize, sum_ty: MIRTypeID },
    
    StructInit  { out: MIRTarget, ty: MIRTypeID, fields: Vec<(usize, MIRValue)> },
    StructField { out: MIRTarget, base: MIRValue, field: usize, struct_ty: MIRTypeID },
    
    ArrayIndex  { out: MIRTarget, base: MIRValue, index: MIRValue, element_ty: MIRTypeID },
}

#[derive(Debug, Clone)]
pub enum MIRInternalIntrinsic {
    GetFnPtr    { out: MIRTarget, fn_id: MIRFunctionID },
    Bitcast     { out: MIRTarget, value: MIRValue, target_ty: MIRTypeID },
    
    Assert      { condition: MIRValue, message: Option<String> },
    Assume      { condition: MIRValue },
}

#[derive(Debug, Clone)]
pub enum MIRVAIntrinsic {
    VaStart     { list: MIRValue, last: MIRValue },
    VaEnd       { list: MIRValue },
    VaArg       { out: MIRTarget, list: MIRValue, ty: MIRTypeID },
}
