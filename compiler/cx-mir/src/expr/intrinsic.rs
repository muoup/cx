use crate::{MIRFloatType, MIRTypeID, unit::MIRFunctionID, value::{MIRRegisterID, MIRTemporaryID, MIRValue}};

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
    Neg         { out: MIRRegisterID, value: MIRValue },
    LNot        { out: MIRRegisterID, value: MIRValue },
    BNot        { out: MIRRegisterID, value: MIRValue },

    ToFloat     { out: MIRRegisterID, value: MIRValue, target_ty: MIRTypeID },
    ToPtr       { out: MIRRegisterID, value: MIRValue, target_ty: MIRTypeID },
    IntCast     { out: MIRRegisterID, value: MIRValue, target_ty: MIRTypeID, sign_extend: bool },
    
    Add         { out: MIRRegisterID, lhs: MIRValue, rhs: MIRValue },
    Sub         { out: MIRRegisterID, lhs: MIRValue, rhs: MIRValue },

    UMul        { out: MIRRegisterID, lhs: MIRValue, rhs: MIRValue },
    SMul        { out: MIRRegisterID, lhs: MIRValue, rhs: MIRValue },
    UDiv        { out: MIRRegisterID, lhs: MIRValue, rhs: MIRValue },
    SDiv        { out: MIRRegisterID, lhs: MIRValue, rhs: MIRValue },
    UMod        { out: MIRRegisterID, lhs: MIRValue, rhs: MIRValue },
    SMod        { out: MIRRegisterID, lhs: MIRValue, rhs: MIRValue },
    
    Eq          { out: MIRRegisterID, lhs: MIRValue, rhs: MIRValue },
    Neq         { out: MIRRegisterID, lhs: MIRValue, rhs: MIRValue },
    ULt         { out: MIRRegisterID, lhs: MIRValue, rhs: MIRValue },
    SLt         { out: MIRRegisterID, lhs: MIRValue, rhs: MIRValue },
    ULe         { out: MIRRegisterID, lhs: MIRValue, rhs: MIRValue },
    SLe         { out: MIRRegisterID, lhs: MIRValue, rhs: MIRValue },
    UGt         { out: MIRRegisterID, lhs: MIRValue, rhs: MIRValue },
    SGt         { out: MIRRegisterID, lhs: MIRValue, rhs: MIRValue },
    UGe         { out: MIRRegisterID, lhs: MIRValue, rhs: MIRValue },
    SGe         { out: MIRRegisterID, lhs: MIRValue, rhs: MIRValue },

    LAnd        { out: MIRRegisterID, lhs: MIRValue, rhs: MIRValue },
    LOr         { out: MIRRegisterID, lhs: MIRValue, rhs: MIRValue },
    BAnd        { out: MIRRegisterID, lhs: MIRValue, rhs: MIRValue },
    BOr         { out: MIRRegisterID, lhs: MIRValue, rhs: MIRValue },
    BXor        { out: MIRRegisterID, lhs: MIRValue, rhs: MIRValue },

    LShift      { out: MIRRegisterID, lhs: MIRValue, rhs: MIRValue },
    ARShift     { out: MIRRegisterID, lhs: MIRValue, rhs: MIRValue },
    LRShift     { out: MIRRegisterID, lhs: MIRValue, rhs: MIRValue },
}

#[derive(Debug, Clone)]
pub enum MIRFloatIntrinsic {
    Neg         { out: MIRRegisterID, value: MIRValue },
    ToInt       { out: MIRRegisterID, value: MIRValue, target_ty: MIRTypeID },
    FloatCast   { out: MIRRegisterID, value: MIRValue, float_ty: MIRFloatType },
    
    Eq          { out: MIRRegisterID, lhs: MIRValue, rhs: MIRValue },
    Neq         { out: MIRRegisterID, lhs: MIRValue, rhs: MIRValue },
    Lt          { out: MIRRegisterID, lhs: MIRValue, rhs: MIRValue },
    Le          { out: MIRRegisterID, lhs: MIRValue, rhs: MIRValue },
    Gt          { out: MIRRegisterID, lhs: MIRValue, rhs: MIRValue },
    Geq         { out: MIRRegisterID, lhs: MIRValue, rhs: MIRValue },
}

#[derive(Debug, Clone)]
pub enum MIRPtrIntrinsic {
    ToInt       { out: MIRRegisterID, ptr: MIRValue, target_ty: MIRTypeID },

    Add         { out: MIRRegisterID, ptr: MIRValue, offset: MIRValue },
    Sub         { out: MIRRegisterID, ptr: MIRValue, offset: MIRValue },

    Diff        { out: MIRRegisterID, lhs: MIRValue, rhs: MIRValue },
    Eq          { out: MIRRegisterID, lhs: MIRValue, rhs: MIRValue },
    Neq         { out: MIRRegisterID, lhs: MIRValue, rhs: MIRValue },
    Lt          { out: MIRRegisterID, lhs: MIRValue, rhs: MIRValue },
    Leq         { out: MIRRegisterID, lhs: MIRValue, rhs: MIRValue },
    Gt          { out: MIRRegisterID, lhs: MIRValue, rhs: MIRValue },
    Geq         { out: MIRRegisterID, lhs: MIRValue, rhs: MIRValue },
}

pub enum MIRAggregateIntrinsic {
    SumIndex    { out: MIRRegisterID, value: MIRValue, sum_ty: MIRTypeID },
    SumVariant  { out: MIRRegisterID, base: MIRValue, variant: usize, sum_ty: MIRTypeID },
    SumVariantL { out: MIRTemporaryID, base: MIRValue, variant: usize, sum_ty: MIRTypeID },
    
    StructInit  { out: MIRRegisterID, ty: MIRTypeID, fields: Vec<(usize, MIRValue)> },
    StructField { out: MIRRegisterID, base: MIRValue, field: usize, struct_ty: MIRTypeID },
    
    ArrayIndex  { out: MIRRegisterID, base: MIRValue, index: MIRValue, element_ty: MIRTypeID },
}

#[derive(Debug, Clone)]
pub enum MIRInternalIntrinsic {
    GetFnPtr    { out: MIRRegisterID, fn_id: MIRFunctionID },
    Bitcast     { out: MIRRegisterID, value: MIRValue, target_ty: MIRTypeID },
    
    Assert      { condition: MIRValue, message: Option<String> },
    Assume      { condition: MIRValue },
}

#[derive(Debug, Clone)]
pub enum MIRVAIntrinsic {
    VaStart     { list: MIRValue, last: MIRValue },
    VaEnd       { list: MIRValue },
    VaArg       { out: MIRRegisterID, list: MIRValue, ty: MIRTypeID },
}
