pub mod constant;
pub mod expr;
pub mod ty;
pub mod unit;
pub mod value;

pub(crate) mod format;

pub use format::MIRDisplay;

pub use expr::body::MIRBody;
pub use expr::instruction::{
    MIRBasicBlock, MIRInstruction, MIRInstructionKind, MIRScopeID, MIRStoreBitfield,
};
pub use expr::intrinsic::{
    MIRAggregateIntrinsic, MIRFloatIntrinsic, MIRIntIntrinsic, MIRInternalIntrinsic, MIRIntrinsic,
    MIRPtrIntrinsic, MIRVAIntrinsic,
};

pub use constant::MIRConstant;
pub use ty::{
    MIRBitfieldAccess, MIRField, MIRFieldLayout, MIRFloatType, MIRIntType, MIRType, MIRTypeID,
    MIRTypeKind, MIRTypeLayout,
};
pub use unit::function::{MIRFnParam, MIRFnPrototype, MIRFnSignature, MIRFunction, MIRFunctionID};
pub use unit::{
    MIRBasicBlockID, MIRGlobalID, MIRGlobalState, MIRGlobalVariable, MIRPlaceDecl, MIRRegisterDecl,
    MIRScopeDecl, MIRUnit,
};
pub use value::MIRRegisterID as MIRRegister;
pub use value::{
    MIRBindable, MIRBlockTarget, MIRGlobalRef, MIRLivenessState, MIRPlaceID, MIRRegisterID,
    MIRTarget, MIRValue,
};
