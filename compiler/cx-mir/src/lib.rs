pub mod constant;
pub mod expr;
pub mod staged;
pub mod ty;
pub mod unit;
pub mod value;

pub(crate) mod format;

pub use format::MIRDisplay;

pub use expr::body::MIRBody;
pub use expr::comptime::{MIRComptimeBody, MIRComptimeInstruction, MIRComptimeOp};
pub use expr::instruction::{
    MIRBasicBlock, MIRInstructionKind, MIRInstruction, MIRInstructionLike, MIRScopeID,
};
pub use expr::intrinsic::{
    MIRAggregateIntrinsic, MIRFloatIntrinsic, MIRIntIntrinsic, MIRInternalIntrinsic, MIRIntrinsic,
    MIRPtrIntrinsic, MIRVAIntrinsic,
};

pub use ty::{
    MIRBitfieldAccess, MIRField, MIRFieldLayout, MIRFloatType, MIRIntType, MIRType, MIRTypeID,
    MIRTypeKind, MIRTypeLayout,
};
pub use unit::{
    MIRBasicBlockID, MIRGlobalID, MIRGlobalState, MIRGlobalVariable, MIRPlaceDecl, MIRRegisterDecl,
    MIRScopeDecl, MIRUnit,
};
pub use unit::function::{
    MIRFnParam, MIRFnPrototype, MIRFnSignature, MIRFunction, MIRFunctionID,
};
pub use unit::comptime_function::{
    MIRComptimeContext, MIRComptimeFnParam, MIRComptimeFnPrototype, MIRComptimeFnSignature,
    MIRComptimeFunction,
};
pub use value::MIRRegisterID as MIRRegister;
pub use value::{
    MIRBindable, MIRBlockTarget, MIRPlaceID, MIRRegisterID, MIRTarget, MIRValue,
};
pub use constant::{MIRConstant, MIRConstantID, MIRStagedID};
pub use staged::MIRStagedExpression;
