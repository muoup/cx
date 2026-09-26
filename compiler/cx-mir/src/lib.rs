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
    MIRBasicBlock, MIRInstruction, MIRInstructionKind, MIRInstructionLike, MIRScopeID,
};
pub use expr::intrinsic::{
    MIRAggregateIntrinsic, MIRFloatIntrinsic, MIRIntIntrinsic, MIRInternalIntrinsic, MIRIntrinsic,
    MIRPtrIntrinsic, MIRVAIntrinsic,
};

pub use constant::{MIRConstant, MIRStagedID};
pub use staged::MIRStagedExpression;
pub use ty::comptime::MIRComptimeType;
pub use ty::{
    MIRBitfieldAccess, MIRField, MIRFieldLayout, MIRFloatType, MIRIntType, MIRType, MIRTypeID,
    MIRTypeKind, MIRTypeLayout,
};
pub use unit::comptime_function::{
    MIRComptimeContext, MIRComptimeFnParam, MIRComptimeFnPrototype, MIRComptimeFnSignature,
    MIRComptimeFunction,
};
pub use unit::function::{MIRFnParam, MIRFnPrototype, MIRFnSignature, MIRFunction, MIRFunctionID};
pub use unit::{
    MIRBasicBlockID, MIRComptimeRegisterDecl, MIRGlobalID, MIRGlobalState, MIRGlobalVariable,
    MIRPlaceDecl, MIRRegisterDecl, MIRScopeDecl, MIRUnit,
};
pub use value::MIRRegisterID as MIRRegister;
pub use value::{
    MIRBindable, MIRBlockTarget, MIRComptimeOperand, MIRComptimeOutput, MIRComptimeParameter,
    MIRComptimeRegisterID, MIRComptimeValue, MIRGlobalRef, MIRLivenessState, MIRPlaceID,
    MIRRegisterID, MIRTarget, MIRValue,
};
