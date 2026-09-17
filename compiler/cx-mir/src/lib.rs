pub mod expr;
pub mod ty;
pub mod unit;
pub mod value;

pub(crate) mod format;
pub(crate) mod log;

pub(crate) use expr::query;
pub use expr::visit;

pub use format::MIRDisplay;
pub use log::layout_error;

pub use expr::body::MIRBody;
pub use expr::comptime::{MIRComptimeBody, MIRComptimeInstr, MIRComptimeInstrKind, MIRComptimeOp};
pub use expr::instruction::{
    MIRBasicBlock, MIRInstrKind, MIRInstruction, MIRInstructionLike, MIRScopeID, MIRStagedExitKind,
    MIRStagedTargets,
};
pub use expr::intrinsic::{
    MIRAggregateIntrinsic, MIRFloatIntrinsic, MIRIntIntrinsic, MIRInternalIntrinsic, MIRIntrinsic,
    MIRPtrIntrinsic, MIRVAIntrinsic,
};
pub use expr::staged::{
    MIRStagedBasicBlock, MIRStagedBody, MIRStagedCapture, MIRStagedInstr, MIRStagedInstrKind,
    MIRStagedTemplate,
};

pub use ty::{
    MIRBitfieldAccess, MIRField, MIRFieldLayout, MIRFloatType, MIRFunctionType, MIRIntType,
    MIRLayoutError, MIRType, MIRTypeID, MIRTypeKind, MIRTypeLayout,
};
pub use unit::{
    MIRBasicBlockID, MIRFnParam, MIRFnPrototype, MIRFnSignature, MIRFunction, MIRFunctionBody,
    MIRFunctionID, MIRFunctionMode, MIRGlobalID, MIRGlobalKind, MIRGlobalState, MIRGlobalVariable,
    MIRPlaceDecl, MIRRegisterDecl, MIRScopeDecl, MIRUnit,
};
pub use value::MIRRegisterID as MIRRegister;
pub use value::{MIRBlockTarget, MIRConstant, MIRPlaceID, MIRRegisterID, MIRTarget, MIRValue};
