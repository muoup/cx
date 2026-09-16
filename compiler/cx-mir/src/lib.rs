pub mod body;
pub mod comptime;
pub mod diagnostic;
pub mod global;
pub mod instruction;
pub mod op;
mod query;
pub mod staged;
pub mod ty;
pub mod unit;
pub mod visit;

pub(crate) mod format;
pub(crate) mod log;

pub use format::MIRDisplay;
pub use log::layout_error;

pub use body::MIRBody;
pub use comptime::{MIRComptimeBody, MIRComptimeInstr, MIRComptimeInstrKind, MIRComptimeOp};
pub use diagnostic::{MIRDiagnostic, MIRDiagnosticLocation};
pub use global::{
    MIRFnParam, MIRFnPrototype, MIRFnSignature, MIRFunction, MIRFunctionBody, MIRFunctionID,
    MIRFunctionMode, MIRGlobalID, MIRGlobalKind, MIRGlobalState, MIRGlobalVariable, MIRPlaceDecl,
    MIRRegisterDecl, MIRScopeDecl,
};
pub use instruction::{
    MIRAggregateOp, MIRBasicBlock, MIRBasicBlockID, MIRBlockTarget, MIRConstant, MIRInstrKind,
    MIRInstruction, MIRInstructionLike, MIRIntrinsic, MIRPlaceID, MIRRegister, MIRScopeID,
    MIRStagedExitKind, MIRStagedTargets, MIRTarget, MIRTargetAggregateOp, MIRValue,
    MIRValueAggregateOp,
};
pub use op::{
    MIRBinaryOp, MIRCoercion, MIRFloatBinaryOp, MIRIntBinaryOp, MIRPointerBinaryOp,
    MIRPointerOffsetOp, MIRUnaryOp,
};
pub use staged::{
    MIRStagedBasicBlock, MIRStagedBody, MIRStagedCapture, MIRStagedInstr, MIRStagedInstrKind,
    MIRStagedTemplate,
};
pub use ty::{
    MIRBitfieldAccess, MIRField, MIRFieldLayout, MIRFloatType, MIRFunctionType, MIRIntType,
    MIRLayoutError, MIRType, MIRTypeID, MIRTypeKind, MIRTypeLayout,
};
pub use unit::MIRUnit;
