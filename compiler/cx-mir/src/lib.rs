pub mod diagnostic;
pub mod expr;
pub mod global;
pub mod op;
pub mod ty;
pub mod unit;

pub(crate) mod format;

pub use format::MIRDisplay;

pub use diagnostic::{MIRDiagnostic, MIRDiagnosticLocation};
pub use expr::{
    MIRAggregateOp, MIRAssignTarget, MIRBasicBlock, MIRBasicBlockID, MIRBlockTarget, MIRCallKind,
    MIRConstant, MIRInstr, MIRInstrKind, MIRInstrOperand, MIRParameterID, MIRPlace,
    MIRPlaceAggregateOp, MIRPlaceID, MIRRegister, MIRScopeID, MIRValue, MIRValueAggregateOp,
};
pub use global::{
    MIRFnParam, MIRFnPrototype, MIRFnSignature, MIRFunction, MIRFunctionDefinition, MIRFunctionID,
    MIRFunctionMode, MIRGlobalID, MIRGlobalState, MIRGlobalVariable, MIRPlaceDecl, MIRRegisterDecl,
    MIRScopeDecl,
};
pub use op::{
    MIRBinaryOp, MIRCoercion, MIRFloatBinaryOp, MIRIntBinaryOp, MIRPointerBinaryOp,
    MIRPointerOffsetOp, MIRUnaryOp,
};
pub use ty::{
    MIRBitfieldAccess, MIRField, MIRFieldLayout, MIRFloatType, MIRFunctionType, MIRIntType,
    MIRLayoutError, MIRType, MIRTypeID, MIRTypeKind, MIRTypeLayout, MIRTypeRegistryBuilder,
};
pub use unit::MIRUnit;
