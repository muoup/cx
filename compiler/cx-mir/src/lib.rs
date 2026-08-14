pub mod expr;
pub mod global;
pub mod op;
pub mod ty;
pub mod unit;
pub mod validator;

mod format;

pub use format::MIRDisplay;

pub use expr::{
    MIRAggregateOp, MIRBasicBlock, MIRBasicBlockID, MIRBlockTarget, MIRConstant, MIRInstr,
    MIRInstrKind, MIRInstrOperand, MIRParameterID, MIRPlace, MIRPlaceAggregateOp, MIRPlaceID,
    MIRRegister, MIRValue, MIRValueAggregateOp,
};
pub use global::{
    MIRFnParam, MIRFnPrototype, MIRFnSignature, MIRFunction, MIRFunctionID, MIRGlobalID,
    MIRGlobalState, MIRGlobalVariable, MIRPlaceDecl, MIRRegisterDecl,
};
pub use op::{
    MIRBinaryOp, MIRCoercion, MIRFloatBinaryOp, MIRIntBinaryOp, MIRPointerBinaryOp,
    MIRPointerOffsetOp, MIRUnaryOp,
};
pub use ty::{
    MIRBitfieldAccess, MIRField, MIRFieldLayout, MIRFloatType, MIRFunctionType, MIRIntType,
    MIRLayoutError, MIRTypeDefinition, MIRTypeID, MIRTypeKind, MIRTypeLayout, MIRTypeRegistry,
};
pub use unit::MIRUnit;
pub use validator::{MIRValidationError, validate};
