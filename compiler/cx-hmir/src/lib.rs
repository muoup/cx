pub mod binding;
pub mod body;
pub mod expr;
pub mod ty;
pub mod type_def;
pub mod unit;

mod format;

pub use binding::{HMIRHole, HMIRHoleID, HMIRLocal, HMIRMetaLocalID, HMIRObjLocalID};
pub use body::HMIRBody;
pub use def::{HMIRDef, HMIRDefID, HMIRDefKind, HMIRDefRef, HMIRGlobal};
pub use expr::aggregate::{HMIRInitializer, HMIRPattern};
pub use expr::intrinsic::{HMIRControlIntrinsic, HMIRObjIntrinsic};
pub use expr::meta::{HMIRMetaExpr, HMIRMetaID, HMIRMetaKind};
pub use expr::native_op::{HMIRBinaryOp, HMIRCoerceMode, HMIRUnaryOp};
pub use expr::obj::{HMIRObjExpr, HMIRObjID, HMIRObjKind};
pub use expr::type_op::HMIRTypeOp;
pub use expr::{HMIRConstant, HMIRExprID};
pub use function::{HMIRContract, HMIRFunction, HMIRFunctionRoot, HMIRParam, HMIRSignature};
pub use ty::desc::{HMIRFloatWidth, HMIRFnTypeDesc, HMIRIntWidth, HMIRTypeDesc, HMIRTypeID};
pub use ty::interner::HMIRTypeInterner;
pub use ty::nominal::{
    HMIRAggregateKind, HMIRFieldDesc, HMIRMoveSemantics, HMIRNominalDesc, HMIRNominalID,
};
pub use type_def::{HMIRFieldDef, HMIRTypeDef, HMIRTypeDefKind};
pub use unit::HMIRUnit;
