pub mod binding;
pub mod body;
pub mod constant;
pub mod def;
pub mod expr;
pub mod function;
pub mod global;
pub mod ids;
pub mod ty;
pub mod type_def;
pub mod unit;

mod format;

pub use binding::{HMIRHole, HMIRLocal};
pub use body::HMIRBody;
pub use constant::HMIRConstant;
pub use def::{HMIRDef, HMIRDefKind, HMIRDefRef};
pub use expr::aggregate::HMIRInitializer;
pub use expr::meta::{HMIRMetaExpr, HMIRMetaKind};
pub use expr::obj::{HMIRObjExpr, HMIRObjKind};
pub use expr::operator::{HMIRBinaryOp, HMIRCoerceMode, HMIRUnaryOp};
pub use expr::pattern::HMIRPattern;
pub use expr::type_op::HMIRTypeOp;
pub use function::{HMIRContract, HMIRFunction, HMIRFunctionRoot, HMIRParam, HMIRSignature};
pub use global::HMIRGlobal;
pub use ids::{
    HMIRDefID, HMIRHoleID, HMIRMetaID, HMIRMetaLocalID, HMIRNominalID, HMIRObjID,
    HMIRObjLocalID, HMIRTypeID,
};
pub use ty::desc::{HMIRFloatWidth, HMIRFnTypeDesc, HMIRIntWidth, HMIRTypeDesc};
pub use ty::interner::HMIRTypeInterner;
pub use ty::nominal::{HMIRAggregateKind, HMIRFieldDesc, HMIRMoveSemantics, HMIRNominalDesc};
pub use type_def::{HMIRFieldDef, HMIRTypeDef, HMIRTypeDefKind};
pub use unit::HMIRUnit;

pub type HMIRObjIntrinsic = cx_intrinsics::Intrinsic<HMIRObjID, HMIRMetaID>;
pub type HMIRMetaIntrinsic = cx_intrinsics::Intrinsic<HMIRMetaID, HMIRMetaID>;
