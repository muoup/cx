pub mod aggregate;
pub mod context;
pub mod intrinsic;

use crate::ty::{
    aggregate::{HMIRFieldDef, HMIRMoveSemantics}, intrinsic::{FunctionType, HMIRFloatType, HMIRIntType},
};
use cx_tokens::TokenRange;
use cx_util::dense_id;

dense_id!(HMIRTypeID, "ty");
dense_id!(NominalID, "nominal");

#[derive(Debug, Clone)]
pub(crate) struct HMIRType {
    id: HMIRTypeID,
    kind: HMIRTypeKind,
    semantics: HMIRMoveSemantics,
    unsafe_move: bool,
    decl_span: TokenRange,
}

#[derive(Debug, Clone)]
pub(crate) enum HMIRTypeKind {
    Void,
    Unreachable,
    Type,
    Str,
    Int(HMIRIntType),
    Float(HMIRFloatType),
    PointerTo(HMIRTypeID),
    ReferenceTo(HMIRTypeID),
    Array {
        element: HMIRTypeID,
        length: Option<u64>,
    },
    Function(FunctionType),
    StagedExpr {
        params: Vec<HMIRTypeID>,
        result: HMIRTypeID,
    },
    Struct {
        fields: Vec<HMIRFieldDef>,
    },
    Union {
        fields: Vec<HMIRFieldDef>,
    },
    TaggedUnion {
        fields: Vec<HMIRFieldDef>,
    },
    Opaque {
        size: usize,
        alignment: usize,
    },
}
