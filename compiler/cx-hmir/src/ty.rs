pub mod aggregate;
pub mod context;
pub mod intrinsic;

use crate::{
    HMIRFnSignature,
    ty::{
        aggregate::{HMIRFieldDef, HMIRTypeSemantics},
        intrinsic::{HMIRFloatType, HMIRIntType},
    },
};
use cx_tokens::TokenRange;
use cx_util::dense_id;

dense_id!(HMIRTypeID, "ty");
dense_id!(NominalID, "nominal");

#[derive(Debug, Clone)]
pub struct HMIRType {
    debug_name: Option<String>,
    decl_span: TokenRange,

    kind: HMIRTypeKind,
    semantics: HMIRTypeSemantics,
}

#[derive(Debug, Clone)]
pub enum HMIRTypeKind {
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
    Function(HMIRFnSignature),
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

impl HMIRType {
    pub fn new(
        debug_name: Option<String>,
        decl_span: TokenRange,
        kind: HMIRTypeKind,
        semantics: HMIRTypeSemantics,
    ) -> Self {
        Self {
            debug_name,
            kind,
            semantics,
            decl_span,
        }
    }

    pub fn kind(&self) -> &HMIRTypeKind {
        &self.kind
    }

    pub fn semantics(&self) -> &HMIRTypeSemantics {
        &self.semantics
    }

    pub fn decl_span(&self) -> &TokenRange {
        &self.decl_span
    }

    pub fn pointer_inner(&self) -> Option<&HMIRTypeID> {
        match &self.kind {
            HMIRTypeKind::PointerTo(inner) => Some(inner),
            _ => None,
        }
    }

    pub fn reference_inner(&self) -> Option<&HMIRTypeID> {
        match &self.kind {
            HMIRTypeKind::ReferenceTo(inner) => Some(inner),
            _ => None,
        }
    }

    pub fn array_inner(&self) -> Option<&HMIRTypeID> {
        match &self.kind {
            HMIRTypeKind::Array { element, .. } => Some(element),
            _ => None,
        }
    }
}
