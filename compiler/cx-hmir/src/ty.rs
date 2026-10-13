pub mod aggregate;
pub mod context;
pub mod intrinsic;

use crate::{
    HMIRFnSignature,
    expr::HMIRExprID,
    ty::{
        aggregate::{HMIRFieldDef, HMIRTypeSemantics},
        intrinsic::{HMIRFloatType, HMIRIntType},
    },
};
use cx_tokens::TokenRange;
use cx_util::dense_id;

dense_id!(HMIRTypeID, "ty");
dense_id!(NominalID, "nominal");

// An evaluated type, which refers to the types it is built from by their IDs
pub type HMIRType = HMIRGenericType<HMIRTypeID, u64>;
pub type HMIRTypeKind = HMIRGenericTypeKind<HMIRTypeID, u64>;

// A declared type, which refers to the types it is built from by the expressions that evaluate to them
pub type HMIRExprType = HMIRGenericType<HMIRExprID, HMIRExprID>;
pub type HMIRExprTypeKind = HMIRGenericTypeKind<HMIRExprID, HMIRExprID>;

#[derive(Debug, Clone)]
pub struct HMIRGenericType<T, L> {
    debug_name: Option<String>,
    decl_span: TokenRange,

    kind: HMIRGenericTypeKind<T, L>,
    semantics: HMIRTypeSemantics,
}

#[derive(Debug, Clone)]
pub enum HMIRGenericTypeKind<T, L> {
    Void,
    Unreachable,
    Type,
    Str,
    Int(HMIRIntType),
    Float(HMIRFloatType),
    PointerTo(T),
    ReferenceTo(T),
    Array { element: T, length: Option<L> },
    Function(HMIRFnSignature<T>),
    StagedExpr { params: Vec<T>, result: T },
    Struct { fields: Vec<HMIRFieldDef<T>> },
    Union { fields: Vec<HMIRFieldDef<T>> },
    TaggedUnion { fields: Vec<HMIRFieldDef<T>> },
    Opaque { size: usize, alignment: usize },
    // A transparent reference to another type, whose semantics only ever add specifiers to it
    Nominal(T),
}

impl<T, L> HMIRGenericType<T, L> {
    pub fn new(
        kind: HMIRGenericTypeKind<T, L>,
        semantics: HMIRTypeSemantics,
        decl_span: TokenRange,
        debug_name: Option<String>,
    ) -> Self {
        Self {
            debug_name,
            kind,
            semantics,
            decl_span,
        }
    }

    pub fn debug_name(&self) -> Option<&str> {
        self.debug_name.as_deref()
    }

    pub fn kind(&self) -> &HMIRGenericTypeKind<T, L> {
        &self.kind
    }

    pub fn semantics(&self) -> &HMIRTypeSemantics {
        &self.semantics
    }

    pub fn decl_span(&self) -> &TokenRange {
        &self.decl_span
    }

    pub fn pointer_inner(&self) -> Option<&T> {
        match &self.kind {
            HMIRGenericTypeKind::PointerTo(inner) => Some(inner),
            _ => None,
        }
    }

    pub fn reference_inner(&self) -> Option<&T> {
        match &self.kind {
            HMIRGenericTypeKind::ReferenceTo(inner) => Some(inner),
            _ => None,
        }
    }

    pub fn array_inner(&self) -> Option<&T> {
        match &self.kind {
            HMIRGenericTypeKind::Array { element, .. } => Some(element),
            _ => None,
        }
    }

    // 'state' is handed to whichever of the two is mapping, as both usually need the same context
    pub fn map<S, U, M, E>(
        &self,
        state: &mut S,
        ty: impl Fn(&mut S, &T) -> Result<U, E>,
        length: impl Fn(&mut S, &L) -> Result<M, E>,
    ) -> Result<HMIRGenericType<U, M>, E> {
        Ok(HMIRGenericType {
            debug_name: self.debug_name.clone(),
            decl_span: self.decl_span.clone(),
            kind: self.kind.map(state, ty, length)?,
            semantics: self.semantics,
        })
    }
}

impl<T, L> HMIRGenericTypeKind<T, L> {
    pub fn map<S, U, M, E>(
        &self,
        state: &mut S,
        ty: impl Fn(&mut S, &T) -> Result<U, E>,
        length: impl Fn(&mut S, &L) -> Result<M, E>,
    ) -> Result<HMIRGenericTypeKind<U, M>, E> {
        let fields = |state: &mut S, fields: &[HMIRFieldDef<T>]| {
            fields
                .iter()
                .map(|field| field.map(|field| ty(state, field)))
                .collect::<Result<Vec<_>, E>>()
        };

        Ok(match self {
            Self::Void => HMIRGenericTypeKind::Void,
            Self::Unreachable => HMIRGenericTypeKind::Unreachable,
            Self::Type => HMIRGenericTypeKind::Type,
            Self::Str => HMIRGenericTypeKind::Str,
            Self::Int(int) => HMIRGenericTypeKind::Int(*int),
            Self::Float(float) => HMIRGenericTypeKind::Float(*float),
            Self::Opaque { size, alignment } => HMIRGenericTypeKind::Opaque {
                size: *size,
                alignment: *alignment,
            },

            Self::PointerTo(inner) => HMIRGenericTypeKind::PointerTo(ty(state, inner)?),
            Self::ReferenceTo(inner) => HMIRGenericTypeKind::ReferenceTo(ty(state, inner)?),
            Self::Nominal(inner) => HMIRGenericTypeKind::Nominal(ty(state, inner)?),
            Self::Array {
                element,
                length: declared,
            } => HMIRGenericTypeKind::Array {
                element: ty(state, element)?,
                length: declared
                    .as_ref()
                    .map(|declared| length(state, declared))
                    .transpose()?,
            },
            Self::Function(signature) => {
                HMIRGenericTypeKind::Function(signature.map(|param| ty(state, param))?)
            }
            Self::StagedExpr { params, result } => HMIRGenericTypeKind::StagedExpr {
                params: params
                    .iter()
                    .map(|param| ty(state, param))
                    .collect::<Result<_, E>>()?,
                result: ty(state, result)?,
            },

            Self::Struct { fields: declared } => HMIRGenericTypeKind::Struct {
                fields: fields(state, declared)?,
            },
            Self::Union { fields: declared } => HMIRGenericTypeKind::Union {
                fields: fields(state, declared)?,
            },
            Self::TaggedUnion { fields: declared } => HMIRGenericTypeKind::TaggedUnion {
                fields: fields(state, declared)?,
            },
        })
    }
}
