use cx_hmir::ty::{
    HMIRTypeID, HMIRTypeKind,
    aggregate::HMIRMoveSemantics,
    context::HMIRTypeContext,
    intrinsic::{HMIRFloatType, HMIRFloatWidth, HMIRIntType, HMIRIntWidth},
};
use cx_mir::{MIRFloatType, MIRIntType};
use cx_tokens::TokenRange;

use crate::env::{HMIREnvironment, lowering::FnLoweringContext};

#[derive(Debug, Clone, Copy, PartialEq, Eq)]
pub(crate) enum TypeShape {
    Void,
    Unreachable,
    Int(HMIRIntType),
    Float(HMIRFloatType),
    Pointer(HMIRTypeID),
    Reference,
    Decays,
    Other,
}

pub(crate) fn shape(cx: &mut FnLoweringContext, ty: HMIRTypeID) -> TypeShape {
    match cx.env().resolve_type_id(ty).kind() {
        HMIRTypeKind::Void => TypeShape::Void,
        HMIRTypeKind::Unreachable => TypeShape::Unreachable,
        HMIRTypeKind::Int(int) => TypeShape::Int(*int),
        HMIRTypeKind::Float(float) => TypeShape::Float(*float),
        HMIRTypeKind::PointerTo(inner) => TypeShape::Pointer(*inner),
        HMIRTypeKind::ReferenceTo(_) => TypeShape::Reference,
        HMIRTypeKind::Array { .. } | HMIRTypeKind::Str | HMIRTypeKind::Function(_) => {
            TypeShape::Decays
        }
        _ => TypeShape::Other,
    }
}

pub(crate) fn is_nodrop(env: &HMIREnvironment, ty: HMIRTypeID) -> bool {
    *env.resolve_type_id(ty).semantics() == HMIRMoveSemantics::Nodrop
}

pub(crate) fn mir_int(width: HMIRIntWidth) -> MIRIntType {
    match width {
        HMIRIntWidth::I1 => MIRIntType::I1,
        HMIRIntWidth::I8 => MIRIntType::I8,
        HMIRIntWidth::I16 => MIRIntType::I16,
        HMIRIntWidth::I32 => MIRIntType::I32,
        HMIRIntWidth::I64 => MIRIntType::I64,
        HMIRIntWidth::I128 => MIRIntType::I128,
    }
}

pub(crate) fn mir_float(width: HMIRFloatWidth) -> MIRFloatType {
    match width {
        HMIRFloatWidth::F32 => MIRFloatType::F32,
        HMIRFloatWidth::F64 => MIRFloatType::F64,
    }
}

pub(crate) fn int_type(
    cx: &mut FnLoweringContext,
    width: HMIRIntWidth,
    signed: bool,
    span: &TokenRange,
) -> HMIRTypeID {
    let kind = HMIRTypeKind::Int(HMIRIntType::new(width, signed));
    cx.env().realize_type(kind, span)
}

pub(crate) fn bool_type(cx: &mut FnLoweringContext, span: &TokenRange) -> HMIRTypeID {
    int_type(cx, HMIRIntWidth::I1, false, span)
}

pub(crate) fn is_bool(int: HMIRIntType) -> bool {
    int.width() == HMIRIntWidth::I1
}

pub(crate) fn promote_type(
    cx: &mut FnLoweringContext,
    ty: HMIRTypeID,
    span: &TokenRange,
) -> HMIRTypeID {
    match shape(cx, ty) {
        TypeShape::Int(int) if int.width() < HMIRIntWidth::I32 => {
            int_type(cx, HMIRIntWidth::I32, true, span)
        }
        _ => ty,
    }
}

pub(crate) fn arithmetic_type(
    cx: &mut FnLoweringContext,
    lhs: HMIRTypeID,
    rhs: HMIRTypeID,
    span: &TokenRange,
) -> Option<HMIRTypeID> {
    let lhs = promote_type(cx, lhs, span);
    let rhs = promote_type(cx, rhs, span);

    match (shape(cx, lhs), shape(cx, rhs)) {
        (TypeShape::Float(left), TypeShape::Float(right)) => {
            let width = left.width().max(right.width());
            let kind = HMIRTypeKind::Float(HMIRFloatType::new(width));
            Some(cx.env().realize_type(kind, span))
        }
        (TypeShape::Float(_), TypeShape::Int(_)) => Some(lhs),
        (TypeShape::Int(_), TypeShape::Float(_)) => Some(rhs),
        (TypeShape::Int(left), TypeShape::Int(right)) => {
            let (width, signed) = if left.width() == right.width() {
                (left.width(), left.signed() && right.signed())
            } else if left.width() > right.width() {
                (left.width(), left.signed())
            } else {
                (right.width(), right.signed())
            };

            Some(int_type(cx, width, signed, span))
        }
        _ => None,
    }
}

pub(crate) fn size_of(cx: &mut FnLoweringContext, ty: HMIRTypeID) -> usize {
    let pointer_size = cx.env().architecture().pointer_size();
    let kind = cx.env().resolve_type_id(ty).kind().clone();

    match kind {
        HMIRTypeKind::Void => 1,
        HMIRTypeKind::Int(int) => int.width().bytes(),
        HMIRTypeKind::Float(float) => float.width().bytes(),
        HMIRTypeKind::PointerTo(_) => pointer_size,
        HMIRTypeKind::Opaque { size, .. } => size,
        HMIRTypeKind::Array {
            element,
            length: Some(length),
        } => size_of(cx, element) * length as usize,
        _ => todo!("size of a pointee that is not a scalar, pointer, opaque or sized array"),
    }
}

pub(crate) fn describe(cx: &mut FnLoweringContext, ty: HMIRTypeID) -> String {
    let kind = cx.env().resolve_type_id(ty).kind().clone();

    match kind {
        HMIRTypeKind::Void => "void".into(),
        HMIRTypeKind::Unreachable => "never".into(),
        HMIRTypeKind::Type => "@type".into(),
        HMIRTypeKind::Str => "str".into(),
        HMIRTypeKind::Int(int) => {
            let prefix = if int.signed() { "i" } else { "u" };
            format!("{prefix}{}", int.width().bytes() * 8)
        }
        HMIRTypeKind::Float(float) => format!("f{}", float.width().bytes() * 8),
        HMIRTypeKind::PointerTo(inner) => format!("{}*", describe(cx, inner)),
        HMIRTypeKind::ReferenceTo(inner) => format!("{}&", describe(cx, inner)),
        HMIRTypeKind::Array {
            element,
            length: Some(length),
        } => format!("{}[{length}]", describe(cx, element)),
        HMIRTypeKind::Array { element, .. } => format!("{}[]", describe(cx, element)),
        HMIRTypeKind::Function(_) => "function".into(),
        HMIRTypeKind::StagedExpr { .. } => "expr".into(),
        HMIRTypeKind::Struct { .. } => "struct".into(),
        HMIRTypeKind::Union { .. } => "union".into(),
        HMIRTypeKind::TaggedUnion { .. } => "tagged union".into(),
        HMIRTypeKind::Opaque { size, alignment } => format!("opaque({size}, {alignment})"),
    }
}
