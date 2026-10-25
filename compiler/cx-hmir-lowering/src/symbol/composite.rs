use cx_hmir::{
    HMIRFnSignature,
    ty::{HMIRTypeID, HMIRTypeKind, context::HMIRTypeContext},
};

use crate::env::HMIREnvironment;

pub(crate) fn alike(env: &HMIREnvironment, a: HMIRTypeID, b: HMIRTypeID, qualified: bool) -> bool {
    let (mut left, mut right) = (env.semantics(a), env.semantics(b));
    if !qualified {
        left.constant = false;
        right.constant = false;
    }
    if left != right {
        return false;
    }

    let (a, b) = (env.underlying(a), env.underlying(b));
    if a == b {
        return true;
    }
    let (a_id, b_id) = (a, b);
    let (a, b) = (env.resolve_type_id(a), env.resolve_type_id(b));
    match (a.kind(), b.kind()) {
        (HMIRTypeKind::Void, HMIRTypeKind::Void)
        | (HMIRTypeKind::Unreachable, HMIRTypeKind::Unreachable)
        | (HMIRTypeKind::Type, HMIRTypeKind::Type)
        | (HMIRTypeKind::Str, HMIRTypeKind::Str) => true,

        (HMIRTypeKind::Int(left), HMIRTypeKind::Int(right)) => left == right,
        (HMIRTypeKind::Float(left), HMIRTypeKind::Float(right)) => left == right,
        (HMIRTypeKind::FunctionItem(left), HMIRTypeKind::FunctionItem(right)) => left == right,

        (HMIRTypeKind::PointerTo(left), HMIRTypeKind::PointerTo(right))
        | (HMIRTypeKind::ReferenceTo(left), HMIRTypeKind::ReferenceTo(right)) => {
            alike(env, *left, *right, true)
        }
        (
            HMIRTypeKind::Array {
                element: left,
                length: left_length,
            },
            HMIRTypeKind::Array {
                element: right,
                length: right_length,
            },
        ) => left_length == right_length && alike(env, *left, *right, true),
        (
            HMIRTypeKind::StagedExpr {
                params: left,
                result: left_result,
            },
            HMIRTypeKind::StagedExpr {
                params: right,
                result: right_result,
            },
        ) => {
            left.len() == right.len()
                && left.iter().zip(right).all(|(l, r)| alike(env, *l, *r, true))
                && alike(env, *left_result, *right_result, true)
        }
        (HMIRTypeKind::Function(left), HMIRTypeKind::Function(right)) => {
            signatures_alike(env, left, right)
        }
        (
            HMIRTypeKind::Opaque {
                size: left_size,
                alignment: left_alignment,
            },
            HMIRTypeKind::Opaque {
                size: right_size,
                alignment: right_alignment,
            },
        ) => {
            !env.resolver().is_incomplete(env.underlying(a_id))
                && !env.resolver().is_incomplete(env.underlying(b_id))
                && (left_size, left_alignment) == (right_size, right_alignment)
                && a.debug_name() == b.debug_name()
        }

        _ => false,
    }
}

pub(crate) fn signatures_alike(
    env: &HMIREnvironment,
    a: &HMIRFnSignature<HMIRTypeID>,
    b: &HMIRFnSignature<HMIRTypeID>,
) -> bool {
    a.contract().is_safe() == b.contract().is_safe()
        && a.is_variadic() == b.is_variadic()
        && a.params().len() == b.params().len()
        && a.params()
            .iter()
            .zip(b.params())
            .all(|(a, b)| alike(env, *a.ty(), *b.ty(), false))
        && alike(env, *a.return_type(), *b.return_type(), false)
}

pub(crate) fn composite(env: &HMIREnvironment, a: HMIRTypeID, b: HMIRTypeID) -> Option<HMIRTypeID> {
    if alike(env, a, b, true) {
        return Some(a);
    }
    if env.semantics(a) != env.semantics(b) {
        return None;
    }

    let array = |ty| match *env.resolve_kind(ty) {
        HMIRTypeKind::Array { element, length } => Some((element, length.is_some())),
        _ => None,
    };
    let ((left, left_bound), (right, right_bound)) = (array(a)?, array(b)?);
    match (left_bound, right_bound) {
        (false, true) if alike(env, left, right, true) => Some(b),
        (true, false) if alike(env, left, right, true) => Some(a),
        _ => None,
    }
}
