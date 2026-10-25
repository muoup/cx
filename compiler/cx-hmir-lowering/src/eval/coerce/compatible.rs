use cx_hmir::ty::{HMIRTypeID, HMIRTypeKind, context::HMIRTypeContext};

use crate::{env::HMIREnvironment, eval::types};

pub fn compatible(env: &HMIREnvironment, lhs: HMIRTypeID, rhs: HMIRTypeID) -> bool {
    if env.same_type(lhs, rhs) {
        return true;
    }
    if env.semantics(lhs).move_semantics != env.semantics(rhs).move_semantics {
        return false;
    }

    match (env.resolve_kind(lhs), env.resolve_kind(rhs)) {
        (HMIRTypeKind::Staged(lhs), HMIRTypeKind::Staged(rhs)) => compatible(env, *lhs, *rhs),
        (HMIRTypeKind::PointerTo(lhs), HMIRTypeKind::PointerTo(rhs)) => {
            is_void(env, *lhs) || is_void(env, *rhs) || compatible(env, *lhs, *rhs)
        }
        (HMIRTypeKind::ReferenceTo(lhs), HMIRTypeKind::ReferenceTo(rhs)) => {
            compatible(env, *lhs, *rhs)
        }
        (
            HMIRTypeKind::Array {
                element: lhs,
                length: lhs_length,
            },
            HMIRTypeKind::Array {
                element: rhs,
                length: rhs_length,
            },
        ) => lhs_length == rhs_length && compatible(env, *lhs, *rhs),
        _ => false,
    }
}

pub fn qualified(env: &HMIREnvironment, from: HMIRTypeID, to: HMIRTypeID) -> bool {
    let pointee = |ty| env.pointer_inner(ty).or_else(|| env.reference_inner(ty));

    match (pointee(from), pointee(to)) {
        (Some(from), Some(to)) => {
            (!env.semantics(from).constant || env.semantics(to).constant)
                && (!env.semantics(from).volatile || env.semantics(to).volatile)
        }
        _ => true,
    }
}

pub fn is_void(env: &HMIREnvironment, ty: HMIRTypeID) -> bool {
    matches!(env.resolve_kind(ty), HMIRTypeKind::Void)
}

pub fn is_designator(env: &HMIREnvironment, ty: HMIRTypeID) -> bool {
    match env.resolve_kind(ty) {
        HMIRTypeKind::Str => true,
        HMIRTypeKind::Function(_) => !types::comptime_only(env, ty),
        _ => false,
    }
}
