use cx_hmir::{
    ty::{HMIRTypeID, HMIRTypeKind, context::HMIRTypeContext},
    value::HMIRValue,
};

use crate::{
    env::HMIREnvironment,
    eval::coerce::{
        implicit::{self, cast, retype},
        pending::Coercion,
    },
};

// A cast performs any implicit conversion, and additionally converts between unrelated pointers, and from
// integers to pointers (C11 6.5.4)
pub(crate) fn rule(env: &HMIREnvironment, value: &HMIRValue, target: HMIRTypeID) -> Coercion {
    implicit::rule(env, value, target).or_else(|| try_pointer_cast(env, value, target))
}

fn try_pointer_cast(env: &HMIREnvironment, value: &HMIRValue, target: HMIRTypeID) -> Coercion {
    match (env.resolve_kind(value.ty()), env.resolve_kind(target)) {
        (HMIRTypeKind::PointerTo(_), HMIRTypeKind::PointerTo(_)) => retype(target),
        (HMIRTypeKind::Int(_), HMIRTypeKind::PointerTo(_)) => cast(target),
        _ => Coercion::unapplied(),
    }
}
