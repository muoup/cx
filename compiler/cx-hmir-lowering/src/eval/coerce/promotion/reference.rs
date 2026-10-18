use cx_hmir::{
    ty::{HMIRTypeID, context::HMIRTypeContext},
    value::HMIRValue,
};

use crate::{
    emit,
    env::HMIREnvironment,
    eval::coerce::{
        pending::{Coercion, Obstacle},
        promotion::decay::decays,
    },
};

// A reference stands for the storage it refers to. Using that storage as a value copies it, which a @nocopy
// type forbids; a moved value is no longer a reference, so it never meets this rule.
pub(crate) fn try_referent(env: &HMIREnvironment, value: &HMIRValue) -> Coercion {
    match env.reference_inner(value.ty()) {
        Some(inner) if uncopyable(env, inner) => Coercion::obstructed(Obstacle::Uncopyable),
        Some(_) => Coercion::apply(|context, value, span| emit::referent(context, value, span)),
        None => Coercion::unapplied(),
    }
}

// As above, but the storage is also read, unless it is about to decay to its address instead
pub(crate) fn try_load(env: &HMIREnvironment, value: &HMIRValue) -> Coercion {
    match env.reference_inner(value.ty()) {
        Some(inner) if decays(env, inner) => try_referent(env, value),
        Some(inner) if uncopyable(env, inner) => Coercion::obstructed(Obstacle::Uncopyable),
        Some(_) => Coercion::apply(|context, value, span| {
            let value = emit::referent(context, value, span)?;
            emit::read(context, value, span)
        }),
        None => Coercion::unapplied(),
    }
}

fn uncopyable(env: &HMIREnvironment, inner: HMIRTypeID) -> bool {
    !decays(env, inner)
        && env
            .resolve_type_id(inner)
            .semantics()
            .move_semantics
            .is_nocopy()
}
