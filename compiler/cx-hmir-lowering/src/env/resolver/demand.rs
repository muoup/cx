use rustc_hash::FxHashSet;
use cx_hmir::ty::{HMIRTypeID, HMIRTypeKind, HMIRUndefined, context::HMIRTypeContext};
use cx_log::CXResult;

use crate::{
    env::HMIREnvironment,
    eval::{symbol, types},
};

// Evaluates the pending definition 'ty' names, and those of the elements it is an array of
pub(crate) fn force(env: &mut HMIREnvironment, ty: HMIRTypeID) -> CXResult<()> {
    let ty = env.underlying(ty);
    if let Some(def) = env.resolver().pending(ty) {
        symbol::complete(env, def, ty)?;
    }
    match *env.resolve_kind(ty) {
        HMIRTypeKind::Array { element, .. }
        | HMIRTypeKind::Undefined(HMIRUndefined::Array { element }) => force(env, element),
        _ => Ok(()),
    }
}

pub(crate) fn complete(env: &mut HMIREnvironment, ty: HMIRTypeID) -> CXResult<bool> {
    force(env, ty)?;
    Ok(types::is_complete(env, ty))
}

// Forces and validates every type 'ty' is built from; a type reaching no undefined tag is not walked again. While a
// symbol is being evaluated nothing is forced, as what 'ty' reaches may refer back to it; 'ty' is instead prepared
// once the outermost evaluation is done, and what the symbol holds by value it demands itself
pub(crate) fn prepare(env: &mut HMIREnvironment, ty: HMIRTypeID) -> CXResult<()> {
    let evaluating = !env.resolver().active().is_empty();
    let mut seen = FxHashSet::default();
    if walk(env, ty, &mut seen, !evaluating)? {
        for ty in seen {
            env.resolver_mut().mark_prepared(ty);
        }
    } else if evaluating {
        env.resolver_mut().defer(ty);
    }
    Ok(())
}

// Prepares the types left unforced by the evaluation that just finished, once no other is in progress
pub(crate) fn drain(env: &mut HMIREnvironment) -> CXResult<()> {
    if !env.resolver().active().is_empty() {
        return Ok(());
    }
    while let Some(ty) = env.resolver_mut().take_deferred() {
        prepare(env, ty)?;
    }
    Ok(())
}

fn walk(
    env: &mut HMIREnvironment,
    ty: HMIRTypeID,
    seen: &mut FxHashSet<HMIRTypeID>,
    forcing: bool,
) -> CXResult<bool> {
    if env.resolver().prepared(ty) || !seen.insert(ty) {
        return Ok(true);
    }
    if forcing {
        force(env, ty)?;
    }
    let children = match env.resolve_kind(ty).clone() {
        HMIRTypeKind::Undefined(HMIRUndefined::Forward(_)) => return Ok(false),
        HMIRTypeKind::PointerTo(inner)
        | HMIRTypeKind::ReferenceTo { inner, .. }
        | HMIRTypeKind::Nominal(inner)
        | HMIRTypeKind::Staged(inner)
        | HMIRTypeKind::Array { element: inner, .. }
        | HMIRTypeKind::Undefined(HMIRUndefined::Array { element: inner }) => vec![inner],
        HMIRTypeKind::Function(signature) => signature
            .params()
            .iter()
            .map(|param| *param.ty())
            .chain(std::iter::once(*signature.return_type()))
            .collect(),
        HMIRTypeKind::Struct { fields }
        | HMIRTypeKind::Union { fields }
        | HMIRTypeKind::TaggedUnion { fields } => fields.iter().map(|field| *field.ty()).collect(),
        _ => Vec::new(),
    };
    let mut complete = true;
    for child in children {
        complete &= walk(env, child, seen, forcing)?;
    }
    types::validate(env, env.resolve_type_id(ty))?;
    Ok(complete)
}
