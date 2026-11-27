use rustc_hash::FxHashSet;
use chl_hmir::ty::{
    HMIRType, HMIRTypeID, HMIRTypeKind, HMIRUndefined, aggregate::HMIRTypeSemantics,
    context::HMIRTypeContext,
};
use chl_log::{CHLResult, catalogue::mir};
use chl_tokens::TokenRange;

use crate::{
    env::{HMIREnvironment, function::LocalLoweringContext},
    eval::name::type_value,
    log::hmir_error,
    value::HMIRValue,
};

// A type definition is the type it evaluates to. 'evaluate' builds an aggregate it defines into the type reserved
// for it beforehand, which 'publish' gives to whatever refers to the definition meanwhile; any other type that
// refers to the definition has no end
pub(crate) fn define(
    context: &mut LocalLoweringContext,
    name: String,
    span: &TokenRange,
    publish: impl FnOnce(&mut HMIREnvironment, HMIRValue),
    evaluate: impl FnOnce(&mut LocalLoweringContext, HMIRTypeID) -> CHLResult<HMIRTypeID>,
) -> CHLResult<HMIRValue> {
    let forward = HMIRTypeKind::Undefined(HMIRUndefined::Forward(None));
    let placeholder = HMIRType::new(
        forward,
        HMIRTypeSemantics::default(),
        span.clone(),
        Some(name.clone()),
    );
    let reserved = context.env_mut().resolver_mut().reserve_nominal(placeholder, None);
    let provisional = type_value(context, reserved, span)?;
    publish(context.env_mut(), provisional);
    let defined = evaluate(context, reserved)?;
    let built = defined == reserved
        && !matches!(
            context.env().resolve_kind(reserved),
            HMIRTypeKind::Undefined(HMIRUndefined::Forward(None))
        );
    if !built && reaches(context.env(), defined, reserved, &mut FxHashSet::default()) {
        return Err(hmir_error(span, &mir::DEPENDENCY_CYCLE, format!("{name} -> {name}")));
    }
    context.env_mut().resolver_mut().name_fresh_nominal(defined, reserved, name);
    type_value(context, defined, span)
}

fn reaches(
    env: &HMIREnvironment,
    ty: HMIRTypeID,
    target: HMIRTypeID,
    seen: &mut FxHashSet<HMIRTypeID>,
) -> bool {
    if ty == target {
        return true;
    }
    if !seen.insert(ty) {
        return false;
    }
    match env.resolve_type_id(ty).kind() {
        HMIRTypeKind::PointerTo(inner)
        | HMIRTypeKind::ReferenceTo { inner, .. }
        | HMIRTypeKind::Nominal { inner, .. }
        | HMIRTypeKind::Staged(inner)
        | HMIRTypeKind::Array { element: inner, .. }
        | HMIRTypeKind::Undefined(HMIRUndefined::Array { element: inner }) => {
            reaches(env, *inner, target, seen)
        }
        HMIRTypeKind::Function { signature, .. } => signature
            .params()
            .iter()
            .map(|param| *param.ty())
            .chain(std::iter::once(*signature.return_type()))
            .any(|ty| reaches(env, ty, target, seen)),
        HMIRTypeKind::Struct { fields }
        | HMIRTypeKind::Union { fields }
        | HMIRTypeKind::TaggedUnion { fields } => {
            fields.iter().any(|field| reaches(env, *field.ty(), target, seen))
        }
        _ => false,
    }
}
