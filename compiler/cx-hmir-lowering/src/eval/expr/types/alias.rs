use rustc_hash::FxHashSet;
use cx_hmir::ty::{
    HMIRType, HMIRTypeID, HMIRTypeKind, HMIRUndefined, aggregate::HMIRTypeSemantics,
    context::HMIRTypeContext,
};
use cx_log::{CXResult, catalogue::mir};
use cx_tokens::TokenRange;

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
    evaluate: impl FnOnce(&mut LocalLoweringContext, HMIRTypeID) -> CXResult<HMIRTypeID>,
) -> CXResult<HMIRValue> {
    let forward = HMIRTypeKind::Undefined(HMIRUndefined::Forward(None));
    let placeholder = HMIRType::new(
        forward,
        HMIRTypeSemantics::default(),
        span.clone(),
        Some(name.clone()),
    );
    let reserved = context.env_mut().resolver_mut().reserve_type(placeholder);
    let provisional = type_value(context, reserved, span)?;
    publish(context.env_mut(), provisional);
    let defined = evaluate(context, reserved)?;
    let resolver = context.env_mut().resolver_mut();
    let ty = resolver.realized_type(reserved);
    let built = !matches!(ty.kind(), HMIRTypeKind::Undefined(HMIRUndefined::Forward(None)));
    if defined == reserved && built {
        if ty.debug_name().is_none() {
            let named = HMIRType::new(
                ty.kind().clone(),
                *ty.semantics(),
                ty.decl_span().clone(),
                Some(name),
            );
            resolver.complete_type(reserved, named);
        }
    } else if reaches(context.env(), defined, reserved, &mut FxHashSet::default()) {
        return Err(hmir_error(span, &mir::DEPENDENCY_CYCLE, format!("{name} -> {name}")));
    }
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
        | HMIRTypeKind::Nominal(inner)
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
