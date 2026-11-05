use cx_hmir::ty::{
    HMIRType, HMIRTypeID, HMIRTypeKind, aggregate::HMIRTypeSemantics, context::HMIRTypeContext,
};
use cx_tokens::TokenRange;

use crate::env::function::LocalLoweringContext;

pub fn unqualified(
    context: &mut LocalLoweringContext,
    ty: HMIRTypeID,
    span: &TokenRange,
) -> HMIRTypeID {
    let qualifiers = HMIRTypeSemantics {
        constant: true,
        volatile: true,
        ..Default::default()
    };
    remove(context, ty, qualifiers, span)
}

// Specifiers only accumulate, so a type that already carries them stands for itself
pub(crate) fn specify(
    context: &mut LocalLoweringContext,
    ty: HMIRTypeID,
    specifiers: HMIRTypeSemantics,
    span: &TokenRange,
) -> HMIRTypeID {
    let env = context.env_mut();
    let current = env.semantics(ty);
    let semantics = current.apply(specifiers);
    if semantics == current {
        return ty;
    }

    let kind = HMIRTypeKind::Nominal(env.underlying(ty));
    env.resolver_mut()
        .realize_type(HMIRType::new(kind, semantics, span.clone(), None))
}

pub(crate) fn remove(
    context: &mut LocalLoweringContext,
    ty: HMIRTypeID,
    qualifiers: HMIRTypeSemantics,
    span: &TokenRange,
) -> HMIRTypeID {
    let env = context.env();
    let current = env.semantics(ty);
    let semantics = HMIRTypeSemantics {
        constant: current.constant && !qualifiers.constant,
        volatile: current.volatile && !qualifiers.volatile,
        ..current
    };
    if semantics == current {
        return ty;
    }

    let underlying = env.underlying(ty);
    let resolved = env.resolve_type_id(underlying);
    let base = resolved.semantics();
    let kind = match resolved.kind() {
        _ if (!base.constant || semantics.constant) && (!base.volatile || semantics.volatile) => {
            HMIRTypeKind::Nominal(underlying)
        }
        kind @ (HMIRTypeKind::Int(_) | HMIRTypeKind::Float(_) | HMIRTypeKind::PointerTo(_)) => {
            kind.clone()
        }
        _ => return ty,
    };
    context.env_mut().resolver_mut().realize_type(HMIRType::new(
        kind,
        semantics,
        span.clone(),
        None,
    ))
}
