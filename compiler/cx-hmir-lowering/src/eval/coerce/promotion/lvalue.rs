use cx_hmir::ty::{
    HMIRType, HMIRTypeID, HMIRTypeKind, aggregate::HMIRTypeSemantics, context::HMIRTypeContext,
};
use cx_log::{CXResult, catalogue::typecheck};
use cx_tokens::TokenRange;

use crate::{
    emit,
    env::function::LocalLoweringContext,
    eval::{
        coerce::{compatible, result::Coercion},
        types,
    },
    log::hmir_error,
    value::HMIRValue,
};

pub fn apply(
    context: &mut LocalLoweringContext,
    value: HMIRValue,
    span: &TokenRange,
) -> CXResult<Coercion> {
    let Some(inner) = context.env().reference_inner(value.ty()) else {
        return Ok(Coercion::Unapplied(value));
    };
    if matches!(
        context.env().resolve_kind(inner),
        HMIRTypeKind::Array { .. }
    ) || compatible::is_designator(context.env(), inner)
    {
        return Ok(Coercion::Unapplied(value));
    }

    if context.env().is_nocopy(inner) {
        return Err(hmir_error(
            span,
            &typecheck::INVALID_CONVERSION,
            (
                types::name(context.env(), value.ty()),
                types::name(context.env(), inner),
            ),
        ));
    }

    let ty = unqualified(context, inner, span);
    Ok(Coercion::Applied(
        emit::copy(context, value, span)?.with_ty(ty),
    ))
}

fn unqualified(
    context: &mut LocalLoweringContext,
    ty: HMIRTypeID,
    span: &TokenRange,
) -> HMIRTypeID {
    let env = context.env();
    let semantics = env.semantics(ty);
    if !semantics.constant {
        return ty;
    }

    let underlying = env.underlying(ty);
    let resolved = env.resolve_type_id(underlying);
    let kind = match resolved.kind() {
        _ if !resolved.semantics().constant => HMIRTypeKind::Nominal(underlying),
        kind @ (HMIRTypeKind::Int(_) | HMIRTypeKind::Float(_) | HMIRTypeKind::PointerTo(_)) => {
            kind.clone()
        }
        _ => return ty,
    };
    let semantics = HMIRTypeSemantics {
        constant: false,
        ..semantics
    };

    context.env_mut().resolver_mut().realize_type(HMIRType::new(
        kind,
        semantics,
        span.clone(),
        None,
    ))
}
