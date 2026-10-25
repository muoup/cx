use cx_hmir::{
    HMIRFnSignature,
    ty::{HMIRTypeID, HMIRTypeKind, context::HMIRTypeContext},
};
use cx_log::{CXResult, catalogue::typecheck};
use cx_tokens::TokenRange;

use crate::{
    env::{HMIREnvironment, function::LocalLoweringContext},
    eval::{
        coerce::{compatible, result::Coercion},
        types,
    },
    log::hmir_error,
    symbol::composite::signatures_alike,
    value::HMIRValue,
};

fn signature<'a>(
    env: &'a HMIREnvironment,
    ty: HMIRTypeID,
) -> Option<&'a HMIRFnSignature<HMIRTypeID>> {
    match env.resolve_kind(env.pointer_inner(ty)?) {
        HMIRTypeKind::Function(signature) => Some(signature),
        _ => None,
    }
}

pub(super) fn discard(
    context: &mut LocalLoweringContext,
    value: HMIRValue,
    target: HMIRTypeID,
    span: &TokenRange,
) -> CXResult<Coercion> {
    let env = context.env();
    let (Some(from), Some(to)) = (signature(env, value.ty()), signature(env, target)) else {
        return Ok(Coercion::Unapplied(value));
    };
    let result = *from.return_type();
    if !compatible::is_void(env, *to.return_type()) || compatible::is_void(env, result) {
        return Ok(Coercion::Unapplied(value));
    }
    let discarding = HMIRFnSignature::new(
        from.stage(),
        from.params().to_vec(),
        *to.return_type(),
        from.is_variadic(),
        from.contract().clone(),
    );
    if !signatures_alike(env, &discarding, to) {
        return Ok(Coercion::Unapplied(value));
    }
    if env.is_nodrop(result) {
        let names = (types::name(env, value.ty()), types::name(env, target));
        return Err(hmir_error(span, &typecheck::DISCARDED_NODROP, names));
    }

    Ok(Coercion::Applied(value.with_ty(target)))
}
