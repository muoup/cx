use cx_hmir::ty::{HMIRTypeKind, aggregate::HMIRTypeSemantics, context::HMIRTypeContext};
use cx_log::{CXResult, catalogue::typecheck};
use cx_tokens::TokenRange;

use crate::{
    emit,
    env::function::{FnContext, LocalLoweringContext},
    eval::{
        coerce::{compatible, result::Coercion},
        op::unspecify,
        safety::require_pure,
        types,
    },
    log::hmir_error,
    value::{HMIRPlace, HMIRValue, HMIRValueKind, address::Addressable, constant::HMIRConstant},
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

    let semantics = context.env().semantics(inner);
    let global = match value.kind() {
        HMIRValueKind::PlaceRef(place) => matches!(place, HMIRPlace::Global(_)),
        HMIRValueKind::Constant(HMIRConstant::Address(address)) => {
            matches!(address.base(), Addressable::Place(HMIRPlace::Global(_)))
        }
        _ => false,
    };
    if semantics.volatile {
        require_pure(context, span, "reading a volatile value")?;
    } else if global
        && !semantics.constant
        && matches!(context.context(), FnContext::Runtime(_))
    {
        require_pure(context, span, "reading a mutable global")?;
    }

    let qualifiers = HMIRTypeSemantics {
        constant: true,
        volatile: true,
        ..Default::default()
    };
    let ty = unspecify(context, inner, qualifiers, span);
    Ok(Coercion::Applied(
        emit::copy(context, value, span)?.with_ty(ty),
    ))
}
