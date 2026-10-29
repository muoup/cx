use cx_hmir::ty::{HMIRTypeID, HMIRTypeKind, context::HMIRTypeContext};
use cx_log::{CXResult, catalogue::typecheck};
use cx_tokens::TokenRange;

use crate::{
    env::function::LocalLoweringContext,
    eval::{coerce, types},
    log::hmir_error,
    value::HMIRValue,
};

pub(super) fn plan(
    context: &LocalLoweringContext,
    source: Option<&HMIRValue>,
    target: HMIRTypeID,
    span: &TokenRange,
) -> CXResult<usize> {
    let source = source.map(|value| (value.ty(), value.as_int()));
    let mut destination = target;
    let mut left = 0;
    let mut best = None;
    while let Some(inner) = context.env().reference_inner(destination) {
        if let Some((mut from, integer)) = source {
            let mut right = 0;
            loop {
                let env = context.env();
                let direct =
                    coerce::compatible(env, from, inner) && coerce::qualified(env, from, inner);
                let scalar = matches!(
                    env.resolve_kind(from),
                    HMIRTypeKind::Int(_) | HMIRTypeKind::Float(_)
                ) && matches!(
                    env.resolve_kind(inner),
                    HMIRTypeKind::Int(_) | HMIRTypeKind::Float(_)
                );
                let null = integer == Some(0)
                    && matches!(env.resolve_kind(from), HMIRTypeKind::Int(_))
                    && matches!(env.resolve_kind(inner), HMIRTypeKind::PointerTo(_));
                let pointer = matches!(
                    (env.resolve_kind(from), env.resolve_kind(inner)),
                    (
                        HMIRTypeKind::Array { .. } | HMIRTypeKind::Function(_) | HMIRTypeKind::Str,
                        HMIRTypeKind::PointerTo(_)
                    )
                );
                if direct || scalar || null || pointer {
                    let cost = left + right;
                    if best.is_none_or(|(_, previous)| cost < previous) {
                        best = Some((left, cost));
                    }
                    break;
                }
                let Some(next) = env.reference_inner(from) else {
                    break;
                };
                from = next;
                right += 1;
            }
        } else if !context.env().is_reference(inner) {
            return Ok(left);
        }
        destination = inner;
        left += 1;
    }
    best.map(|(left, _)| left).ok_or_else(|| {
        let from = source
            .map(|(ty, _)| types::name(context.env(), ty))
            .unwrap_or_else(|| "contextual value".into());
        hmir_error(
            span,
            &typecheck::INVALID_CONVERSION,
            (from, types::name(context.env(), target)),
        )
    })
}
