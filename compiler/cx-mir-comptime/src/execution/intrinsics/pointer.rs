use std::cmp::Ordering;

use cx_log::{CXResult, catalogue::mir};
use cx_mir::{
    MIRComptimeBody, MIRConstant, MIRPtrIntrinsic, MIRTarget, MIRTypeKind,
    ty::{interface::MTRegistry, layout::calculate_type_layout},
};
use cx_tokens::TokenRange;

use crate::{
    ComptimeContext,
    execution::{
        engine::{Engine, ExecutionFrame},
        scalar::{bool_const, integer_value, mask_integer},
        typing::{integer_type, target_type},
    },
    log::{comptime_error, internal_error},
};

pub(super) fn execute<'c, 'thir, C: ComptimeContext<'thir>>(
    engine: &mut Engine<'c, 'thir, C>,
    frame: &mut ExecutionFrame,
    body: &MIRComptimeBody<'_>,
    op: &MIRPtrIntrinsic,
    range: &TokenRange,
) -> CXResult<()> {
    use MIRPtrIntrinsic as P;
    let result = match op {
        P::Add { ptr, offset, .. } | P::Sub { ptr, offset, .. } => {
            let pointer = engine.read(frame, body, ptr, range)?;
            let delta = engine
                .read(frame, body, offset, range)
                .and_then(|value| integer_value(value, range))?;
            let delta = i64::try_from(delta).map_err(|_| {
                internal_error(
                    &mir::COMPTIME_INVALID_OPERATION,
                    "pointer offset overflow".into(),
                    "comptime pointer arithmetic",
                )
            })?;
            let delta = if matches!(op, P::Sub { .. }) {
                delta.checked_neg()
            } else {
                Some(delta)
            };
            let Some(delta) = delta else {
                return comptime_error(
                    range.clone(),
                    (
                        &mir::COMPTIME_INVALID_OPERATION,
                        "pointer offset overflow".into(),
                    ),
                );
            };
            match pointer {
                MIRConstant::GlobalRef(mut reference) => {
                    let Some(offset) = reference.offset.checked_add(delta) else {
                        return comptime_error(
                            range.clone(),
                            (
                                &mir::COMPTIME_INVALID_OPERATION,
                                "pointer offset overflow".into(),
                            ),
                        );
                    };
                    reference.offset = offset;
                    let types = engine.context().types();
                    let result_ty = target_type(body, target(op), range)?;
                    let Some(inner) = types.definition(result_ty).and_then(|ty| {
                        types
                            .pointer_inner(ty)
                            .or_else(|| types.reference_inner(ty))
                    }) else {
                        return comptime_error(
                            range.clone(),
                            (
                                &mir::COMPTIME_INVALID_OPERATION,
                                "pointer operation has a non-pointer result".into(),
                            ),
                        );
                    };
                    reference.ty = inner;
                    MIRConstant::GlobalRef(reference)
                }
                MIRConstant::Nullptr { ty } if delta == 0 => MIRConstant::Nullptr { ty },
                _ => {
                    return comptime_error(
                        range.clone(),
                        (
                            &mir::COMPTIME_INVALID_OPERATION,
                            "pointer arithmetic on a non-global pointer".into(),
                        ),
                    );
                }
            }
        }
        P::Diff {
            lhs,
            rhs,
            element_ty,
            ..
        } => {
            let lhs = engine.read(frame, body, lhs, range)?;
            let rhs = engine.read(frame, body, rhs, range)?;
            let stride = calculate_type_layout(engine.context().types(), *element_ty)
                .size()
                .max(1) as i128;
            let difference = match (lhs, rhs) {
                (MIRConstant::GlobalRef(left), MIRConstant::GlobalRef(right))
                    if left.global == right.global =>
                {
                    (left.offset - right.offset) as i128
                }
                (MIRConstant::Nullptr { .. }, MIRConstant::Nullptr { .. }) => 0,
                _ => {
                    return comptime_error(
                        range.clone(),
                        (
                            &mir::COMPTIME_INVALID_OPERATION,
                            "difference between unrelated pointers".into(),
                        ),
                    );
                }
            };
            let difference = difference / stride;
            let ty = integer_type(engine.context().types(), body, target(op), range)?;
            MIRConstant::Integer {
                value: mask_integer(difference, ty),
                ty,
            }
        }
        P::ToInt { ptr, target_ty, .. } => {
            let pointer = engine.read(frame, body, ptr, range)?;
            if !matches!(pointer, MIRConstant::Nullptr { .. }) {
                return comptime_error(
                    range.clone(),
                    (
                        &mir::COMPTIME_INVALID_OPERATION,
                        "convert a global address to an integer".into(),
                    ),
                );
            }
            let Some(MIRTypeKind::Integer { ty, .. }) = engine
                .context()
                .types()
                .definition(*target_ty)
                .map(|definition| definition.kind())
            else {
                return comptime_error(
                    range.clone(),
                    (
                        &mir::COMPTIME_INVALID_OPERATION,
                        "pointer conversion to a non-integer type".into(),
                    ),
                );
            };
            MIRConstant::Integer { value: 0, ty: *ty }
        }
        P::Eq { lhs, rhs, .. }
        | P::Neq { lhs, rhs, .. }
        | P::Lt { lhs, rhs, .. }
        | P::Leq { lhs, rhs, .. }
        | P::Gt { lhs, rhs, .. }
        | P::Geq { lhs, rhs, .. } => {
            let lhs = engine.read(frame, body, lhs, range)?;
            let rhs = engine.read(frame, body, rhs, range)?;
            let ordering = pointer_order(&lhs, &rhs);
            let result = match op {
                P::Eq { .. } => ordering == Some(Ordering::Equal),
                P::Neq { .. } => ordering != Some(Ordering::Equal),
                P::Lt { .. } => ordering == Some(Ordering::Less),
                P::Leq { .. } => matches!(ordering, Some(Ordering::Less | Ordering::Equal)),
                P::Gt { .. } => ordering == Some(Ordering::Greater),
                P::Geq { .. } => matches!(ordering, Some(Ordering::Greater | Ordering::Equal)),
                _ => unreachable!(),
            };
            if ordering.is_none() {
                return comptime_error(
                    range.clone(),
                    (
                        &mir::COMPTIME_INVALID_OPERATION,
                        "compare unrelated pointers".into(),
                    ),
                );
            }
            bool_const(result)
        }
    };
    engine.write(frame, &target(op), result)
}

fn target(op: &MIRPtrIntrinsic) -> MIRTarget {
    use MIRPtrIntrinsic as P;
    match op {
        P::ToInt { out, .. }
        | P::Add { out, .. }
        | P::Sub { out, .. }
        | P::Diff { out, .. }
        | P::Eq { out, .. }
        | P::Neq { out, .. }
        | P::Lt { out, .. }
        | P::Leq { out, .. }
        | P::Gt { out, .. }
        | P::Geq { out, .. } => *out,
    }
}

fn pointer_order(lhs: &MIRConstant, rhs: &MIRConstant) -> Option<std::cmp::Ordering> {
    match (lhs, rhs) {
        (MIRConstant::Nullptr { .. }, MIRConstant::Nullptr { .. }) => Some(Ordering::Equal),
        (MIRConstant::GlobalRef(left), MIRConstant::GlobalRef(right))
            if left.global == right.global =>
        {
            Some(left.offset.cmp(&right.offset))
        }
        (MIRConstant::GlobalRef(_), MIRConstant::Nullptr { .. }) => None,
        (MIRConstant::Nullptr { .. }, MIRConstant::GlobalRef(_)) => None,
        _ => None,
    }
}
