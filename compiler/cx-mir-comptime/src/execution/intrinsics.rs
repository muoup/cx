use cx_log::{CXResult, catalogue::mir};
use cx_mir::{
    MIRAggregateIntrinsic, MIRConstant, MIRFloatIntrinsic, MIRInternalIntrinsic, MIRIntrinsic,
    MIRPtrIntrinsic, MIRTarget, MIRTypeID, MIRTypeKind, MIRValue,
    ty::{interface::MTRegistry, layout::calculate_type_layout},
};
use cx_tokens::TokenRange;
use cx_util::unsafe_float::FloatWrapper;

use crate::{
    ComptimeContext,
    arithmetic::execute_integer_op,
    execution::{
        engine::{Engine, ExecutionFrame},
        memory::{field_byte_offset, read_global},
    },
    log::comptime_error,
};

pub(crate) fn execute<'c, 'thir, C: ComptimeContext<'thir>>(
    engine: &mut Engine<'c, 'thir, C>,
    frame: &mut ExecutionFrame,
    body: &cx_mir::MIRComptimeBody<'_>,
    intrinsic: &MIRIntrinsic,
    range: &TokenRange,
) -> CXResult<()> {
    match intrinsic {
        MIRIntrinsic::Int(op) => execute_integer_op(engine, frame, body, op, range),
        MIRIntrinsic::Float(op) => execute_float(engine, frame, body, op, range),
        MIRIntrinsic::Pointer(op) => execute_pointer(engine, frame, body, op, range),
        MIRIntrinsic::Aggregate(op) => execute_aggregate(engine, frame, body, op, range),
        MIRIntrinsic::Internal(op) => execute_internal(engine, frame, body, op, range),
        MIRIntrinsic::VA(op) => comptime_error(
            range.clone(),
            (
                &mir::COMPTIME_INVALID_OPERATION,
                format!("variadic operation {op:?}"),
            ),
        ),
    }
}

fn execute_float<'c, 'thir, C: ComptimeContext<'thir>>(
    engine: &mut Engine<'c, 'thir, C>,
    frame: &mut ExecutionFrame,
    body: &cx_mir::MIRComptimeBody<'_>,
    op: &MIRFloatIntrinsic,
    range: &TokenRange,
) -> CXResult<()> {
    use MIRFloatIntrinsic as F;
    let read = |value: &MIRValue| engine.read(frame, value, range);
    let float = |value: MIRConstant| match value {
        MIRConstant::Float { value, .. } => Ok(f64::from(&value)),
        _ => comptime_error(
            range.clone(),
            (&mir::COMPTIME_INVALID_OPERATION, "non-float operand".into()),
        ),
    };
    let (out, value) = match op {
        F::Neg { out, value } => {
            let value = -float(read(value)?)?;
            engine.write(
                frame,
                out,
                float_constant(
                    value,
                    float_type(engine.context().types(), body, *out, range)?,
                ),
            )?;
            return Ok(());
        }
        F::ToInt {
            out,
            value,
            target_ty,
        } => {
            let value = float(read(value)?)?;
            let Some(MIRTypeKind::Integer { ty, signed }) = engine
                .context()
                .types()
                .definition(*target_ty)
                .map(|ty| ty.kind())
            else {
                return comptime_error(
                    range.clone(),
                    (
                        &mir::COMPTIME_INVALID_OPERATION,
                        "float conversion to a non-integer type".into(),
                    ),
                );
            };
            let value = if *signed {
                value.trunc() as i128
            } else {
                value.trunc() as u128 as i128
            };
            let value = mask_integer(value, *ty);
            engine.write(frame, out, MIRConstant::Integer { value, ty: *ty })?;
            return Ok(());
        }
        F::FloatCast {
            out,
            value,
            float_ty,
        } => {
            let value = float(read(value)?)?;
            engine.write(frame, out, float_constant(value, *float_ty))?;
            return Ok(());
        }
        F::Add { out, lhs, rhs }
        | F::Sub { out, lhs, rhs }
        | F::Mul { out, lhs, rhs }
        | F::Div { out, lhs, rhs }
        | F::Eq { out, lhs, rhs }
        | F::Neq { out, lhs, rhs }
        | F::Lt { out, lhs, rhs }
        | F::Le { out, lhs, rhs }
        | F::Gt { out, lhs, rhs }
        | F::Geq { out, lhs, rhs } => (*out, (lhs, rhs)),
    };
    let (lhs, rhs) = (float(read(value.0)?)?, float(read(value.1)?)?);
    let result = match op {
        F::Add { .. } => float_constant(
            lhs + rhs,
            float_type(engine.context().types(), body, out, range)?,
        ),
        F::Sub { .. } => float_constant(
            lhs - rhs,
            float_type(engine.context().types(), body, out, range)?,
        ),
        F::Mul { .. } => float_constant(
            lhs * rhs,
            float_type(engine.context().types(), body, out, range)?,
        ),
        F::Div { .. } => float_constant(
            lhs / rhs,
            float_type(engine.context().types(), body, out, range)?,
        ),
        F::Eq { .. } => bool_constant(lhs == rhs),
        F::Neq { .. } => bool_constant(lhs != rhs),
        F::Lt { .. } => bool_constant(lhs < rhs),
        F::Le { .. } => bool_constant(lhs <= rhs),
        F::Gt { .. } => bool_constant(lhs > rhs),
        F::Geq { .. } => bool_constant(lhs >= rhs),
        _ => unreachable!(),
    };
    engine.write(frame, &out, result)
}

fn execute_pointer<'c, 'thir, C: ComptimeContext<'thir>>(
    engine: &mut Engine<'c, 'thir, C>,
    frame: &mut ExecutionFrame,
    body: &cx_mir::MIRComptimeBody<'_>,
    op: &MIRPtrIntrinsic,
    range: &TokenRange,
) -> CXResult<()> {
    use MIRPtrIntrinsic as P;
    let result = match op {
        P::Add { ptr, offset, .. } | P::Sub { ptr, offset, .. } => {
            let pointer = engine.read(frame, ptr, range)?;
            let delta = integer_value(engine.read(frame, offset, range)?, range)?;
            let delta = i64::try_from(delta).map_err(|_| {
                crate::log::internal_error(
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
                    reference.ty =
                        pointer_inner(engine.context().types(), body, target(op), range)?;
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
        P::Diff { lhs, rhs, .. } => {
            let lhs = engine.read(frame, lhs, range)?;
            let rhs = engine.read(frame, rhs, range)?;
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
            let ty = integer_type(engine.context().types(), body, target(op), range)?;
            MIRConstant::Integer {
                value: mask_integer(difference, ty),
                ty,
            }
        }
        P::ToInt { ptr, target_ty, .. } => {
            let pointer = engine.read(frame, ptr, range)?;
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
            let lhs = engine.read(frame, lhs, range)?;
            let rhs = engine.read(frame, rhs, range)?;
            let ordering = pointer_order(&lhs, &rhs);
            let result = match op {
                P::Eq { .. } => ordering == Some(std::cmp::Ordering::Equal),
                P::Neq { .. } => ordering != Some(std::cmp::Ordering::Equal),
                P::Lt { .. } => ordering == Some(std::cmp::Ordering::Less),
                P::Leq { .. } => matches!(
                    ordering,
                    Some(std::cmp::Ordering::Less | std::cmp::Ordering::Equal)
                ),
                P::Gt { .. } => ordering == Some(std::cmp::Ordering::Greater),
                P::Geq { .. } => matches!(
                    ordering,
                    Some(std::cmp::Ordering::Greater | std::cmp::Ordering::Equal)
                ),
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
            bool_constant(result)
        }
    };
    engine.write(frame, &target(op), result)
}

fn execute_aggregate<'c, 'thir, C: ComptimeContext<'thir>>(
    engine: &mut Engine<'c, 'thir, C>,
    frame: &mut ExecutionFrame,
    body: &cx_mir::MIRComptimeBody<'_>,
    op: &MIRAggregateIntrinsic,
    range: &TokenRange,
) -> CXResult<()> {
    use MIRAggregateIntrinsic as A;
    match op {
        A::AggregateInit { out, ty, fields } => {
            let fields = fields
                .iter()
                .map(|(index, value)| {
                    engine
                        .read(frame, value, range)
                        .map(|value| (*index, value))
                })
                .collect::<CXResult<_>>()?;
            engine.write(frame, out, MIRConstant::Aggregate { ty: *ty, fields })
        }
        A::StructField {
            out,
            base,
            field,
            struct_ty,
        } => {
            let Some((offset, field_ty)) =
                field_byte_offset(engine.context().types(), *struct_ty, *field)
            else {
                return comptime_error(
                    range.clone(),
                    (
                        &mir::COMPTIME_INVALID_OPERATION,
                        "invalid aggregate field".into(),
                    ),
                );
            };
            let base = engine.read(frame, base, range)?;
            let value = project_aggregate(
                engine, body, *out, base, *struct_ty, *field, offset, field_ty, range,
            )?;
            engine.write(frame, out, value)
        }
        A::ArrayIndex {
            out,
            base,
            index,
            element_ty,
        } => {
            let base = engine.read(frame, base, range)?;
            let index =
                usize::try_from(integer_value(engine.read(frame, index, range)?, range)?).ok();
            let Some(index) = index else {
                return comptime_error(
                    range.clone(),
                    (
                        &mir::COMPTIME_INVALID_OPERATION,
                        "negative or oversized array index".into(),
                    ),
                );
            };
            let stride = calculate_type_layout(engine.context().types(), *element_ty).size();
            let offset = index.checked_mul(stride).ok_or_else(|| {
                crate::log::internal_error(
                    &mir::COMPTIME_INVALID_OPERATION,
                    "array offset overflow".into(),
                    "comptime aggregate access",
                )
            })?;
            let value = match base {
                MIRConstant::GlobalRef(mut reference) => {
                    let Some(offset) = reference.offset.checked_add(offset as i64) else {
                        return comptime_error(
                            range.clone(),
                            (
                                &mir::COMPTIME_INVALID_OPERATION,
                                "array offset overflow".into(),
                            ),
                        );
                    };
                    reference.offset = offset;
                    reference.ty = *element_ty;
                    if is_memory_reference(engine.context().types(), body, *out) {
                        MIRConstant::GlobalRef(reference)
                    } else {
                        read_global(engine.context(), reference, range)?
                    }
                }
                MIRConstant::Aggregate { .. } => {
                    aggregate_element(engine, &base, index, *element_ty)
                }
                _ => {
                    return comptime_error(
                        range.clone(),
                        (
                            &mir::COMPTIME_INVALID_OPERATION,
                            "index a non-aggregate value".into(),
                        ),
                    );
                }
            };
            engine.write(frame, out, value)
        }
        A::SumIndex { out, value, .. } => {
            let value = engine.read(frame, value, range)?;
            let index = match value {
                MIRConstant::Aggregate { fields, .. } => {
                    fields.first().map(|(index, _)| *index).unwrap_or(0)
                }
                _ => {
                    return comptime_error(
                        range.clone(),
                        (
                            &mir::COMPTIME_INVALID_OPERATION,
                            "read a tag from a non-aggregate".into(),
                        ),
                    );
                }
            };
            engine.write(
                frame,
                out,
                MIRConstant::Integer {
                    value: index as i128,
                    ty: integer_type(engine.context().types(), body, *out, range)?,
                },
            )
        }
        A::SumVariant {
            out,
            base,
            variant,
            sum_ty,
        } => {
            let base = engine.read(frame, base, range)?;
            let field_ty = field_byte_offset(engine.context().types(), *sum_ty, *variant)
                .map(|(_, ty)| ty)
                .ok_or_else(|| {
                    crate::log::internal_error(
                        &mir::COMPTIME_INVALID_OPERATION,
                        "invalid sum variant".into(),
                        "comptime aggregate access",
                    )
                })?;
            let value = project_aggregate(
                engine, body, *out, base, *sum_ty, *variant, 0, field_ty, range,
            )?;
            engine.write(frame, out, value)
        }
        A::SumVariantL {
            out,
            source,
            variant,
            sum_ty,
        } => {
            let base = engine.read(frame, source, range)?;
            let payload = aggregate_field(&base, *variant).unwrap_or_else(|| {
                let ty = field_byte_offset(engine.context().types(), *sum_ty, *variant)
                    .map(|(_, ty)| ty);
                ty.map(|ty| crate::execution::memory::zero_value(engine.context().types(), ty))
                    .unwrap_or(MIRConstant::Undefined)
            });
            engine.write(frame, &MIRTarget::Place(*out), payload)
        }
    }
}

fn project_aggregate<'c, 'thir, C: ComptimeContext<'thir>>(
    engine: &Engine<'c, 'thir, C>,
    body: &cx_mir::MIRComptimeBody<'_>,
    out: MIRTarget,
    base: MIRConstant,
    aggregate_ty: MIRTypeID,
    field: usize,
    offset: usize,
    field_ty: MIRTypeID,
    range: &TokenRange,
) -> CXResult<MIRConstant> {
    match base {
        MIRConstant::GlobalRef(mut reference) => {
            let Some(new_offset) = reference.offset.checked_add(offset as i64) else {
                return comptime_error(
                    range.clone(),
                    (
                        &mir::COMPTIME_INVALID_OPERATION,
                        "aggregate offset overflow".into(),
                    ),
                );
            };
            reference.offset = new_offset;
            reference.ty = field_ty;
            if is_memory_reference(engine.context().types(), body, out) {
                Ok(MIRConstant::GlobalRef(reference))
            } else {
                read_global(engine.context(), reference, range)
            }
        }
        value @ MIRConstant::Aggregate { .. } => {
            Ok(aggregate_field(&value, field).unwrap_or_else(|| {
                crate::execution::memory::zero_value(engine.context().types(), field_ty)
            }))
        }
        _ => {
            let _ = aggregate_ty;
            comptime_error(
                range.clone(),
                (
                    &mir::COMPTIME_INVALID_OPERATION,
                    "project a non-aggregate value".into(),
                ),
            )
        }
    }
}

fn aggregate_field(value: &MIRConstant, index: usize) -> Option<MIRConstant> {
    match value {
        MIRConstant::Aggregate { fields, .. } => fields
            .iter()
            .find(|(field, _)| *field == index)
            .map(|(_, value)| value.clone()),
        _ => None,
    }
}

fn aggregate_element<'c, 'thir, C: ComptimeContext<'thir>>(
    engine: &Engine<'c, 'thir, C>,
    aggregate: &MIRConstant,
    index: usize,
    ty: MIRTypeID,
) -> MIRConstant {
    aggregate_field(aggregate, index)
        .unwrap_or_else(|| crate::execution::memory::zero_value(engine.context().types(), ty))
}

fn execute_internal<'c, 'thir, C: ComptimeContext<'thir>>(
    engine: &mut Engine<'c, 'thir, C>,
    frame: &mut ExecutionFrame,
    body: &cx_mir::MIRComptimeBody<'_>,
    op: &MIRInternalIntrinsic,
    range: &TokenRange,
) -> CXResult<()> {
    use MIRInternalIntrinsic as I;
    match op {
        I::GlobalAddress { out, global } => {
            engine.write(frame, out, MIRConstant::GlobalRef(*global))
        }
        I::ArrayAddress { out, array } => {
            let value = engine.read(frame, array, range)?;
            let value = match value {
                MIRConstant::GlobalRef(mut reference) => {
                    reference.ty = pointer_inner(engine.context().types(), body, *out, range)?;
                    MIRConstant::GlobalRef(reference)
                }
                value => value,
            };
            engine.write(frame, out, value)
        }
        I::ReferenceAddress {
            out,
            reference: array,
        } => {
            let value = engine.read(frame, array, range)?;
            engine.write(frame, out, value)
        }
        I::StringAddress { out, string } => {
            engine.write(frame, out, MIRConstant::String(string.clone()))
        }
        I::GetFnPtr { out, fn_id } => engine.write(frame, out, MIRConstant::Function(*fn_id)),
        I::Bitcast {
            out,
            value,
            target_ty,
        } => {
            let value = engine.read(frame, value, range)?;
            let value = match value {
                MIRConstant::GlobalRef(mut reference) => {
                    reference.ty = match engine
                        .context()
                        .types()
                        .definition(*target_ty)
                        .map(|ty| ty.kind())
                    {
                        Some(
                            MIRTypeKind::PointerTo { inner }
                            | MIRTypeKind::MemoryReference { inner, .. },
                        ) => *inner,
                        _ => *target_ty,
                    };
                    MIRConstant::GlobalRef(reference)
                }
                value => value,
            };
            engine.write(frame, out, value)
        }
        I::PlaceAddress { .. } | I::AdoptPlace { .. } => comptime_error(
            range.clone(),
            (
                &mir::COMPTIME_INVALID_OPERATION,
                "take the address of a local comptime place".into(),
            ),
        ),
        I::Assert { condition, message } => {
            let condition = engine.read(frame, condition, range)?;
            if !crate::arithmetic::truthy(&condition) {
                return comptime_error(range.clone(), (&mir::COMPTIME_ASSERTION, message.clone()));
            }
            Ok(())
        }
        I::Assume { condition } => {
            let _ = engine.read(frame, condition, range)?;
            Ok(())
        }
    }
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

fn target_type(
    body: &cx_mir::MIRComptimeBody<'_>,
    target: MIRTarget,
    range: &TokenRange,
) -> CXResult<MIRTypeID> {
    let ty = match target {
        MIRTarget::Place(id) => body.place(id).map(|place| place.ty),
        MIRTarget::Register(id) => body.register(id).map(|register| register.ty),
        MIRTarget::Global(reference) => Some(reference.ty),
        MIRTarget::Indirect(_) => None,
    };
    match ty {
        Some(ty) => Ok(ty),
        None => comptime_error(
            range.clone(),
            (
                &mir::COMPTIME_INVALID_OPERATION,
                "unknown intrinsic output type".into(),
            ),
        ),
    }
}

fn pointer_inner<R: MTRegistry>(
    registry: &R,
    body: &cx_mir::MIRComptimeBody<'_>,
    target: MIRTarget,
    range: &TokenRange,
) -> CXResult<MIRTypeID> {
    let ty = target_type(body, target, range)?;
    match registry.definition(ty).map(|definition| definition.kind()) {
        Some(MIRTypeKind::PointerTo { inner } | MIRTypeKind::MemoryReference { inner, .. }) => {
            Ok(*inner)
        }
        _ => comptime_error(
            range.clone(),
            (
                &mir::COMPTIME_INVALID_OPERATION,
                "pointer operation has a non-pointer result".into(),
            ),
        ),
    }
}

fn integer_type<R: MTRegistry>(
    registry: &R,
    body: &cx_mir::MIRComptimeBody<'_>,
    target: MIRTarget,
    range: &TokenRange,
) -> CXResult<cx_mir::MIRIntType> {
    let ty = target_type(body, target, range)?;
    match registry.definition(ty).map(|definition| definition.kind()) {
        Some(MIRTypeKind::Integer { ty, .. }) => Ok(*ty),
        _ => comptime_error(
            range.clone(),
            (
                &mir::COMPTIME_INVALID_OPERATION,
                "integer result has a non-integer type".into(),
            ),
        ),
    }
}

fn is_memory_reference<R: MTRegistry>(
    registry: &R,
    body: &cx_mir::MIRComptimeBody<'_>,
    target: MIRTarget,
) -> bool {
    target_type(body, target, &TokenRange::internal())
        .ok()
        .and_then(|ty| registry.definition(ty))
        .is_some_and(|ty| matches!(ty.kind(), MIRTypeKind::MemoryReference { .. }))
}

fn float_type<R: MTRegistry>(
    registry: &R,
    body: &cx_mir::MIRComptimeBody<'_>,
    target: MIRTarget,
    range: &TokenRange,
) -> CXResult<cx_mir::MIRFloatType> {
    let ty = target_type(body, target, range)?;
    match registry.definition(ty).map(|definition| definition.kind()) {
        Some(MIRTypeKind::Float { ty }) => Ok(*ty),
        _ => comptime_error(
            range.clone(),
            (
                &mir::COMPTIME_INVALID_OPERATION,
                "float result has a non-float type".into(),
            ),
        ),
    }
}

fn float_constant(value: f64, ty: cx_mir::MIRFloatType) -> MIRConstant {
    MIRConstant::Float {
        value: FloatWrapper::from(if ty == cx_mir::MIRFloatType::F32 {
            (value as f32) as f64
        } else {
            value
        }),
        ty,
    }
}

fn bool_constant(value: bool) -> MIRConstant {
    MIRConstant::Integer {
        value: value as i128,
        ty: cx_mir::MIRIntType::I1,
    }
}

fn integer_value(value: MIRConstant, range: &TokenRange) -> CXResult<i128> {
    match value {
        MIRConstant::Integer { value, .. } => Ok(value),
        _ => comptime_error(
            range.clone(),
            (
                &mir::COMPTIME_INVALID_OPERATION,
                "non-integer operand".into(),
            ),
        ),
    }
}

fn mask_integer(value: i128, ty: cx_mir::MIRIntType) -> i128 {
    let width = if ty == cx_mir::MIRIntType::I1 {
        1
    } else {
        (ty.bytes() * 8) as u32
    };
    if width == 128 {
        value
    } else {
        ((value as u128) & ((1u128 << width) - 1)) as i128
    }
}

fn pointer_order(lhs: &MIRConstant, rhs: &MIRConstant) -> Option<std::cmp::Ordering> {
    match (lhs, rhs) {
        (MIRConstant::Nullptr { .. }, MIRConstant::Nullptr { .. }) => {
            Some(std::cmp::Ordering::Equal)
        }
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
