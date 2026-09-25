use cx_log::{CXResult, catalogue::mir};
use cx_mir::{
    MIRAggregateIntrinsic, MIRComptimeBody, MIRConstant, MIRTarget, MIRTypeID,
    ty::{interface::MTRegistry, layout::calculate_type_layout},
};
use cx_tokens::TokenRange;

use crate::{
    ComptimeContext,
    execution::{
        engine::{Engine, ExecutionFrame},
        memory::{self, field_byte_offset, read_global},
        scalar::integer_value,
        typing::{integer_type, target_type},
    },
    log::{comptime_error, internal_error},
};

pub(super) fn execute<'c, 'thir, C: ComptimeContext<'thir>>(
    engine: &mut Engine<'c, 'thir, C>,
    frame: &mut ExecutionFrame,
    body: &MIRComptimeBody<'_>,
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
                        .read(frame, body, value, range)
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
            let base = engine.read(frame, body, base, range)?;
            let value =
                project_aggregate(engine, body, *out, base, *field, offset, field_ty, range)?;
            engine.write(frame, out, value)
        }
        A::ArrayIndex {
            out,
            base,
            index,
            element_ty,
        } => {
            let base = engine.read(frame, body, base, range)?;
            let index = engine
                .read(frame, body, index, range)
                .and_then(|value| integer_value(value, range))?;
            let index = usize::try_from(index).ok();
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
                internal_error(
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
                    let types = engine.context().types();
                    let result_ty = target_type(body, *out, range)?;
                    if types
                        .definition(result_ty)
                        .is_some_and(|ty| types.is_reference_type(ty))
                    {
                        MIRConstant::GlobalRef(reference)
                    } else {
                        read_global(engine.context(), reference, range)?
                    }
                }
                MIRConstant::Aggregate { .. } => memory::aggregate_field(&base, index)
                    .unwrap_or_else(|| memory::zero_value(engine.context().types(), *element_ty)),
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
            let value = engine.read(frame, body, value, range)?;
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
            let base = engine.read(frame, body, base, range)?;
            let field_ty = field_byte_offset(engine.context().types(), *sum_ty, *variant)
                .map(|(_, ty)| ty)
                .ok_or_else(|| {
                    internal_error(
                        &mir::COMPTIME_INVALID_OPERATION,
                        "invalid sum variant".into(),
                        "comptime aggregate access",
                    )
                })?;
            let value = project_aggregate(engine, body, *out, base, *variant, 0, field_ty, range)?;
            engine.write(frame, out, value)
        }
        A::SumVariantL {
            out,
            source,
            variant,
            sum_ty,
        } => {
            let base = engine.read(frame, body, source, range)?;
            let payload = memory::aggregate_field(&base, *variant).unwrap_or_else(|| {
                let ty = field_byte_offset(engine.context().types(), *sum_ty, *variant)
                    .map(|(_, ty)| ty);
                ty.map(|ty| memory::zero_value(engine.context().types(), ty))
                    .unwrap_or(MIRConstant::Undefined)
            });
            engine.write(frame, &MIRTarget::Place(*out), payload)
        }
    }
}

fn project_aggregate<'c, 'thir, C: ComptimeContext<'thir>>(
    engine: &Engine<'c, 'thir, C>,
    body: &MIRComptimeBody<'_>,
    out: MIRTarget,
    base: MIRConstant,
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
            let types = engine.context().types();
            let result_ty = target_type(body, out, range)?;
            if types
                .definition(result_ty)
                .is_some_and(|ty| types.is_reference_type(ty))
            {
                Ok(MIRConstant::GlobalRef(reference))
            } else {
                read_global(engine.context(), reference, range)
            }
        }
        value @ MIRConstant::Aggregate { .. } => Ok(memory::aggregate_field(&value, field)
            .unwrap_or_else(|| memory::zero_value(engine.context().types(), field_ty))),
        _ => comptime_error(
            range.clone(),
            (
                &mir::COMPTIME_INVALID_OPERATION,
                "project a non-aggregate value".into(),
            ),
        ),
    }
}
