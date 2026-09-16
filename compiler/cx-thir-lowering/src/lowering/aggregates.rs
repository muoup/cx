use cx_log::catalogue::mir as catalogue;

use crate::{log::log_mir_error, lowering::lower_expression};
use cx_log::CXResult;
use cx_mir::{
    MIRAggregateOp, MIRBinaryOp, MIRConstant, MIRInstrKind, MIRIntBinaryOp, MIRIntType,
    MIRTargetAggregateOp, MIRValue, MIRValueAggregateOp,
};
use cx_thir::thir::{
    data::{THIRIntType, THIRType, THIRTypeKind},
    expression::THIRExpression,
    pattern::THIRPattern,
};
use cx_thir::type_context::THIRTypeContext;

use crate::{
    builder::MIRBuilder,
    lowering::{memory, types::lower_type},
};

pub(super) fn lower_pattern_test(
    builder: &mut MIRBuilder<'_>,
    lhs: &THIRExpression,
    pattern: &THIRPattern,
    result_type: &THIRType,
) -> CXResult<MIRValue> {
    let lhs_value = lower_expression(builder, lhs)?;
    let (tested, constant) = match pattern {
        THIRPattern::Binding { .. } => {
            return log_mir_error(
                &lhs.token_range,
                (
                    &catalogue::REQUIRED_CONTEXT,
                    ("binding patterns".into(), "match arms".into()),
                ),
            );
        }
        THIRPattern::TaggedUnionVariant {
            sum_type,
            variant_index,
            inner_local_id,
            inner_name: _,
        } => {
            let base = memory::ensure_place(builder, lhs_value.clone(), &lhs._type)?;
            if let Some(local_id) = inner_local_id {
                let payload_type = sum_variant_type(builder, sum_type, *variant_index);
                let payload_type_id = lower_type(builder, &payload_type)?;
                let payload = memory::target_register(builder, payload_type_id);
                let sum_type_id = lower_type(builder, sum_type)?;

                builder.emit(MIRInstrKind::AggregateOp(MIRAggregateOp::Target {
                    out: payload,
                    op: MIRTargetAggregateOp::Variant {
                        base,
                        variant: *variant_index,
                        sum_type: sum_type_id,
                    },
                }));

                builder.fun_mut().bind_local(
                    *local_id,
                    MIRValue::Reference(cx_mir::MIRTarget::Register(payload)),
                );
            }
            let tag_type = lower_type(
                builder,
                &THIRType::from(THIRTypeKind::Integer {
                    _type: THIRIntType::I8,
                    signed: false,
                }),
            )?;
            let tag = builder.fun_mut().new_register(tag_type, None);
            let sum_type_id = lower_type(builder, sum_type)?;
            builder.emit(MIRInstrKind::AggregateOp(MIRAggregateOp::Value {
                out: tag,
                op: MIRValueAggregateOp::Discriminant {
                    value: lhs_value,
                    sum_type: sum_type_id,
                },
            }));
            (
                MIRValue::Register(tag),
                MIRConstant::Integer {
                    value: *variant_index as i128,
                    ty: MIRIntType::I8,
                    signed: false,
                },
            )
        }
        THIRPattern::Integer(value) => (
            lhs_value,
            MIRConstant::Integer {
                value: *value as i128,
                ty: MIRIntType::I64,
                signed: true,
            },
        ),
        THIRPattern::Float(value, ty) => (
            lhs_value,
            MIRConstant::Float {
                value: *value,
                ty: super::types::lower_float_type(*ty),
            },
        ),
    };
    let result_type_id = lower_type(builder, result_type)?;
    let out = builder.fun_mut().new_register(result_type_id, None);
    builder.emit(MIRInstrKind::BinOp {
        out,
        op: MIRBinaryOp::Integer {
            ty: MIRIntType::I8,
            signed: false,
            op: MIRIntBinaryOp::Eq,
        },
        lhs: tested,
        rhs: MIRValue::Constant(constant),
    });
    Ok(MIRValue::Register(out))
}

pub(super) fn bind_pattern_payload(
    builder: &mut MIRBuilder<'_>,
    pattern: &THIRPattern,
    subject: MIRValue,
    sum_type: &THIRType,
) -> CXResult<()> {
    if let THIRPattern::Binding { name, local_id } = pattern {
        let place = if sum_type.is_memory_reference() {
            memory::ensure_place(builder, subject, sum_type)?
        } else {
            cx_mir::MIRTarget::Place(memory::assign_operand_to_place(
                builder,
                subject,
                sum_type,
                Some(name.clone()),
            )?)
        };
        builder
            .fun_mut()
            .bind_local(*local_id, MIRValue::Reference(place));
        builder
            .fun_mut()
            .bind_named_value(name, MIRValue::Reference(place));
        return Ok(());
    }
    if let THIRPattern::TaggedUnionVariant {
        variant_index,
        inner_local_id: Some(local_id),
        inner_name,
        ..
    } = pattern
    {
        let payload_type = sum_variant_type(builder, sum_type, *variant_index);
        let payload_type_id = lower_type(builder, &payload_type)?;
        let sum_type_id = lower_type(builder, sum_type)?;

        let subject = if sum_type.is_memory_reference() {
            MIRValue::Reference(memory::ensure_place(builder, subject, sum_type)?)
        } else {
            subject
        };

        let (payload, instr) = match subject {
            MIRValue::Reference(target) => {
                let out = memory::target_register(builder, payload_type_id);

                (
                    MIRValue::Reference(cx_mir::MIRTarget::Register(out)),
                    MIRAggregateOp::Target {
                        out,
                        op: MIRTargetAggregateOp::Variant {
                            base: target,
                            variant: *variant_index,
                            sum_type: sum_type_id,
                        },
                    },
                )
            }

            MIRValue::Register(reg) => {
                let out = builder
                    .fun_mut()
                    .new_register(payload_type_id, inner_name.clone());

                (
                    MIRValue::Register(out),
                    MIRAggregateOp::Value {
                        out: out.clone(),
                        op: MIRValueAggregateOp::ProjectVariant {
                            value: MIRValue::Register(reg),
                            variant: *variant_index,
                            sum_type: sum_type_id,
                        },
                    },
                )
            }

            _ => unreachable!(),
        };

        builder.emit(MIRInstrKind::AggregateOp(instr));

        builder.fun_mut().bind_local(*local_id, payload.clone());
        if let Some(name) = inner_name {
            builder.fun_mut().bind_named_value(name, payload);
        }
    }
    Ok(())
}

pub(super) fn sum_variant_type(
    builder: &MIRBuilder<'_>,
    sum_type: &THIRType,
    variant_index: usize,
) -> THIRType {
    let semantic_sum = builder
        .registry()
        .mem_ref_inner(sum_type)
        .unwrap_or(sum_type);
    semantic_sum
        .aggregate_fields(builder.registry())
        .and_then(|variants| variants.into_iter().nth(variant_index))
        .map(|(_, variant)| variant)
        .unwrap_or_else(|| semantic_sum.clone())
}

pub(super) fn constant_from_pattern(pattern: &THIRPattern) -> MIRConstant {
    match pattern {
        THIRPattern::Binding { .. } => unreachable!("binding patterns have no case constant"),
        THIRPattern::Integer(value) => MIRConstant::Integer {
            value: *value as i128,
            ty: MIRIntType::I64,
            signed: true,
        },
        THIRPattern::Float(value, ty) => MIRConstant::Float {
            value: *value,
            ty: super::types::lower_float_type(*ty),
        },
        THIRPattern::TaggedUnionVariant { variant_index, .. } => MIRConstant::Integer {
            value: *variant_index as i128,
            ty: MIRIntType::I8,
            signed: false,
        },
    }
}
