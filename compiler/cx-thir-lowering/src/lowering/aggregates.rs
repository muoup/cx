use cx_log::CXResult;
use cx_mir::{
    MIRAggregateOp, MIRBinaryOp, MIRConstant, MIRInstrKind, MIRIntBinaryOp, MIRIntType, MIRPlace,
    MIRPlaceAggregateOp, MIRValue, MIRValueAggregateOp,
};
use cx_thir::thir::{
    data::{THIRIntType, THIRType, THIRTypeKind},
    expression::{THIRExpression, THIRExpressionKind},
    pattern::THIRPattern,
};
use cx_thir::type_context::THIRTypeContext;

use crate::{
    builder::{MIRBuilder, integer_type},
    lowering::types::lower_type,
};

pub(super) fn lower_pattern_test(
    builder: &mut MIRBuilder<'_>,
    lhs: &THIRExpression,
    pattern: &THIRPattern,
    result_type: &THIRType,
) -> CXResult<MIRValue> {
    let lhs_value = super::lower_expression(builder, lhs)?;
    let (tested, constant) = match pattern {
        THIRPattern::TaggedUnionVariant {
            sum_type,
            variant_index,
            inner_local_id,
            inner_name,
        } => {
            let base = super::memory::ensure_place(builder, lhs_value.clone(), &lhs._type);
            if let Some(local_id) = inner_local_id {
                let payload_type = sum_variant_type(builder, sum_type, *variant_index);
                let payload_type_id = lower_type(builder, &payload_type);
                let payload = builder.place(payload_type_id, inner_name.clone(), false);
                let sum_type_id = lower_type(builder, sum_type);
                builder.emit(MIRInstrKind::AggregateOp(MIRAggregateOp::Place {
                    out: payload,
                    op: MIRPlaceAggregateOp::Variant {
                        base,
                        variant: *variant_index,
                        sum_type: sum_type_id,
                    },
                }));
                builder.bind_local(*local_id, payload);
            }
            let tag_type = lower_type(
                builder,
                &THIRType::from(THIRTypeKind::Integer {
                    _type: THIRIntType::I8,
                    signed: false,
                }),
            );
            let tag = builder.register(tag_type, None);
            let sum_type_id = lower_type(builder, sum_type);
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
    let result_type_id = lower_type(builder, result_type);
    let out = builder.register(result_type_id, None);
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
    subject: MIRPlace,
    sum_type: &THIRType,
) {
    if let THIRPattern::TaggedUnionVariant {
        variant_index,
        inner_local_id: Some(local_id),
        inner_name,
        ..
    } = pattern
    {
        let payload_type = sum_variant_type(builder, sum_type, *variant_index);
        let payload_type_id = lower_type(builder, &payload_type);
        let payload = builder.place(payload_type_id, inner_name.clone(), false);
        let sum_type_id = lower_type(builder, sum_type);

        builder.emit(MIRInstrKind::AggregateOp(MIRAggregateOp::Place {
            out: payload,
            op: MIRPlaceAggregateOp::Variant {
                base: subject,
                variant: *variant_index,
                sum_type: sum_type_id,
            },
        }));

        builder.bind_local(*local_id, payload);
        if let Some(name) = inner_name {
            builder.bind_named(name, MIRValue::Place(payload));
        }
    }
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

pub(super) fn constant_from_expression(expression: &THIRExpression) -> MIRConstant {
    match &expression.kind {
        THIRExpressionKind::BoolLiteral(value) => MIRConstant::Bool(*value),
        THIRExpressionKind::IntLiteral(value) => {
            let (ty, signed) = integer_type(&expression._type);
            MIRConstant::Integer {
                value: *value as i128,
                ty,
                signed,
            }
        }
        THIRExpressionKind::FloatLiteral(value) => MIRConstant::Float {
            value: *value,
            ty: match expression._type.kind {
                THIRTypeKind::Float { _type } => super::types::lower_float_type(_type),
                _ => cx_mir::MIRFloatType::F64,
            },
        },
        _ => MIRConstant::Undefined,
    }
}

pub(super) fn constant_from_pattern(pattern: &THIRPattern) -> MIRConstant {
    match pattern {
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
