use cx_log::{CXResult, catalogue::mir};
use cx_mir::{
    MIRAggregateIntrinsic, MIRConstant, MIRFloatIntrinsic, MIRIntIntrinsic, MIRIntType,
    MIRIntrinsic, MIRTarget, MIRType, MIRTypeKind, MIRValue,
};
use cx_thir::thir::{
    data::{THIRType, THIRTypeKind},
    expression::THIRExpression,
    pattern::THIRPattern,
};
use cx_thir::type_context::THIRTypeContext;
use crate::{
    builder::MIRBuilder,
    log::log_mir_error,
    lowering::{
        lower_expression, memory,
        types::{lower_float_type, lower_int_type, lower_type, lower_type_id},
    },
};

pub(super) fn lower_pattern_test<'thir>(
    builder: &mut MIRBuilder<'thir>,
    lhs: &'thir THIRExpression,
    pattern: &THIRPattern,
    result_type: &'thir THIRType,
    subject: Option<MIRValue>,
) -> CXResult<MIRValue> {
    let token_range = lhs.token_range.clone();
    let tested = match pattern {
        THIRPattern::Binding { .. } => {
            return log_mir_error(
                &lhs.token_range,
                (
                    &mir::REQUIRED_CONTEXT,
                    ("binding patterns".into(), "match arms".into()),
                ),
            );
        }
        THIRPattern::TaggedUnionVariant { variant_index, .. } => {
            let sum_type = match &lhs._type.kind {
                THIRTypeKind::MemoryReference { inner_type, .. } => {
                    builder.registry().resolve_type_id(*inner_type)
                }
                _ => &lhs._type,
            };
            let sum_type_id = lower_type(builder, sum_type)?;
            let tag_type = builder.types_mut().intern(MIRType::new(
                MIRTypeKind::Integer {
                    ty: MIRIntType::I8,
                    signed: false,
                },
                None,
            ));
            let out = builder.fun_mut().new_register(tag_type, None);
            let value = match subject {
                Some(value) => value,
                None => lower_expression(builder, lhs)?,
            };
            builder.fun_mut().emit_intrinsic(
                MIRAggregateIntrinsic::SumIndex {
                    out: MIRTarget::Register(out),
                    value,
                    sum_ty: sum_type_id,
                },
                lhs.token_range.clone(),
            );
            (
                MIRValue::Register(out),
                MIRValue::Constant(MIRConstant::Integer {
                    value: *variant_index as i128,
                    ty: MIRIntType::I8,
                }),
                false,
            )
        }
        THIRPattern::Integer(value) => {
            let input = match subject {
                Some(value) => value,
                None => lower_expression(builder, lhs)?,
            };
            let ty = match lhs._type.kind {
                THIRTypeKind::Integer { _type, .. } => lower_int_type(_type),
                THIRTypeKind::MemoryReference { inner_type, .. } => {
                    let inner = builder.registry().resolve_type_id(inner_type);
                    match inner.kind {
                        THIRTypeKind::Integer { _type, .. } => lower_int_type(_type),
                        _ => unreachable!("integer pattern has non-integer subject"),
                    }
                }
                _ => unreachable!("integer pattern has non-integer subject"),
            };
            let value_type = match &lhs._type.kind {
                THIRTypeKind::MemoryReference { inner_type, .. } => {
                    builder.registry().resolve_type_id(*inner_type)
                }
                _ => &lhs._type,
            };
            let input = if lhs._type.is_memory_reference() {
                let type_id = lower_type(builder, value_type)?;
                memory::copy(builder, input, type_id, &lhs.token_range)
            } else {
                input
            };
            (
                input,
                MIRValue::Constant(MIRConstant::Integer {
                    value: *value as i128,
                    ty,
                }),
                false,
            )
        }
        THIRPattern::Float(value, ty) => (
            match subject {
                Some(value) => value,
                None => lower_expression(builder, lhs)?,
            },
            MIRValue::Constant(MIRConstant::Float {
                value: *value,
                ty: lower_float_type(*ty),
            }),
            true,
        ),
    };

    let (lhs, rhs, signed) = tested;
    let result_type = lower_type(builder, result_type)?;
    let out = builder.fun_mut().new_register(result_type, None);
    let target = MIRTarget::Register(out);
    let intrinsic: MIRIntrinsic = if signed {
        MIRFloatIntrinsic::Eq {
            out: target,
            lhs,
            rhs,
        }
        .into()
    } else {
        MIRIntIntrinsic::Eq {
            out: target,
            lhs,
            rhs,
        }
        .into()
    };
    builder.fun_mut().emit_intrinsic(intrinsic, token_range);
    Ok(MIRValue::Register(out))
}

pub(super) fn bind_pattern_payload<'thir>(
    builder: &mut MIRBuilder<'thir>,
    pattern: &THIRPattern,
    subject: MIRValue,
    sum_type: &'thir THIRType,
) -> CXResult<()> {
    match pattern {
        THIRPattern::Binding { name, local_id } => {
            let value = if sum_type.is_memory_reference() {
                subject
            } else {
                let place = memory::assign_operand_to_place(
                    builder,
                    subject,
                    sum_type,
                    Some(name.clone()),
                    &builder.fun().current_scope_range(),
                )?;
                MIRValue::PlaceRef(place)
            };
            builder.fun_mut().bind_local(*local_id, value.clone());
            builder.fun_mut().bind_named_value(name, value);
        }
        THIRPattern::TaggedUnionVariant {
            variant_index,
            inner_local_id: Some(local_id),
            inner_name,
            ..
        } => {
            let union_type = match &sum_type.kind {
                THIRTypeKind::MemoryReference { inner_type, .. } => {
                    builder.registry().resolve_type_id(*inner_type)
                }
                _ => sum_type,
            };
            let payload_type = match &union_type.kind {
                THIRTypeKind::TaggedUnion { variants } => {
                    variants.get(*variant_index).map(|field| field.ty())
                }
                _ => unreachable!("tagged union pattern has a non-union subject"),
            };
            let payload_type_id = match payload_type {
                Some(type_id) => lower_type_id(builder, type_id)?,
                None => lower_type(builder, union_type)?,
            };
            let sum_type_id = lower_type(builder, union_type)?;
            let result_type_id = if sum_type.is_memory_reference() {
                builder.types_mut().intern(MIRType::new(
                    MIRTypeKind::MemoryReference {
                        inner: payload_type_id,
                        bitfield: None,
                    },
                    None,
                ))
            } else {
                payload_type_id
            };
            let out = builder
                .fun_mut()
                .new_register(result_type_id, inner_name.clone());
            let range = builder.fun().current_scope_range();
            builder.fun_mut().emit_intrinsic(
                MIRAggregateIntrinsic::SumVariantL {
                    out: MIRTarget::Register(out),
                    base: subject,
                    variant: *variant_index,
                    sum_ty: sum_type_id,
                },
                range,
            );
            let value = MIRValue::Register(out);
            builder.fun_mut().bind_local(*local_id, value.clone());
            if let Some(name) = inner_name {
                builder.fun_mut().bind_named_value(name, value);
            }
        }
        _ => {}
    }
    Ok(())
}

#[allow(dead_code)]
pub(super) fn constant_from_pattern(pattern: &THIRPattern) -> MIRConstant {
    match pattern {
        THIRPattern::Binding { .. } => unreachable!("binding patterns have no case constant"),
        THIRPattern::Integer(value) => MIRConstant::Integer {
            value: *value as i128,
            ty: MIRIntType::I64,
        },
        THIRPattern::Float(_, _) => unreachable!("floating patterns cannot form integer cases"),
        THIRPattern::TaggedUnionVariant { variant_index, .. } => MIRConstant::Integer {
            value: *variant_index as i128,
            ty: MIRIntType::I8,
        },
    }
}
