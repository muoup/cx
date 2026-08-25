mod calls;
pub(crate) mod comptime;
mod control_flow;
mod memory;
mod operators;
mod staged;

pub(crate) mod aggregates;
pub(crate) mod globals;
pub(crate) mod types;

use cx_log::{
    CXResult,
    error::{CXErr, context::CXInternalContext, message::CXStdErrMessage},
};
use cx_mir::{
    MIRAggregateOp, MIRAssignTarget, MIRBlockTarget, MIRConstant, MIRFunctionID, MIRInstrKind,
    MIRIntType, MIRParameterID, MIRPlace, MIRPlaceAggregateOp, MIRStagedExitKind, MIRTypeKind,
    MIRValue, MIRValueAggregateOp, ty::interface::MTRegistry,
};
use cx_thir::{
    thir::{
        comptime::THIRComptimeFn,
        data::{THIRFunction, THIRTypeKind},
        expression::{THIRBinOp, THIRCoercion, THIRExpression, THIRExpressionKind, THIRIntBinOp},
        r#type::THIRType,
    },
    type_context::THIRTypeContext,
};

use crate::lowering::{
    aggregates::move_value,
    control_flow::{auto_cleanup, auto_pop_scope, lower_control_exit},
    types::lower_float_type,
};
use crate::{
    builder::{MIRBuilder, integer_type},
    lowering::types::lower_type,
};

pub(crate) fn lower_function(
    builder: &mut MIRBuilder<'_>,
    id: MIRFunctionID,
    function: &THIRFunction,
) -> CXResult<()> {
    let Some(body) = function.body.as_ref() else {
        return Ok(());
    };

    builder.start_function(id);

    for (index, parameter) in function.prototype.signature().params.iter().enumerate() {
        let place = MIRPlace::Parameter(MIRParameterID::new(index));

        builder
            .fun_mut()
            .bind_local(parameter.local_id, MIRValue::PlaceRef(place.clone()));
        if let Some(name) = &parameter.name {
            builder
                .fun_mut()
                .bind_named_value(name, MIRValue::PlaceRef(place));
        }
    }

    lower_expression(builder, body)?;

    if !builder.fun_mut().current_block_terminated() {
        if matches!(
            function.prototype.signature().return_type.kind,
            THIRTypeKind::Void
        ) {
            builder.emit(MIRInstrKind::Return { value: None });
        } else {
            builder.emit(MIRInstrKind::Unreachable);
        }
    }

    builder.finish_function();
    Ok(())
}

pub(crate) fn lower_comptime_function(
    builder: &mut MIRBuilder<'_>,
    id: MIRFunctionID,
    function: &THIRComptimeFn,
) -> CXResult<()> {
    let Some(body) = function.body.as_ref() else {
        return Ok(());
    };

    builder.start_function(id);
    builder.fun_mut().push_scope(body.token_range.clone());

    for (index, parameter) in function.prototype.params().iter().enumerate() {
        let value = MIRValue::PlaceRef(MIRPlace::Parameter(cx_mir::MIRParameterID::new(index)));
        builder
            .fun_mut()
            .bind_local(parameter.local_id, value.clone());
        if let Some(name) = &parameter.name {
            builder.fun_mut().bind_named_value(name, value);
        }
    }

    let value = lower_expression(builder, body)?;

    if !builder.fun_mut().current_block_terminated() {
        let value = if matches!(
            function.prototype.return_type()._type.kind,
            THIRTypeKind::Void
        ) {
            None
        } else {
            Some(value)
        };
        builder.emit(MIRInstrKind::Return { value });
    }

    auto_pop_scope(builder)?;
    builder.finish_function();

    Ok(())
}

pub(super) fn materialize_value(
    builder: &mut MIRBuilder<'_>,
    value: MIRValue,
    ty: &THIRType,
) -> CXResult<MIRValue> {
    let (place, moves) = match value {
        MIRValue::Copy(place) => (place, false),
        MIRValue::Move(place) => (place, true),
        value => return Ok(value),
    };
    let type_id = lower_type(builder, ty)?;
    let out = builder.fun_mut().new_register(type_id, None);

    builder.emit(MIRInstrKind::Assign {
        target: MIRAssignTarget::Register(out),
        value: if moves {
            MIRValue::Move(place)
        } else {
            MIRValue::Copy(place)
        },
        ty: type_id,
    });

    Ok(MIRValue::Register(out))
}

pub(crate) fn lower_expression(
    builder: &mut MIRBuilder<'_>,
    expression: &THIRExpression,
) -> CXResult<MIRValue> {
    let previous_range = builder.set_source_range(expression.token_range.clone());
    let result = (|| -> CXResult<MIRValue> {
        let value = match &expression.kind {
            THIRExpressionKind::BoolLiteral(value) => MIRValue::Constant(MIRConstant::Bool(*value)),
            THIRExpressionKind::IntLiteral(value) => {
                let (ty, signed) = integer_type(&expression._type);
                MIRValue::Constant(MIRConstant::Integer {
                    value: *value as i128,
                    ty,
                    signed,
                })
            }
            THIRExpressionKind::FloatLiteral(value) => {
                let ty = match expression._type.kind {
                    THIRTypeKind::Float { _type } => lower_float_type(_type),
                    _ => cx_mir::MIRFloatType::F64,
                };
                MIRValue::Constant(MIRConstant::Float { value: *value, ty })
            }
            THIRExpressionKind::StringLiteral { value } => {
                if builder.types().find_kind(&MIRTypeKind::Str).is_none() {
                    builder
                        .types_mut()
                        .intern(cx_mir::MIRType::new(MIRTypeKind::Str, None));
                }
                MIRValue::PlaceRef(MIRPlace::Global(
                    builder.module_mut().add_string_literal(value.as_str())?,
                ))
            }
            THIRExpressionKind::Unit => MIRValue::Constant(MIRConstant::Unit),
            THIRExpressionKind::SizeOf { _type } | THIRExpressionKind::AlignOf { _type } => {
                let type_id = lower_type(builder, _type)?;
                let layout =
                    cx_mir::ty::layout::layout_of(builder.types(), type_id).map_err(|error| {
                        cx_log::error::CXErr::new(
                            cx_log::error::message::CXStdErrMessage::error(
                                "MIRLayoutError",
                                error.to_string(),
                            ),
                            cx_log::error::context::CXInternalContext::error(
                                "failed to calculate type layout during MIR lowering",
                            ),
                        )
                    })?;
                MIRValue::Constant(MIRConstant::Integer {
                    value: if matches!(&expression.kind, THIRExpressionKind::SizeOf { .. }) {
                        layout.size as i128
                    } else {
                        layout.alignment as i128
                    },
                    ty: MIRIntType::I64,
                    signed: false,
                })
            }

            THIRExpressionKind::Variable { local_id, .. } => {
                let value = builder
                    .local_value(*local_id, &expression._type)?
                    .ok_or_else(|| {
                        CXErr::new(
                            CXStdErrMessage::error(
                                "MIR ERROR",
                                format!("could not find local id {:?}", local_id),
                            ),
                            CXInternalContext::error(
                                "runtime local is unavailable in an thir lowering context",
                            ),
                        )
                    })?;
                if builder.is_capturing()
                    && (expression._type.is_void() || expression._type.is_unreachable())
                    && matches!(value, MIRValue::Register(_))
                {
                    builder.emit(MIRInstrKind::StagedUse {
                        value: value.clone(),
                    });
                }
                value
            }

            THIRExpressionKind::GlobalVariable { symbol } => MIRValue::PlaceRef(MIRPlace::Global(
                builder
                    .module_mut()
                    .global_symbol(symbol.as_str())
                    .ok_or_else(|| {
                        CXErr::new(
                            CXStdErrMessage::error(
                                "MissingGlobalVariable",
                                format!("global variable '{}' not found", symbol),
                            ),
                            CXInternalContext::error("failed to lower global variable reference"),
                        )
                    })?,
            )),

            THIRExpressionKind::ContractVariable { name, .. } => builder
                .fun_mut()
                .named(name)
                .map(|value| match value {
                    MIRValue::PlaceRef(place) => {
                        if expression._type.is_memory_reference() {
                            MIRValue::PlaceRef(place)
                        } else {
                            MIRValue::Copy(place)
                        }
                    }
                    value => value,
                })
                .unwrap_or(MIRValue::Constant(MIRConstant::Undefined)),

            THIRExpressionKind::FunctionReference { name, .. } => builder
                .module_mut()
                .function_symbol(name.as_str())
                .ok_or_else(|| {
                    CXErr::new(
                        CXStdErrMessage::error(
                            "MissingFunction",
                            format!("function '{}' not found", name),
                        ),
                        CXInternalContext::error("failed to lower function reference"),
                    )
                })
                .map(|v| MIRValue::Constant(MIRConstant::Function(v)))?,

            THIRExpressionKind::BinaryOperation { lhs, rhs, op } => {
                if matches!(
                    op,
                    THIRBinOp::Integer {
                        op: THIRIntBinOp::LAND | THIRIntBinOp::LOR,
                        ..
                    }
                ) {
                    control_flow::lower_short_circuit(builder, lhs, rhs, op, &expression._type)?
                } else {
                    let lhs = lower_expression(builder, lhs)?;
                    let rhs = lower_expression(builder, rhs)?;
                    let type_id = lower_type(builder, &expression._type)?;
                    let out = builder.fun_mut().new_register(type_id, None);
                    let lowered_op = operators::lower_binary_op(builder, op)?;
                    builder.emit(MIRInstrKind::BinOp {
                        out,
                        op: lowered_op,
                        lhs,
                        rhs,
                    });
                    MIRValue::Register(out)
                }
            }
            THIRExpressionKind::UnaryOperation { operand, op } => {
                let lowered = lower_expression(builder, operand)?;
                let lowered = if operand._type.is_memory_reference() {
                    MIRValue::PlaceRef(memory::ensure_place(builder, lowered, &operand._type)?)
                } else {
                    lowered
                };
                let type_id = lower_type(builder, &expression._type)?;
                let out = builder.fun_mut().new_register(type_id, None);
                builder.emit(MIRInstrKind::UnOp {
                    out,
                    op: operators::lower_unary_op(op, &operand._type),
                    operand: lowered,
                });
                MIRValue::Register(out)
            }

            THIRExpressionKind::Copy { source } => {
                let value = lower_expression(builder, source)?;
                if !source._type.is_memory_reference() {
                    return Ok(match value {
                        MIRValue::PlaceRef(place) => MIRValue::Copy(place),
                        value => value,
                    });
                }

                let inner_type = source
                    ._type
                    .mem_ref_inner()
                    .expect("memory reference is missing its pointee type");
                let pointee = builder.registry().resolve_type_id(inner_type).clone();
                let pointee_type = lower_type(builder, &pointee)?;
                match value {
                    MIRValue::PlaceRef(place) => MIRValue::Copy(place),
                    MIRValue::Register(register)
                        if builder.fun().register_type(register) == Some(pointee_type) =>
                    {
                        MIRValue::Register(register)
                    }
                    pointer => {
                        let out = builder.fun_mut().new_place(pointee_type, None, false);
                        builder.emit(MIRInstrKind::Dereference {
                            out,
                            pointer,
                            pointee_type,
                        });
                        MIRValue::Copy(out)
                    }
                }
            }

            THIRExpressionKind::Move { local_id, .. } => {
                let value = builder
                    .local_value(*local_id, &expression._type)?
                    .ok_or_else(|| {
                        CXErr::new(
                            CXStdErrMessage::error(
                                "COMPTIME ERROR",
                                "expression depends on a runtime local",
                            ),
                            CXInternalContext::error("THIRExpressionKind::Move"),
                        )
                    })?;
                if builder.is_capturing() && matches!(value, MIRValue::Register(_)) {
                    let ty = lower_type(builder, &expression._type)?;
                    let out = builder.fun_mut().new_register(ty, None);
                    builder.emit(MIRInstrKind::StagedMove { out, value });
                    MIRValue::Register(out)
                } else {
                    move_value(value)?
                }
            }

            THIRExpressionKind::CreateLocalVariable {
                name,
                local_id,
                _type,
                initial_value,
                adopting,
            } => {
                let initial_value = initial_value
                    .as_deref()
                    .map(|value| lower_expression(builder, value))
                    .transpose()?;

                if *adopting {
                    let initial_value = initial_value
                        .expect("adopting local variable is missing its initial value");
                    match initial_value {
                        MIRValue::PlaceRef(place) => {
                            builder
                                .fun_mut()
                                .bind_local(*local_id, MIRValue::PlaceRef(place));
                            builder
                                .fun_mut()
                                .bind_named_value(name, MIRValue::PlaceRef(place));
                            MIRValue::PlaceRef(place)
                        }
                        value => {
                            let place = memory::assign_operand_to_place(
                                builder,
                                value,
                                _type,
                                Some(name.clone()),
                            )?;
                            builder
                                .fun_mut()
                                .bind_local(*local_id, MIRValue::PlaceRef(place));
                            builder
                                .fun_mut()
                                .bind_named_value(name, MIRValue::PlaceRef(place));
                            MIRValue::PlaceRef(place)
                        }
                    }
                } else {
                    let type_id = lower_type(builder, _type)?;
                    let place = builder.create(type_id, Some(name.clone()), _type.is_nodrop());
                    if let Some(value) = initial_value {
                        builder.emit(MIRInstrKind::Assign {
                            target: MIRAssignTarget::Place(place),
                            value,
                            ty: type_id,
                        });
                    } else {
                        builder.emit(MIRInstrKind::Initialize { place });
                    }
                    builder
                        .fun_mut()
                        .bind_local(*local_id, MIRValue::PlaceRef(place));
                    builder
                        .fun_mut()
                        .bind_named_value(name, MIRValue::PlaceRef(place));
                    MIRValue::PlaceRef(place)
                }
            }

            THIRExpressionKind::Assign { target, value } => {
                let assignment_type = lower_type(builder, &value._type)?;

                let mtarget = lower_expression(builder, target)?;
                let mvalue = lower_expression(builder, value)?;

                let ptarget = memory::ensure_place(builder, mtarget, &target._type)?;

                builder.set_source_range(target.token_range.clone());
                builder.emit(MIRInstrKind::Assign {
                    target: MIRAssignTarget::Place(ptarget),
                    value: mvalue,
                    ty: assignment_type,
                });

                MIRValue::PlaceRef(ptarget)
            }

            THIRExpressionKind::Typechange(inner) => {
                let value = lower_expression(builder, inner)?;

                // Keep reference conversions explicit so places, pointer
                // values, and pointee values remain distinct in MIR.
                let expression_is_reference =
                    matches!(&expression._type.kind, THIRTypeKind::MemoryReference { .. });
                let inner_is_reference =
                    matches!(&inner._type.kind, THIRTypeKind::MemoryReference { .. });
                let expression_is_pointer =
                    matches!(&expression._type.kind, THIRTypeKind::PointerTo { .. });
                let inner_is_pointer = matches!(&inner._type.kind, THIRTypeKind::PointerTo { .. });
                if expression_is_reference || inner_is_reference {
                    let reference_type = if expression_is_reference {
                        &expression._type
                    } else {
                        &inner._type
                    };
                    let THIRTypeKind::MemoryReference { inner_type, .. } = &reference_type.kind
                    else {
                        unreachable!("reference type was checked above")
                    };
                    let pointee = builder.registry().resolve_type_id(*inner_type).clone();
                    let pointee_type = lower_type(builder, &pointee)?;
                    if inner_is_reference && expression_is_pointer {
                        value
                    } else {
                        let type_id = if expression_is_reference {
                            lower_type(builder, reference_type)?
                        } else {
                            lower_type(builder, &expression._type)?
                        };
                        let out = builder.fun_mut().new_place(type_id, None, false);
                        builder.emit(MIRInstrKind::Dereference {
                            out,
                            pointer: value,
                            pointee_type,
                        });
                        MIRValue::PlaceRef(out)
                    }
                } else if inner_is_pointer && !expression_is_pointer {
                    let THIRTypeKind::PointerTo { inner_type } = &inner._type.kind else {
                        unreachable!("pointer type was checked above")
                    };
                    let pointee = builder.registry().resolve_type_id(*inner_type).clone();
                    let pointee_type = lower_type(builder, &pointee)?;
                    let type_id = lower_type(builder, &expression._type)?;
                    let out = builder.fun_mut().new_place(type_id, None, false);
                    builder.emit(MIRInstrKind::Dereference {
                        out,
                        pointer: value,
                        pointee_type,
                    });
                    MIRValue::PlaceRef(out)
                } else {
                    value
                }
            }

            THIRExpressionKind::MemberAccess {
                base,
                member_index,
                aggregate_type,
            } => {
                let base_value = lower_expression(builder, base)?;
                let base = memory::ensure_place(builder, base_value, &base._type)?;
                let type_id = lower_type(builder, &expression._type)?;
                let out = builder.fun_mut().new_place(type_id, None, false);
                let aggregate_type_id = lower_type(builder, aggregate_type)?;
                builder.emit(MIRInstrKind::AggregateOp(MIRAggregateOp::Place {
                    out,
                    op: MIRPlaceAggregateOp::Field {
                        base,
                        field: *member_index,
                        aggregate_type: aggregate_type_id,
                    },
                }));
                MIRValue::PlaceRef(out)
            }

            THIRExpressionKind::ArrayAccess {
                array,
                index,
                element_type,
            } => {
                let array_value = lower_expression(builder, array)?;
                let base = memory::ensure_place(builder, array_value, &array._type)?;
                let index = lower_expression(builder, index)?;
                let type_id = lower_type(builder, &expression._type)?;
                let out = builder.fun_mut().new_place(type_id, None, false);
                let element_type_id = lower_type(builder, element_type)?;
                builder.emit(MIRInstrKind::AggregateOp(MIRAggregateOp::Place {
                    out,
                    op: MIRPlaceAggregateOp::Index {
                        base,
                        index,
                        element_type: element_type_id,
                    },
                }));
                MIRValue::PlaceRef(out)
            }

            THIRExpressionKind::PatternIs { lhs, pattern } => {
                aggregates::lower_pattern_test(builder, lhs, pattern, &expression._type)?
            }

            THIRExpressionKind::Unpack {
                value, bindings, ..
            } => {
                let lowered_value = lower_expression(builder, value)?;

                let target = memory::ensure_place(builder, lowered_value, &value._type)?;
                let struct_type_id = lower_type(builder, &value._type)?;
                let base = builder.create(struct_type_id, None, false);

                builder.emit(MIRInstrKind::Assign {
                    target: MIRAssignTarget::Place(base),
                    value: MIRValue::Move(target),
                    ty: struct_type_id,
                });

                for binding in bindings {
                    let field_type = lower_type(builder, &binding.field_type)?;
                    let field_place = builder.fun_mut().new_place(
                        field_type,
                        Some(binding.field_name.clone()),
                        binding.field_type.is_nodrop(),
                    );

                    builder.emit(MIRInstrKind::AggregateOp(MIRAggregateOp::Place {
                        out: field_place,
                        op: MIRPlaceAggregateOp::Field {
                            base,
                            field: binding.field_index,
                            aggregate_type: struct_type_id,
                        },
                    }));
                    builder
                        .fun_mut()
                        .bind_local(binding.binding_local_id, MIRValue::PlaceRef(field_place));
                }

                MIRValue::Constant(MIRConstant::Unit)
            }

            THIRExpressionKind::TaggedUnionTag { value, sum_type } => {
                let base = lower_expression(builder, value)?;
                let type_id = lower_type(builder, &expression._type)?;
                let out = builder.fun_mut().new_register(type_id, None);
                let sum_type_id = lower_type(builder, sum_type)?;
                builder.emit(MIRInstrKind::AggregateOp(MIRAggregateOp::Value {
                    out,
                    op: MIRValueAggregateOp::Discriminant {
                        value: base,
                        sum_type: sum_type_id,
                    },
                }));
                MIRValue::Register(out)
            }
            THIRExpressionKind::TaggedUnionGet {
                value,
                variant_index,
                variant_type,
            } => {
                let base_value = lower_expression(builder, value)?;
                let sum_type = match &value._type.kind {
                    THIRTypeKind::MemoryReference { inner_type, .. } => {
                        builder.registry().resolve_type_id(*inner_type).clone()
                    }
                    _ => value._type.clone(),
                };
                let sum_type_id = lower_type(builder, &sum_type)?;
                let variant_type_id = lower_type(builder, variant_type)?;
                match base_value {
                    MIRValue::PlaceRef(base) => {
                        let out = builder.fun_mut().new_place(variant_type_id, None, false);
                        builder.emit(MIRInstrKind::AggregateOp(MIRAggregateOp::Place {
                            out,
                            op: MIRPlaceAggregateOp::Variant {
                                base,
                                variant: *variant_index,
                                sum_type: sum_type_id,
                            },
                        }));
                        MIRValue::PlaceRef(out)
                    }
                    value => {
                        let out = builder.fun_mut().new_register(variant_type_id, None);
                        builder.emit(MIRInstrKind::AggregateOp(MIRAggregateOp::Value {
                            out,
                            op: MIRValueAggregateOp::ProjectVariant {
                                variant: *variant_index,
                                value,
                                sum_type: sum_type_id,
                            },
                        }));
                        MIRValue::Register(out)
                    }
                }
            }
            THIRExpressionKind::TaggedUnionSet {
                target,
                variant_index,
                inner_value,
                sum_type,
            } => {
                let target_value = lower_expression(builder, target)?;
                let target = memory::ensure_place(builder, target_value, &target._type)?;
                let value = lower_expression(builder, inner_value)?;
                let sum_type_id = lower_type(builder, sum_type)?;
                let constructed = builder.fun_mut().new_register(sum_type_id, None);
                builder.emit(MIRInstrKind::AggregateOp(MIRAggregateOp::Value {
                    out: constructed,
                    op: MIRValueAggregateOp::Variant {
                        variant: *variant_index,
                        value,
                        sum_type: sum_type_id,
                    },
                }));
                builder.emit(MIRInstrKind::Assign {
                    target: MIRAssignTarget::Place(target),
                    value: MIRValue::Register(constructed),
                    ty: sum_type_id,
                });
                MIRValue::PlaceRef(target)
            }
            THIRExpressionKind::TaggedUnionInitializer {
                variant_index,
                value,
                sum_type,
            } => {
                let value = lower_expression(builder, value)?;
                let sum_type_id = lower_type(builder, sum_type)?;
                let type_id = lower_type(builder, &expression._type)?;
                let out = builder.fun_mut().new_register(type_id, None);
                builder.emit(MIRInstrKind::AggregateOp(MIRAggregateOp::Value {
                    out,
                    op: MIRValueAggregateOp::Variant {
                        variant: *variant_index,
                        value,
                        sum_type: sum_type_id,
                    },
                }));
                MIRValue::Register(out)
            }
            THIRExpressionKind::ArrayInitializer { elements, .. } => {
                let mut fields = Vec::with_capacity(elements.len());
                for (index, element) in elements.iter().enumerate() {
                    fields.push((index, lower_expression(builder, element)?));
                }
                let type_id = lower_type(builder, &expression._type)?;
                if let Ok(MIRTypeKind::Array { length, .. }) = builder.types().kind(type_id)
                    && fields.len() > *length
                {
                    return Err(cx_log::error::CXErr::new(
                        cx_log::error::message::CXStdErrMessage::error(
                            "MIR ARRAY ERROR",
                            format!(
                                "array initializer has {} elements but the array length is {}",
                                fields.len(),
                                length
                            ),
                        ),
                        cx_log::error::context::CXInternalContext::error(
                            "array initializer exceeds its concrete MIR array type",
                        ),
                    ));
                }
                let out = builder.fun_mut().new_register(type_id, None);
                builder.emit(MIRInstrKind::AggregateOp(MIRAggregateOp::Value {
                    out,
                    op: MIRValueAggregateOp::Construct {
                        ty: type_id,
                        fields,
                    },
                }));
                MIRValue::Register(out)
            }
            THIRExpressionKind::StructInitializer {
                initializations,
                struct_type,
            } => {
                let mut fields = Vec::with_capacity(initializations.len());
                for initialization in initializations {
                    fields.push((
                        initialization.field_index,
                        lower_expression(builder, &initialization.value)?,
                    ));
                }
                let type_id = lower_type(builder, &expression._type)?;
                let out = builder.fun_mut().new_register(type_id, None);
                let aggregate_type_id = lower_type(builder, struct_type)?;
                builder.emit(MIRInstrKind::AggregateOp(MIRAggregateOp::Value {
                    out,
                    op: MIRValueAggregateOp::Construct {
                        ty: aggregate_type_id,
                        fields,
                    },
                }));
                MIRValue::Register(out)
            }

            THIRExpressionKind::Break { staged } => {
                lower_control_exit(builder, MIRStagedExitKind::Break, *staged)?
            }
            THIRExpressionKind::Continue { staged } => {
                lower_control_exit(builder, MIRStagedExitKind::Continue, *staged)?
            }
            THIRExpressionKind::Goto { name } => {
                let target = if let Some(target) = builder.fun_mut().label(name) {
                    target
                } else {
                    let target = builder.fun_mut().new_block(name.clone());
                    builder.fun_mut().declare_label(name, target);
                    target
                };
                builder.emit(MIRInstrKind::Jump {
                    target: MIRBlockTarget::new(target),
                });
                let dead_block = builder.fun_mut().new_block("after.goto");
                builder.fun_mut().set_current_block(dead_block);
                MIRValue::Constant(MIRConstant::Unit)
            }
            THIRExpressionKind::Label { name, statement } => {
                let target = if let Some(target) = builder.fun_mut().label(name) {
                    target
                } else {
                    let target = builder.fun_mut().new_block(name.clone());
                    builder.fun_mut().declare_label(name, target);
                    target
                };
                builder.emit(MIRInstrKind::Jump {
                    target: MIRBlockTarget::new(target),
                });
                builder.fun_mut().set_current_block(target);
                lower_expression(builder, statement)?
            }
            THIRExpressionKind::If {
                condition,
                then_branch,
                else_branch,
            } => control_flow::lower_if(
                builder,
                condition,
                then_branch,
                else_branch.as_deref(),
                &expression._type,
            )?,
            THIRExpressionKind::While {
                condition,
                body,
                pre_eval,
            } => {
                control_flow::lower_while(builder, condition, body, *pre_eval)?;
                MIRValue::Constant(MIRConstant::Unit)
            }
            THIRExpressionKind::For {
                init,
                condition,
                increment,
                body,
            } => {
                control_flow::lower_for(builder, init, condition, increment, body)?;
                MIRValue::Constant(MIRConstant::Unit)
            }
            THIRExpressionKind::CSwitch {
                condition,
                cases,
                default,
            } => {
                control_flow::lower_switch(builder, condition, cases, default.as_deref())?;
                MIRValue::Constant(MIRConstant::Unit)
            }
            THIRExpressionKind::Match {
                condition,
                subject,
                arms,
                default,
                exhaustive,
            } => control_flow::lower_match(
                builder,
                condition,
                *subject,
                arms,
                default.as_deref(),
                *exhaustive,
                &expression._type,
            )?,
            THIRExpressionKind::Return {
                postcondition,
                value,
            } => {
                let value_expression = value.as_deref();
                let lowered_value = value_expression
                    .map(|value| lower_expression(builder, value))
                    .transpose()?;
                let lowered_value = lowered_value.unwrap_or(MIRValue::Constant(MIRConstant::Unit));
                let lowered_value = match (lowered_value, value_expression) {
                    (MIRValue::PlaceRef(place), Some(expression))
                        if !expression._type.is_memory_reference() =>
                    {
                        MIRValue::Copy(place)
                    }
                    (value, _) => value,
                };
                let value = match value_expression {
                    Some(expression) => Some(materialize_value(
                        builder,
                        lowered_value,
                        &expression._type,
                    )?),
                    None => None,
                };
                if let Some(postcondition) = postcondition {
                    builder
                        .fun_mut()
                        .push_scope(postcondition.condition.token_range.clone());
                    if let (Some(name), Some(value)) = (&postcondition.binding, value.clone()) {
                        builder.fun_mut().bind_named_value(name, value);
                    }
                    lower_expression(builder, &postcondition.condition)?;
                    let _ = builder.fun_mut().pop_scope();
                }

                auto_cleanup(builder, builder.fun().scope_stack().first().unwrap().id())?;
                builder.emit(MIRInstrKind::Return { value });
                MIRValue::Constant(MIRConstant::Unit)
            }

            THIRExpressionKind::Unreachable => {
                builder.emit(MIRInstrKind::Unreachable);
                MIRValue::Constant(MIRConstant::Unit)
            }

            THIRExpressionKind::Yield { value } => {
                let value = value
                    .as_deref()
                    .map(|value| lower_expression(builder, value))
                    .transpose()?;

                let Some((scope_id, block_id)) = builder
                    .fun()
                    .scope_stack()
                    .iter()
                    .rev()
                    .find_map(|scope| scope.yield_target.map(|t| (scope.id(), t)))
                else {
                    unreachable!("yield expression is not inside a yieldable scope");
                };

                let args = value.into_iter().collect();
                auto_cleanup(builder, scope_id)?;
                builder.emit(MIRInstrKind::Jump {
                    target: MIRBlockTarget::with_args(block_id, args),
                });
                MIRValue::Constant(MIRConstant::Unit)
            }

            THIRExpressionKind::Emit(inner) => {
                let (template, captures) = builder.capture_staged(inner, &[], None)?;
                let out = builder.fun_mut().new_register(template.result_type(), None);
                builder.emit(MIRInstrKind::MakeStaged {
                    out,
                    template,
                    captures,
                });
                MIRValue::Register(out)
            }
            THIRExpressionKind::Assert { condition, message } => {
                let condition = lower_expression(builder, condition)?;
                builder.emit(MIRInstrKind::Assert {
                    condition,
                    message: Some(message.clone()),
                });
                MIRValue::Constant(MIRConstant::Unit)
            }
            THIRExpressionKind::Defer {
                expression: deferred,
            } => {
                builder
                    .fun_mut()
                    .current_scope_mut()
                    .defered_expressions
                    .push(std::rc::Rc::new((**deferred).clone()));
                MIRValue::Constant(MIRConstant::Unit)
            }
            THIRExpressionKind::Block {
                statements,
                creates_scope,
            } => {
                let mut result = MIRValue::Constant(MIRConstant::Unit);
                if *creates_scope {
                    builder.fun_mut().push_scope(expression.token_range.clone());
                }

                builder.fun_mut().push_invisible_scope();

                for statement in statements {
                    result = lower_expression(builder, statement)?;
                }

                control_flow::auto_pop_scope(builder)?;
                if *creates_scope {
                    control_flow::auto_pop_scope(builder)?;
                }

                result
            }

            THIRExpressionKind::CallFunction {
                function,
                arguments,
                contract,
            } => calls::lower_call(builder, function, arguments, contract, &expression._type)?,
            THIRExpressionKind::VaStart { list, last } => {
                let list = lower_expression(builder, list)?;
                let last = lower_expression(builder, last)?;
                builder.emit(MIRInstrKind::VaStart { list, last });
                MIRValue::Constant(MIRConstant::Unit)
            }
            THIRExpressionKind::VaEnd { list } => {
                let list = lower_expression(builder, list)?;
                builder.emit(MIRInstrKind::VaEnd { list });
                MIRValue::Constant(MIRConstant::Unit)
            }
            THIRExpressionKind::VaArg { list, _type } => {
                let list = lower_expression(builder, list)?;
                let ty = lower_type(builder, _type)?;
                let out = builder.fun_mut().new_register(ty, None);
                builder.emit(MIRInstrKind::VaArg { out, list, ty });
                MIRValue::Register(out)
            }
            THIRExpressionKind::TypeConversion {
                operand,
                conversion,
            } => {
                if matches!(conversion, THIRCoercion::Unreachable) {
                    lower_expression(builder, operand)?;
                    return Ok(MIRValue::Constant(MIRConstant::Undefined));
                }
                if matches!(conversion, THIRCoercion::ReinterpretBits)
                    && matches!(&operand._type.kind, THIRTypeKind::MemoryReference { .. })
                    && matches!(
                        &expression._type.kind,
                        THIRTypeKind::MemoryReference { .. } | THIRTypeKind::PointerTo { .. }
                    )
                {
                    let value = lower_expression(builder, operand)?;
                    let type_id = lower_type(builder, &expression._type)?;
                    let is_str_reference = builder.registry().is_cx_str(&expression._type);
                    return Ok(match value {
                        MIRValue::PlaceRef(place) if !is_str_reference => {
                            let out = builder.fun_mut().new_register(type_id, None);
                            builder.emit(MIRInstrKind::AddressOf { out, place });
                            MIRValue::Register(out)
                        }
                        value => {
                            let out = builder.fun_mut().new_register(type_id, None);
                            builder.emit(MIRInstrKind::Coerce {
                                out,
                                operand: value,
                                coercion: operators::lower_coercion(
                                    conversion,
                                    &operand._type,
                                    &expression._type,
                                ),
                                to_type: type_id,
                            });
                            MIRValue::Register(out)
                        }
                    });
                }

                let value = lower_expression(builder, operand)?;
                let type_id = lower_type(builder, &expression._type)?;
                let out = builder.fun_mut().new_register(type_id, None);
                builder.emit(MIRInstrKind::Coerce {
                    out,
                    operand: value,
                    coercion: operators::lower_coercion(
                        conversion,
                        &operand._type,
                        &expression._type,
                    ),
                    to_type: type_id,
                });
                MIRValue::Register(out)
            }

            THIRExpressionKind::LifetimeStart { variable, _type } => {
                if let Some(MIRValue::PlaceRef(place)) = builder.fun().named(variable) {
                    builder.emit(MIRInstrKind::Initialize { place });
                    MIRValue::PlaceRef(place)
                } else {
                    MIRValue::Constant(MIRConstant::Unit)
                }
            }
            THIRExpressionKind::LifetimeEnd { .. } => MIRValue::Constant(MIRConstant::Unit),
            THIRExpressionKind::LeakLifetime { expression: inner } => {
                let value = lower_expression(builder, inner)?;
                if let MIRValue::PlaceRef(place) = value {
                    builder.emit(MIRInstrKind::Leak { place });
                    MIRValue::PlaceRef(place)
                } else {
                    value
                }
            }
            THIRExpressionKind::Unsafe { expression: inner } => lower_expression(builder, inner)?,
            THIRExpressionKind::StagedExpression { params, body } => {
                let params = params
                    .iter()
                    .map(|(_, local, ty)| (*local, ty))
                    .collect::<Vec<_>>();
                let (template, captures) = builder.capture_staged(body, &params, None)?;
                let out = builder.fun_mut().new_register(template.result_type(), None);
                builder.emit(MIRInstrKind::MakeStaged {
                    out,
                    template,
                    captures,
                });
                MIRValue::Register(out)
            }
        };

        Ok(value)
    })();
    builder.restore_source_range(previous_range);
    result
}
