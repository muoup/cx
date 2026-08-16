mod aggregates;
mod calls;
mod control_flow;
mod memory;
mod operators;

use cx_log::CXResult;
use cx_mir::{
    MIRAggregateOp, MIRAssignTarget, MIRBlockTarget, MIRConstant, MIRInstrKind, MIRIntType,
    MIRPlace, MIRPlaceAggregateOp, MIRValue, MIRValueAggregateOp,
};
use cx_thir::{
    THIRUnit,
    thir::{
        data::{THIRType, THIRTypeKind},
        expression::{THIRBinOp, THIRCoercion, THIRExpression, THIRExpressionKind, THIRIntBinOp},
    },
    type_context::THIRTypeContext,
};

use crate::builder::{MIRBuilder, integer_type};

pub(crate) fn lower_unit(builder: &mut MIRBuilder<'_>, thir: &THIRUnit) -> CXResult<()> {
    for (index, function) in thir.functions.iter().enumerate() {
        lower_function(builder, index, function)?;
    }
    Ok(())
}

fn lower_function(
    builder: &mut MIRBuilder<'_>,
    index: usize,
    function: &cx_thir::thir::data::THIRFunction,
) -> CXResult<()> {
    builder.start_function(index, function);
    lower_expression(builder, &function.body)?;
    if !builder.current_block_terminated() {
        control_flow::lower_root_defers(builder)?;
    }
    builder.finish_function();
    Ok(())
}

pub(super) fn materialize_value(
    builder: &mut MIRBuilder<'_>,
    value: MIRValue,
    ty: &THIRType,
) -> MIRValue {
    let place = match value {
        MIRValue::Copy(place) => Some((place, false)),
        MIRValue::Move(place) => Some((place, true)),
        value => return value,
    };
    let (place, moves) = place.expect("a place value was just matched");
    let type_id = builder.lower_type(ty);
    let out = builder.register(type_id, None);
    builder.emit(MIRInstrKind::Assign {
        target: MIRAssignTarget::Register(out),
        value: if moves {
            MIRValue::Move(place)
        } else {
            MIRValue::Copy(place)
        },
        ty: type_id,
    });
    MIRValue::Register(out)
}

fn lower_expression(
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
                    THIRTypeKind::Float { _type } => operators::lower_float_type(_type),
                    _ => cx_mir::MIRFloatType::F64,
                };
                MIRValue::Constant(MIRConstant::Float { value: *value, ty })
            }
            THIRExpressionKind::Unit => MIRValue::Constant(MIRConstant::Unit),
            THIRExpressionKind::SizeOf { _type } | THIRExpressionKind::AlignOf { _type } => {
                let type_id = builder.lower_type(_type);
                let layout = builder.unit().types.layout(type_id).map_err(|error| {
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

            THIRExpressionKind::Variable { local_id, .. } => builder
                .local_value(*local_id)
                .or_else(|| builder.local(*local_id).map(MIRValue::Place))
                .expect("local variable not found"),

            THIRExpressionKind::GlobalVariable { symbol } => MIRValue::Place(MIRPlace::Global(
                builder.ensure_global(symbol, &expression._type),
            )),

            THIRExpressionKind::ContractVariable { name, .. } => builder
                .named(name)
                .map(|value| match value {
                    MIRValue::Place(place) => {
                        if expression._type.is_memory_reference() {
                            MIRValue::Place(place)
                        } else {
                            MIRValue::Copy(place)
                        }
                    }
                    value => value,
                })
                .unwrap_or(MIRValue::Constant(MIRConstant::Undefined)),
            THIRExpressionKind::FunctionReference { name, debug_name } => {
                MIRValue::Constant(MIRConstant::Function(builder.ensure_function(
                    name,
                    &expression._type,
                    debug_name.as_ref(),
                )))
            }

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
                    let type_id = builder.lower_type(&expression._type);
                    let out = builder.register(type_id, None);
                    let lowered_op = operators::lower_binary_op(builder, op);
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
                let type_id = builder.lower_type(&expression._type);
                let out = builder.register(type_id, None);
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
                        MIRValue::Place(place) => MIRValue::Copy(place),
                        value => value,
                    });
                }

                let inner_type = source
                    ._type
                    .mem_ref_inner()
                    .expect("memory reference is missing its pointee type");
                let pointee = builder.registry().resolve_type_id(inner_type).clone();
                let pointee_type = builder.lower_type(&pointee);
                match value {
                    MIRValue::Place(place) => MIRValue::Copy(place),
                    MIRValue::Register(register)
                        if builder.register_type(register) == Some(pointee_type) =>
                    {
                        MIRValue::Register(register)
                    }
                    pointer => {
                        let out = builder.place(pointee_type, None, false);
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
                builder
                    .local_value(*local_id)
                    .or_else(|| builder.local(*local_id).map(MIRValue::Move))
                    .expect("move target local is missing")
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
                        MIRValue::Place(place) => {
                            builder.bind_local(*local_id, place);
                            builder.bind_named(name, MIRValue::Place(place));
                            MIRValue::Place(place)
                        }
                        value => {
                            let place = memory::assign_operand_to_place(
                                builder,
                                value,
                                _type,
                                Some(name.clone()),
                            );
                            builder.bind_local(*local_id, place);
                            builder.bind_named(name, MIRValue::Place(place));
                            MIRValue::Place(place)
                        }
                    }
                } else {
                    let type_id = builder.lower_type(_type);
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
                    builder.bind_local(*local_id, place);
                    builder.bind_named(name, MIRValue::Place(place));
                    MIRValue::Place(place)
                }
            }

            THIRExpressionKind::Assign { target, value } => {
                let assignment_type = builder.lower_type(&value._type);

                let target_value = lower_expression(builder, target)?;
                let target = memory::ensure_place(builder, target_value, &target._type);
                let value = lower_expression(builder, value)?;

                builder.emit(MIRInstrKind::Assign {
                    target: MIRAssignTarget::Place(target),
                    value,
                    ty: assignment_type,
                });
                MIRValue::Place(target)
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
                    let pointee_type = builder.lower_type(&pointee);
                    if inner_is_reference && expression_is_pointer {
                        value
                    } else {
                        let type_id = if expression_is_reference {
                            builder.lower_type(reference_type)
                        } else {
                            builder.lower_type(&expression._type)
                        };
                        let out = builder.place(type_id, None, false);
                        builder.emit(MIRInstrKind::Dereference {
                            out,
                            pointer: value,
                            pointee_type,
                        });
                        MIRValue::Place(out)
                    }
                } else if inner_is_pointer && !expression_is_pointer {
                    let THIRTypeKind::PointerTo { inner_type } = &inner._type.kind else {
                        unreachable!("pointer type was checked above")
                    };
                    let pointee = builder.registry().resolve_type_id(*inner_type).clone();
                    let pointee_type = builder.lower_type(&pointee);
                    let type_id = builder.lower_type(&expression._type);
                    let out = builder.place(type_id, None, false);
                    builder.emit(MIRInstrKind::Dereference {
                        out,
                        pointer: value,
                        pointee_type,
                    });
                    MIRValue::Place(out)
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
                let base = memory::ensure_place(builder, base_value, &base._type);
                let type_id = builder.lower_type(&expression._type);
                let out = builder.place(type_id, None, false);
                let aggregate_type_id = builder.lower_type(aggregate_type);
                builder.emit(MIRInstrKind::AggregateOp(MIRAggregateOp::Place {
                    out,
                    op: MIRPlaceAggregateOp::Field {
                        base,
                        field: *member_index,
                        aggregate_type: aggregate_type_id,
                    },
                }));
                MIRValue::Place(out)
            }
            
            THIRExpressionKind::ArrayAccess {
                array,
                index,
                element_type,
            } => {
                let array_value = lower_expression(builder, array)?;
                let base = memory::ensure_place(builder, array_value, &array._type);
                let index = lower_expression(builder, index)?;
                let type_id = builder.lower_type(&expression._type);
                let out = builder.place(type_id, None, false);
                let element_type_id = builder.lower_type(element_type);
                builder.emit(MIRInstrKind::AggregateOp(MIRAggregateOp::Place {
                    out,
                    op: MIRPlaceAggregateOp::Index {
                        base,
                        index,
                        element_type: element_type_id,
                    },
                }));
                MIRValue::Place(out)
            }

            THIRExpressionKind::PatternIs { lhs, pattern } => {
                aggregates::lower_pattern_test(builder, lhs, pattern, &expression._type)?
            }

            THIRExpressionKind::Unpack { local_id, struct_type, bindings, .. } => {
                let target = builder
                    .local(*local_id)
                    .expect("unpack target local is missing");
                let struct_type_id = builder.lower_type(struct_type);
                let base = builder.create(struct_type_id, None, false);
                builder.emit(MIRInstrKind::Assign {
                    target: MIRAssignTarget::Place(base),
                    value: MIRValue::Move(target),
                    ty: struct_type_id,
                });

                for binding in bindings {
                    let field_type = builder.lower_type(&binding.field_type);
                    let field_place = builder.place(
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
                    builder.bind_local(binding.binding_local_id, field_place);
                }

                MIRValue::Constant(MIRConstant::Unit)
            }

            THIRExpressionKind::TaggedUnionTag { value, sum_type } => {
                let base = lower_expression(builder, value)?;
                let type_id = builder.lower_type(&expression._type);
                let out = builder.register(type_id, None);
                let sum_type_id = builder.lower_type(sum_type);
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
                let sum_type_id = builder.lower_type(&sum_type);
                let variant_type_id = builder.lower_type(variant_type);
                match base_value {
                    MIRValue::Place(base) => {
                        let out = builder.place(variant_type_id, None, false);
                        builder.emit(MIRInstrKind::AggregateOp(MIRAggregateOp::Place {
                            out,
                            op: MIRPlaceAggregateOp::Variant {
                                base,
                                variant: *variant_index,
                                sum_type: sum_type_id,
                            },
                        }));
                        MIRValue::Place(out)
                    }
                    value => {
                        let out = builder.register(variant_type_id, None);
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
                let target = memory::ensure_place(builder, target_value, &target._type);
                let value = lower_expression(builder, inner_value)?;
                let sum_type_id = builder.lower_type(sum_type);
                let constructed = builder.register(sum_type_id, None);
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
                MIRValue::Place(target)
            }
            THIRExpressionKind::TaggedUnionInitializer {
                variant_index,
                value,
                sum_type,
            } => {
                let value = lower_expression(builder, value)?;
                let sum_type_id = builder.lower_type(sum_type);
                let type_id = builder.lower_type(&expression._type);
                let out = builder.register(type_id, None);
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
                let type_id = builder.lower_type(&expression._type);
                let out = builder.register(type_id, None);
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
                let type_id = builder.lower_type(&expression._type);
                let out = builder.register(type_id, None);
                let aggregate_type_id = builder.lower_type(struct_type);
                builder.emit(MIRInstrKind::AggregateOp(MIRAggregateOp::Value {
                    out,
                    op: MIRValueAggregateOp::Construct {
                        ty: aggregate_type_id,
                        fields,
                    },
                }));
                MIRValue::Register(out)
            }

            THIRExpressionKind::Break => {
                if let Some(target) = builder.break_target() {
                    if let Some(depth) = builder.break_scope_depth() {
                        control_flow::unwind_lexical_scopes_to(builder, depth)?;
                    }
                    builder.emit(MIRInstrKind::Jump {
                        target: MIRBlockTarget::new(target),
                    });
                } else {
                    builder.emit(MIRInstrKind::Unreachable);
                }
                MIRValue::Constant(MIRConstant::Unit)
            }
            THIRExpressionKind::Continue => {
                if let Some(target) = builder.continue_target() {
                    if let Some(depth) = builder.continue_scope_depth() {
                        control_flow::unwind_lexical_scopes_to(builder, depth)?;
                    }
                    builder.emit(MIRInstrKind::Jump {
                        target: MIRBlockTarget::new(target),
                    });
                } else {
                    builder.emit(MIRInstrKind::Unreachable);
                }
                MIRValue::Constant(MIRConstant::Unit)
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
                let value_type = value
                    .as_deref()
                    .map(|value| value._type.clone())
                    .unwrap_or_else(|| expression._type.clone());
                let has_value = value.is_some();
                let value = value
                    .as_deref()
                    .map(|value| lower_expression(builder, value))
                    .transpose()?;
                let value = control_flow::cleanup_value_for_return(
                    builder,
                    value.unwrap_or(MIRValue::Constant(MIRConstant::Unit)),
                    &value_type,
                )?;
                let value = has_value.then_some(value);
                if let Some(postcondition) = postcondition {
                    builder.push_named_scope();
                    if let (Some(name), Some(value)) = (&postcondition.binding, value.clone()) {
                        builder.bind_named(name, value);
                    }
                    lower_expression(builder, &postcondition.condition)?;
                    builder.pop_named_scope();
                }
                builder.emit(MIRInstrKind::Return { value });
                MIRValue::Constant(MIRConstant::Unit)
            }
            THIRExpressionKind::Yield { value } => {
                let value_type = value
                    .as_deref()
                    .map(|value| value._type.clone())
                    .unwrap_or_else(|| expression._type.clone());
                let value = value
                    .as_deref()
                    .map(|value| lower_expression(builder, value))
                    .transpose()?;
                if let Some(target) = builder.yield_target() {
                    let depth = builder
                        .yield_scope_depth()
                        .expect("yield target is missing its lexical scope depth");
                    let value = control_flow::cleanup_value_to(
                        builder,
                        depth,
                        value.unwrap_or(MIRValue::Constant(MIRConstant::Unit)),
                        &value_type,
                    )?;
                    let args = builder.yield_result().map(|_| value).into_iter().collect();
                    builder.emit(MIRInstrKind::Jump {
                        target: MIRBlockTarget::with_args(target, args),
                    });
                } else if value.is_some() {
                    builder.emit(MIRInstrKind::Unreachable);
                }
                MIRValue::Constant(MIRConstant::Unit)
            }
            THIRExpressionKind::Emit(inner) => {
                let value = lower_expression(builder, inner)?;
                builder.emit(MIRInstrKind::Emit {
                    value: value.clone(),
                });
                value
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
                builder.register_defer((**deferred).clone());
                MIRValue::Constant(MIRConstant::Unit)
            }
            THIRExpressionKind::Block {
                statements,
                creates_scope,
            } => {
                let mut result = MIRValue::Constant(MIRConstant::Unit);
                if *creates_scope {
                    builder.push_lexical_scope(expression.token_range.clone());
                }
                builder.push_named_scope();
                for statement in statements {
                    if builder.current_block_terminated() {
                        break;
                    }
                    result = lower_expression(builder, statement)?;
                }
                builder.pop_named_scope();
                if *creates_scope {
                    let (scope, defers) = builder.pop_lexical_scope();
                    if !builder.current_block_terminated() {
                        if expression._type.is_unit() {
                            control_flow::lower_scope_exit(builder, scope, &defers)?;
                        } else {
                            result = control_flow::finish_value_cleanup(
                                builder,
                                result,
                                &expression._type,
                                vec![(Some(scope), defers)],
                            )?;
                        }
                    }
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
                let ty = builder.lower_type(_type);
                let out = builder.register(ty, None);
                builder.emit(MIRInstrKind::VaArg { out, list, ty });
                MIRValue::Register(out)
            }
            THIRExpressionKind::TypeConversion {
                operand,
                conversion,
            } => {
                if matches!(conversion, THIRCoercion::ReinterpretBits)
                    && matches!(&operand._type.kind, THIRTypeKind::MemoryReference { .. })
                    && matches!(
                        &expression._type.kind,
                        THIRTypeKind::MemoryReference { .. } | THIRTypeKind::PointerTo { .. }
                    )
                {
                    let value = lower_expression(builder, operand)?;
                    let type_id = builder.lower_type(&expression._type);
                    let is_str_reference = builder.registry().is_cx_str(&expression._type);
                    return Ok(match value {
                        MIRValue::Place(place) if !is_str_reference => {
                            let out = builder.register(type_id, None);
                            builder.emit(MIRInstrKind::AddressOf { out, place });
                            MIRValue::Register(out)
                        }
                        value => {
                            let out = builder.register(type_id, None);
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
                let type_id = builder.lower_type(&expression._type);
                let out = builder.register(type_id, None);
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
                if let Some(MIRValue::Place(place)) = builder.named(variable) {
                    builder.emit(MIRInstrKind::Initialize { place });
                    MIRValue::Place(place)
                } else {
                    MIRValue::Constant(MIRConstant::Unit)
                }
            }
            THIRExpressionKind::LifetimeEnd { .. } => MIRValue::Constant(MIRConstant::Unit),
            THIRExpressionKind::LeakLifetime { expression: inner } => {
                let value = lower_expression(builder, inner)?;
                if let MIRValue::Place(place) = value {
                    builder.emit(MIRInstrKind::Leak { place });
                    MIRValue::Place(place)
                } else {
                    value
                }
            }
            THIRExpressionKind::Unsafe { expression: inner } => lower_expression(builder, inner)?,
        };

        Ok(value)
    })();
    builder.restore_source_range(previous_range);
    result
}
