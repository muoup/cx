mod aggregates;
mod calls;
mod control_flow;
mod memory;
mod operators;

use cx_log::CXResult;
use cx_mir::{
    MIRAggregateOp, MIRBlockTarget, MIRConstant, MIRInstrKind, MIRIntType, MIRPlaceAggregateOp,
    MIRValue, MIRValueAggregateOp,
};
use cx_thir::{
    THIRUnit,
    thir::{
        data::THIRTypeKind,
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

            THIRExpressionKind::Variable {
                name,
                local_id,
                location,
            } => match location {
                cx_thir::thir::expression::SymbolValueOrigin::Local => local_id
                    .and_then(|id| builder.local(id))
                    .map(MIRValue::Place)
                    .or_else(|| builder.named(name))
                    .unwrap_or(MIRValue::Constant(MIRConstant::Undefined)),
                cx_thir::thir::expression::SymbolValueOrigin::Global => MIRValue::Place(
                    cx_mir::MIRPlace::Global(builder.ensure_global(name, &expression._type)),
                ),
            },
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

            THIRExpressionKind::RegionCreate {
                _type,
                initial_value,
            } => {
                let type_id = builder.lower_type(_type);
                let place = builder.create(type_id, None, _type.is_nodrop());
                if let Some(initial_value) = initial_value {
                    let value = lower_expression(builder, initial_value)?;
                    let type_id = builder.lower_type(_type);
                    builder.emit(MIRInstrKind::Assign {
                        dest: place,
                        value,
                        ty: type_id,
                    });
                }
                MIRValue::Place(place)
            }
            THIRExpressionKind::BindRegion {
                name,
                local_id,
                _type,
                initial_region,
                adopting,
            } => {
                let initial = lower_expression(builder, initial_region)?;
                let initial = if !*adopting
                    && matches!(initial_region.kind, THIRExpressionKind::RegionCreate { .. })
                {
                    match initial {
                        MIRValue::Place(place) => MIRValue::Move(place),
                        value => value,
                    }
                } else {
                    initial
                };
                let place = if *adopting {
                    if let MIRValue::Place(place) = initial {
                        place
                    } else {
                        memory::assign_operand_to_place(builder, initial, _type, Some(name.clone()))
                    }
                } else {
                    memory::assign_operand_to_place(builder, initial, _type, Some(name.clone()))
                };
                builder.bind_local(*local_id, place);
                builder.bind_named(name, MIRValue::Place(place));
                MIRValue::Place(place)
            }
            THIRExpressionKind::RegionDuplicate { source } => {
                let value = lower_expression(builder, source)?;
                if let THIRTypeKind::MemoryReference { inner_type, .. } = &source._type.kind {
                    match value {
                        MIRValue::Place(place) | MIRValue::Copy(place) => MIRValue::Copy(place),
                        MIRValue::Move(place) => MIRValue::Move(place),
                        value => {
                            let pointee = builder.registry().resolve_type_id(*inner_type).clone();
                            let pointee_type = builder.lower_type(&pointee);
                            let type_id = builder.lower_type(&expression._type);
                            let out = builder.place(type_id, None, false);
                            builder.emit(MIRInstrKind::Dereference {
                                out,
                                pointer: value,
                                pointee_type,
                            });
                            MIRValue::Copy(out)
                        }
                    }
                } else {
                    match value {
                        MIRValue::Place(place) | MIRValue::Copy(place) => MIRValue::Copy(place),
                        MIRValue::Move(place) => MIRValue::Move(place),
                        value => value,
                    }
                }
            }
            THIRExpressionKind::RegionMove { source } => match lower_expression(builder, source)? {
                MIRValue::Place(place) | MIRValue::Copy(place) => MIRValue::Move(place),
                value => value,
            },
            THIRExpressionKind::RegionWrite { target, value } => {
                let assignment_type = builder.lower_type(&value._type);
                let target_value = lower_expression(builder, target)?;
                let value = lower_expression(builder, value)?;
                let place = match target_value {
                    MIRValue::Place(place) => place,
                    pointer => {
                        let THIRTypeKind::MemoryReference { inner_type, .. } = &target._type.kind
                        else {
                            return Err(cx_log::error::CXErr::new(
                                cx_log::error::message::CXStdErrMessage::error(
                                    "MIRLoweringError",
                                    "region write target is not addressable",
                                ),
                                cx_log::error::context::CXInternalContext::error(
                                    "region write target lowered to a non-reference value",
                                ),
                            ));
                        };
                        let pointee = builder.registry().resolve_type_id(*inner_type).clone();
                        let pointee_type = builder.lower_type(&pointee);
                        let type_id = builder.lower_type(&target._type);
                        let place = builder.place(type_id, None, false);
                        builder.emit(MIRInstrKind::Dereference {
                            out: place,
                            pointer,
                            pointee_type,
                        });
                        place
                    }
                };
                builder.emit(MIRInstrKind::Assign {
                    dest: place,
                    value,
                    ty: assignment_type,
                });
                MIRValue::Place(place)
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
                ..
            } => {
                let base_value = lower_expression(builder, value)?;
                let base = memory::ensure_place(builder, base_value, &value._type);
                let type_id = builder.lower_type(&expression._type);
                let out = builder.place(type_id, None, false);
                let sum_type_id = builder.lower_type(&value._type);
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
                    dest: target,
                    value: MIRValue::Register(constructed),
                    ty: sum_type_id,
                });
                MIRValue::Place(target)
            }
            THIRExpressionKind::ConstructTaggedUnion {
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
                let value = value
                    .as_deref()
                    .map(|value| {
                        let lowered = lower_expression(builder, value)?;
                        Ok(control_flow::capture_value(builder, lowered, &value._type))
                    })
                    .transpose()?;
                control_flow::unwind_lexical_scopes_to(builder, 1)?;
                control_flow::lower_root_defers(builder)?;
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
                let value = value
                    .as_deref()
                    .map(|value| {
                        let lowered = lower_expression(builder, value)?;
                        Ok(control_flow::capture_value(builder, lowered, &value._type))
                    })
                    .transpose()?;
                if let Some(target) = builder.yield_target() {
                    let depth = builder
                        .yield_scope_depth()
                        .expect("yield target is missing its lexical scope depth");
                    control_flow::unwind_lexical_scopes_to(builder, depth)?;
                    let args = builder
                        .yield_result()
                        .map(|_| value.unwrap_or(MIRValue::Constant(MIRConstant::Unit)))
                        .into_iter()
                        .collect();
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
                if *creates_scope
                    && !expression._type.is_unit()
                    && !builder.current_block_terminated()
                {
                    result = control_flow::capture_value(builder, result, &expression._type);
                }
                if *creates_scope {
                    control_flow::pop_lexical_scope(builder)?;
                }
                result
            }

            THIRExpressionKind::CallFunction {
                function,
                arguments,
                contract,
            } => calls::lower_call(builder, function, arguments, contract, &expression._type)?,
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
