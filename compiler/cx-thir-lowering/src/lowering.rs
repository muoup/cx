use cx_log::CXResult;
use cx_mir::{
    MIRAggregateOp, MIRBinaryOp, MIRBlockTarget, MIRCoercion, MIRConstant, MIRFloatBinaryOp,
    MIRInstrKind, MIRIntBinaryOp, MIRIntType, MIRPlaceAggregateOp, MIRPointerBinaryOp,
    MIRPointerOffsetOp, MIRUnaryOp, MIRValue, MIRValueAggregateOp,
};
use cx_thir::{
    thir::{
        data::{THIRFloatType, THIRFunction, THIRIntType, THIRType, THIRTypeKind},
        expression::{
            THIRBinOp, THIRCoercion, THIRExpression, THIRExpressionKind, THIRFloatBinOp,
            THIRIntBinOp, THIRPtrBinOp, THIRPtrDiffBinOp, THIRUnOp,
        },
        pattern::THIRPattern,
    },
    type_context::THIRTypeContext,
    THIRUnit,
};

use crate::builder::{integer_type, MIRBuilder};

pub(crate) fn lower_unit(builder: &mut MIRBuilder<'_>, thir: &THIRUnit) -> CXResult<()> {
    for (index, function) in thir.functions.iter().enumerate() {
        lower_function(builder, index, function)?;
    }
    Ok(())
}

fn lower_function(
    builder: &mut MIRBuilder<'_>,
    index: usize,
    function: &THIRFunction,
) -> CXResult<()> {
    builder.start_function(index, function);
    lower_expression(builder, &function.body)?;
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
                    THIRTypeKind::Float { _type } => lower_float_type(_type),
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
                .unwrap_or(MIRValue::Constant(MIRConstant::Undefined)),
            THIRExpressionKind::FunctionReference { name } => MIRValue::Constant(
                MIRConstant::Function(builder.ensure_function(name, &expression._type)),
            ),

            THIRExpressionKind::BinaryOperation { lhs, rhs, op } => {
                if matches!(
                    op,
                    THIRBinOp::Integer {
                        op: THIRIntBinOp::LAND | THIRIntBinOp::LOR,
                        ..
                    }
                ) {
                    lower_short_circuit(builder, lhs, rhs, op, &expression._type)?
                } else {
                    let lhs = lower_expression(builder, lhs)?;
                    let rhs = lower_expression(builder, rhs)?;
                    let out = builder.new_register_for_type(&expression._type, None);
                    let lowered_op = lower_binary_op(builder, op);
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
                let out = builder.new_register_for_type(&expression._type, None);
                builder.emit(MIRInstrKind::UnOp {
                    out,
                    op: lower_unary_op(op, &operand._type),
                    operand: lowered,
                });
                MIRValue::Register(out)
            }

            THIRExpressionKind::RegionCreate {
                _type,
                initial_value,
            } => {
                let place = builder.create_place_for_type(_type, None, _type.is_nodrop());
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
                let place = if *adopting {
                    if let MIRValue::Place(place) = initial {
                        place
                    } else {
                        assign_operand_to_place(builder, initial, _type, Some(name.clone()))
                    }
                } else {
                    assign_operand_to_place(builder, initial, _type, Some(name.clone()))
                };
                builder.bind_local(*local_id, place);
                builder.bind_named(name, MIRValue::Place(place));
                MIRValue::Place(place)
            }
            THIRExpressionKind::RegionDuplicate { source } => {
                let value = lower_expression(builder, source)?;
                if let THIRTypeKind::MemoryReference { inner_type, .. } = &source._type.kind {
                    match value {
                        MIRValue::Place(_) | MIRValue::Move(_) => value,
                        value => {
                            let pointee = builder.registry().resolve_type_id(*inner_type).clone();
                            let pointee_type = builder.lower_type(&pointee);
                            let out = builder.declare_place_for_type(&source._type, None);
                            builder.emit(MIRInstrKind::Dereference {
                                out,
                                pointer: value,
                                pointee_type,
                            });
                            MIRValue::Place(out)
                        }
                    }
                } else {
                    MIRValue::Place(assign_operand_to_place(
                        builder,
                        value,
                        &expression._type,
                        None,
                    ))
                }
            }
            THIRExpressionKind::RegionMove { source } => match lower_expression(builder, source)? {
                MIRValue::Place(place) => MIRValue::Move(place),
                value => value,
            },
            THIRExpressionKind::RegionWrite { target, value } => {
                let assignment_type = builder.lower_type(&value._type);
                let target_value = lower_expression(builder, target)?;
                let value = lower_expression(builder, value)?;
                let place = match target_value {
                    MIRValue::Place(place) | MIRValue::Move(place) => place,
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
                        let place = builder.declare_place_for_type(&target._type, None);
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

                // A type change involving a memory reference carries an
                // address, not the pointer bits as a scalar value. Preserve
                // that address as an explicit MIR place; this covers both
                // dereference expressions (`*ptr`) and ownership forwarding
                // such as `@adopt(place)` or returning a structured lvalue.
                let expression_is_reference =
                    matches!(&expression._type.kind, THIRTypeKind::MemoryReference { .. });
                let inner_is_reference =
                    matches!(&inner._type.kind, THIRTypeKind::MemoryReference { .. });
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
                    if inner_is_reference && !expression_is_reference {
                        match value {
                            MIRValue::Place(_) | MIRValue::Move(_) => value,
                            value => {
                                let out = builder.declare_place_for_type(reference_type, None);
                                builder.emit(MIRInstrKind::Dereference {
                                    out,
                                    pointer: value,
                                    pointee_type,
                                });
                                MIRValue::Place(out)
                            }
                        }
                    } else {
                        let out = builder.declare_place_for_type(reference_type, None);
                        builder.emit(MIRInstrKind::Dereference {
                            out,
                            pointer: value,
                            pointee_type,
                        });
                        MIRValue::Place(out)
                    }
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
                let base = ensure_place(builder, base_value, aggregate_type);
                let out = builder.declare_place_for_type(&expression._type, None);
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
                let base = ensure_place(builder, array_value, &array._type);
                let index = lower_expression(builder, index)?;
                let out = builder.declare_place_for_type(&expression._type, None);
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
                lower_pattern_test(builder, lhs, pattern, &expression._type)?
            }
            THIRExpressionKind::TaggedUnionTag { value, sum_type } => {
                let base = lower_expression(builder, value)?;
                let out = builder.new_register_for_type(&expression._type, None);
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
                variant_type: _,
            } => {
                let base_value = lower_expression(builder, value)?;
                let base = ensure_place(builder, base_value, &value._type);
                let out = builder.declare_place_for_type(&expression._type, None);
                let sum_type_id = builder.lower_type(&value._type);
                builder.emit(MIRInstrKind::AggregateOp(MIRAggregateOp::Place {
                    out,
                    op: MIRPlaceAggregateOp::Variant {
                        base,
                        // Tagged-union payload storage is shared. Pattern tests and
                        // variant switches supply the semantic variant when known.
                        variant: 0,
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
                let target = ensure_place(builder, target_value, sum_type);
                let value = lower_expression(builder, inner_value)?;
                let sum_type_id = builder.lower_type(sum_type);
                let constructed = builder.new_register(sum_type_id, None);
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
                let out = builder.new_register_for_type(&expression._type, None);
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
                let out = builder.new_register_for_type(&expression._type, None);
                let type_id = builder.lower_type(&expression._type);
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
                let out = builder.new_register_for_type(&expression._type, None);
                let type_id = builder.lower_type(struct_type);
                builder.emit(MIRInstrKind::AggregateOp(MIRAggregateOp::Value {
                    out,
                    op: MIRValueAggregateOp::Construct {
                        ty: type_id,
                        fields,
                    },
                }));
                MIRValue::Register(out)
            }

            THIRExpressionKind::Break { cleanups, .. } => {
                lower_cleanups(builder, cleanups)?;
                if let Some(target) = builder.break_target() {
                    builder.emit(MIRInstrKind::Jump {
                        target: MIRBlockTarget::new(target),
                    });
                } else {
                    builder.emit(MIRInstrKind::Unreachable);
                }
                MIRValue::Constant(MIRConstant::Unit)
            }
            THIRExpressionKind::Continue { cleanups, .. } => {
                lower_cleanups(builder, cleanups)?;
                if let Some(target) = builder.continue_target() {
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
            } => lower_if(
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
                lower_while(builder, condition, body, *pre_eval)?;
                MIRValue::Constant(MIRConstant::Unit)
            }
            THIRExpressionKind::For {
                init,
                condition,
                increment,
                body,
            } => {
                lower_for(builder, init, condition, increment, body)?;
                MIRValue::Constant(MIRConstant::Unit)
            }
            THIRExpressionKind::CSwitch {
                condition,
                cases,
                default,
            } => {
                lower_switch(builder, condition, cases, default.as_deref())?;
                MIRValue::Constant(MIRConstant::Unit)
            }
            THIRExpressionKind::Match {
                condition,
                subject,
                arms,
                default,
                exhaustive,
            } => lower_match(
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
                cleanups,
            } => {
                let return_type = value.as_deref().map(|value| value._type.clone());
                let mut value = value
                    .as_deref()
                    .map(|value| lower_expression(builder, value))
                    .transpose()?;
                if !cleanups.is_empty() {
                    if let (Some(current), Some(ty)) = (value.take(), return_type.as_ref()) {
                        let saved = assign_operand_to_place(builder, current, ty, None);
                        value = Some(MIRValue::Place(saved));
                    }
                }
                lower_cleanups(builder, cleanups)?;
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
            THIRExpressionKind::Yield {
                value,
                target_scope: _,
                cleanups,
            } => {
                let value = value
                    .as_deref()
                    .map(|value| lower_expression(builder, value))
                    .transpose()?
                    .unwrap_or(MIRValue::Constant(MIRConstant::Unit));
                lower_cleanups(builder, cleanups)?;
                if let Some(target) = builder.yield_target() {
                    builder.record_yield();
                    builder.emit(MIRInstrKind::Jump {
                        target: MIRBlockTarget::with_args(target, vec![value]),
                    });
                } else {
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
            THIRExpressionKind::Block { statements } => {
                let mut result = MIRValue::Constant(MIRConstant::Unit);
                builder.push_named_scope();
                for statement in statements {
                    if builder.current_block_terminated() {
                        break;
                    }
                    result = lower_expression(builder, statement)?;
                }
                builder.pop_named_scope();
                result
            }

            THIRExpressionKind::CallFunction {
                function,
                arguments,
                contract,
            } => lower_call(builder, function, arguments, contract, &expression._type)?,
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
                    let place = ensure_place(builder, value, &operand._type);
                    let type_id = builder.lower_type(&expression._type);
                    let out = builder.new_register(type_id, None);
                    builder.emit(MIRInstrKind::AddressOf { out, place });
                    return Ok(MIRValue::Register(out));
                }

                let value = lower_expression(builder, operand)?;
                let type_id = builder.lower_type(&expression._type);
                let out = builder.new_register(type_id, None);
                builder.emit(MIRInstrKind::Coerce {
                    out,
                    operand: value,
                    coercion: lower_coercion(conversion, &operand._type, &expression._type),
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

fn lower_if(
    builder: &mut MIRBuilder<'_>,
    condition: &THIRExpression,
    then_branch: &THIRExpression,
    else_branch: Option<&THIRExpression>,
    result_type: &THIRType,
) -> CXResult<MIRValue> {
    let condition = lower_expression(builder, condition)?;
    let then_block = builder.new_block("if.then");
    let else_block = builder.new_block("if.else");
    let merge_block = builder.new_block("if.merge");
    let result = (!matches!(result_type.kind, THIRTypeKind::Unit))
        .then(|| builder.add_block_param_for_type(merge_block, result_type, None));
    builder.emit(MIRInstrKind::Branch {
        cond: condition,
        true_target: MIRBlockTarget::new(then_block),
        false_target: MIRBlockTarget::new(else_block),
    });

    let mut has_incoming = false;
    builder.set_current_block(then_block);
    let then_value = lower_expression(builder, then_branch)?;
    if !builder.current_block_terminated() {
        let args = result.map(|_| vec![then_value]).unwrap_or_default();
        builder.emit(MIRInstrKind::Jump {
            target: MIRBlockTarget::with_args(merge_block, args),
        });
        has_incoming = true;
    }

    builder.set_current_block(else_block);
    let else_value = else_branch
        .map(|branch| lower_expression(builder, branch))
        .transpose()?
        .unwrap_or(MIRValue::Constant(MIRConstant::Unit));
    if !builder.current_block_terminated() {
        let args = result.map(|_| vec![else_value]).unwrap_or_default();
        builder.emit(MIRInstrKind::Jump {
            target: MIRBlockTarget::with_args(merge_block, args),
        });
        has_incoming = true;
    }

    builder.set_current_block(merge_block);
    match (result, has_incoming) {
        (Some(result), true) => Ok(MIRValue::Register(result)),
        (Some(_), false) => Ok(MIRValue::Constant(MIRConstant::Undefined)),
        (None, _) => Ok(MIRValue::Constant(MIRConstant::Unit)),
    }
}

fn lower_short_circuit(
    builder: &mut MIRBuilder<'_>,
    lhs: &THIRExpression,
    rhs: &THIRExpression,
    op: &THIRBinOp,
    result_type: &THIRType,
) -> CXResult<MIRValue> {
    let lhs_value = lower_expression(builder, lhs)?;
    let rhs_block = builder.new_block("logical.rhs");
    let merge_block = builder.new_block("logical.merge");
    let result = builder.add_block_param_for_type(merge_block, result_type, None);
    let is_and = matches!(
        op,
        THIRBinOp::Integer {
            op: THIRIntBinOp::LAND,
            ..
        }
    );
    let rhs_target = MIRBlockTarget::new(rhs_block);
    let merge_target = MIRBlockTarget::with_args(merge_block, vec![lhs_value.clone()]);
    builder.emit(MIRInstrKind::Branch {
        cond: lhs_value,
        true_target: if is_and {
            rhs_target.clone()
        } else {
            merge_target.clone()
        },
        false_target: if is_and { merge_target } else { rhs_target },
    });

    builder.set_current_block(rhs_block);
    let rhs_value = lower_expression(builder, rhs)?;
    if !builder.current_block_terminated() {
        builder.emit(MIRInstrKind::Jump {
            target: MIRBlockTarget::with_args(merge_block, vec![rhs_value]),
        });
    }

    builder.set_current_block(merge_block);
    Ok(MIRValue::Register(result))
}

fn lower_while(
    builder: &mut MIRBuilder<'_>,
    condition: &THIRExpression,
    body: &THIRExpression,
    pre_eval: bool,
) -> CXResult<()> {
    let condition_block = builder.new_block("while.condition");
    let body_block = builder.new_block("while.body");
    let exit_block = builder.new_block("while.exit");
    builder.emit(MIRInstrKind::Jump {
        target: MIRBlockTarget::new(if pre_eval {
            condition_block
        } else {
            body_block
        }),
    });

    builder.set_current_block(condition_block);
    let condition = lower_expression(builder, condition)?;
    builder.emit(MIRInstrKind::Branch {
        cond: condition,
        true_target: MIRBlockTarget::new(body_block),
        false_target: MIRBlockTarget::new(exit_block),
    });

    builder.set_current_block(body_block);
    builder.push_loop(exit_block, Some(condition_block));
    lower_expression(builder, body)?;
    builder.pop_loop();
    if !builder.current_block_terminated() {
        builder.emit(MIRInstrKind::Jump {
            target: MIRBlockTarget::new(condition_block),
        });
    }
    builder.set_current_block(exit_block);
    Ok(())
}

fn lower_for(
    builder: &mut MIRBuilder<'_>,
    init: &THIRExpression,
    condition: &THIRExpression,
    increment: &THIRExpression,
    body: &THIRExpression,
) -> CXResult<()> {
    lower_expression(builder, init)?;
    let condition_block = builder.new_block("for.condition");
    let body_block = builder.new_block("for.body");
    let increment_block = builder.new_block("for.increment");
    let exit_block = builder.new_block("for.exit");
    builder.emit(MIRInstrKind::Jump {
        target: MIRBlockTarget::new(condition_block),
    });

    builder.set_current_block(condition_block);
    let condition = lower_expression(builder, condition)?;
    builder.emit(MIRInstrKind::Branch {
        cond: condition,
        true_target: MIRBlockTarget::new(body_block),
        false_target: MIRBlockTarget::new(exit_block),
    });

    builder.set_current_block(body_block);
    builder.push_loop(exit_block, Some(increment_block));
    lower_expression(builder, body)?;
    builder.pop_loop();
    if !builder.current_block_terminated() {
        builder.emit(MIRInstrKind::Jump {
            target: MIRBlockTarget::new(increment_block),
        });
    }

    builder.set_current_block(increment_block);
    lower_expression(builder, increment)?;
    if !builder.current_block_terminated() {
        builder.emit(MIRInstrKind::Jump {
            target: MIRBlockTarget::new(condition_block),
        });
    }
    builder.set_current_block(exit_block);
    Ok(())
}

fn lower_switch(
    builder: &mut MIRBuilder<'_>,
    condition: &THIRExpression,
    cases: &[(Box<THIRExpression>, Box<THIRExpression>)],
    default: Option<&THIRExpression>,
) -> CXResult<()> {
    let value = lower_expression(builder, condition)?;
    let exit = builder.new_block("switch.exit");
    let default_block = default
        .map(|_| builder.new_block("switch.default"))
        .unwrap_or(exit);
    let mut targets = Vec::with_capacity(cases.len());
    let mut bodies = Vec::with_capacity(cases.len());
    for (case, _) in cases {
        let block = builder.new_block("switch.case");
        targets.push((constant_from_expression(case), MIRBlockTarget::new(block)));
        bodies.push(block);
    }
    builder.emit(MIRInstrKind::IntSwitch {
        value,
        cases: targets,
        default: Some(MIRBlockTarget::new(default_block)),
    });

    builder.push_loop(exit, None);
    for ((_, body), block) in cases.iter().zip(bodies) {
        builder.set_current_block(block);
        lower_expression(builder, body)?;
        if !builder.current_block_terminated() {
            builder.emit(MIRInstrKind::Jump {
                target: MIRBlockTarget::new(exit),
            });
        }
    }
    if let Some(default) = default {
        builder.set_current_block(default_block);
        lower_expression(builder, default)?;
        if !builder.current_block_terminated() {
            builder.emit(MIRInstrKind::Jump {
                target: MIRBlockTarget::new(exit),
            });
        }
    }
    builder.pop_loop();
    builder.set_current_block(exit);
    Ok(())
}

fn lower_match(
    builder: &mut MIRBuilder<'_>,
    condition: &THIRExpression,
    subject: cx_thir::thir::expression::THIRLocalID,
    arms: &[(THIRPattern, Box<THIRExpression>)],
    default: Option<&THIRExpression>,
    exhaustive: bool,
    result_type: &THIRType,
) -> CXResult<MIRValue> {
    let subject_value = lower_expression(builder, condition)?;
    let subject_place = ensure_place(builder, subject_value.clone(), &condition._type);
    builder.bind_local(subject, subject_place);

    let variant_match = matches!(
        condition._type.kind,
        THIRTypeKind::TaggedUnion { .. } | THIRTypeKind::MemoryReference { .. }
    );
    let semantic_sum_type = builder
        .registry()
        .mem_ref_inner(&condition._type)
        .unwrap_or(&condition._type)
        .clone();
    let exit = builder.new_block("match.exit");
    let value_match = !matches!(result_type.kind, THIRTypeKind::Unit);
    if value_match {
        builder.push_yield_for_type(exit, result_type);
    }
    let synthetic_unreachable = default.is_none() && (exhaustive || value_match);
    let default_block = default
        .map(|_| builder.new_block("match.default"))
        .or_else(|| synthetic_unreachable.then(|| builder.new_block("match.unreachable")))
        .unwrap_or(exit);
    let mut blocks = Vec::with_capacity(arms.len());
    for _ in arms {
        blocks.push(builder.new_block("match.arm"));
    }
    let default_target = Some(MIRBlockTarget::new(default_block));
    if variant_match {
        let cases = arms
            .iter()
            .zip(&blocks)
            .map(|((pattern, _), block)| {
                let THIRPattern::TaggedUnionVariant { variant_index, .. } = pattern else {
                    panic!("tagged-union match contains a non-variant pattern");
                };
                (*variant_index, MIRBlockTarget::new(*block))
            })
            .collect();
        let sum_type_id = builder.lower_type(&semantic_sum_type);
        builder.emit(MIRInstrKind::VariantSwitch {
            subject: subject_place,
            sum_type: sum_type_id,
            cases,
            default: default_target,
        });
    } else {
        let cases = arms
            .iter()
            .zip(&blocks)
            .map(|((pattern, _), block)| {
                (constant_from_pattern(pattern), MIRBlockTarget::new(*block))
            })
            .collect();
        builder.emit(MIRInstrKind::IntSwitch {
            value: MIRValue::Place(subject_place),
            cases,
            default: default_target,
        });
    }

    builder.push_loop(exit, None);
    for ((pattern, body), block) in arms.iter().zip(blocks) {
        builder.set_current_block(block);
        bind_pattern_payload(builder, pattern, subject_place, &condition._type);
        let value = lower_expression(builder, body)?;
        if !builder.current_block_terminated() {
            let args = if value_match {
                builder.record_yield();
                vec![value]
            } else {
                Vec::new()
            };
            builder.emit(MIRInstrKind::Jump {
                target: MIRBlockTarget::with_args(exit, args),
            });
        }
    }
    if let Some(default) = default {
        builder.set_current_block(default_block);
        let value = lower_expression(builder, default)?;
        if !builder.current_block_terminated() {
            let args = if value_match {
                builder.record_yield();
                vec![value]
            } else {
                Vec::new()
            };
            builder.emit(MIRInstrKind::Jump {
                target: MIRBlockTarget::with_args(exit, args),
            });
        }
    }
    if synthetic_unreachable {
        builder.set_current_block(default_block);
        builder.emit(MIRInstrKind::Unreachable);
    }
    builder.pop_loop();
    let yields = value_match.then(|| builder.pop_yield());
    builder.set_current_block(exit);
    match yields {
        Some(yields) if yields.has_incoming => Ok(MIRValue::Register(yields.result)),
        Some(_) => Ok(MIRValue::Constant(MIRConstant::Undefined)),
        None => Ok(MIRValue::Constant(MIRConstant::Unit)),
    }
}

fn lower_pattern_test(
    builder: &mut MIRBuilder<'_>,
    lhs: &THIRExpression,
    pattern: &THIRPattern,
    result_type: &THIRType,
) -> CXResult<MIRValue> {
    let lhs_value = lower_expression(builder, lhs)?;
    let (tested, constant) = match pattern {
        THIRPattern::TaggedUnionVariant {
            sum_type,
            variant_index,
            inner_local_id,
            ..
        } => {
            let base = ensure_place(builder, lhs_value.clone(), sum_type);
            if let Some(local_id) = inner_local_id {
                let payload_type = sum_variant_type(builder, sum_type, *variant_index);
                let payload = builder.declare_place_for_type(&payload_type, None);
                let sum_type_id = builder.lower_type(sum_type);
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
            let tag_type = builder.lower_type(&THIRType::from(THIRTypeKind::Integer {
                _type: THIRIntType::I8,
                signed: false,
            }));
            let tag = builder.new_register(tag_type, None);
            let sum_type_id = builder.lower_type(sum_type);
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
                ty: lower_float_type(*ty),
            },
        ),
    };
    let result_type_id = builder.lower_type(result_type);
    let out = builder.new_register(result_type_id, None);
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

fn bind_pattern_payload(
    builder: &mut MIRBuilder<'_>,
    pattern: &THIRPattern,
    subject: cx_mir::MIRPlace,
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
        let payload = builder.declare_place_for_type(&payload_type, inner_name.clone());
        let sum_type_id = builder.lower_type(sum_type);
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
fn sum_variant_type(
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

fn lower_call(
    builder: &mut MIRBuilder<'_>,
    function: &THIRExpression,
    arguments: &[THIRExpression],
    contract: &cx_thir::thir::expression::THIRFnContract,
    result_type: &THIRType,
) -> CXResult<MIRValue> {
    let callee = lower_expression(builder, function)?;
    let mut args = Vec::with_capacity(arguments.len());
    for argument in arguments {
        args.push(lower_expression(builder, argument)?);
    }

    if let Some(precondition) = &contract.precondition {
        builder.push_named_scope();
        let parameter_names = builder
            .registry()
            .intern_signature(&function._type)
            .map(|signature| {
                signature
                    .params
                    .iter()
                    .map(|parameter| parameter.name.clone())
                    .collect::<Vec<_>>()
            })
            .unwrap_or_default();
        for (name, argument) in parameter_names.iter().zip(args.iter().cloned()) {
            if let Some(name) = name {
                builder.bind_named(name, argument);
            }
        }
        lower_expression(builder, precondition)?;
        builder.pop_named_scope();
    }

    let returns_value = !matches!(result_type.kind, THIRTypeKind::Unit);
    let out = returns_value.then(|| builder.new_register_for_type(result_type, None));
    builder.emit(MIRInstrKind::Call {
        out,
        callee,
        args: args.clone(),
    });
    let value = out
        .map(MIRValue::Register)
        .unwrap_or(MIRValue::Constant(MIRConstant::Unit));

    if let Some(postcondition) = &contract.postcondition {
        builder.push_named_scope();
        let parameter_names = builder
            .registry()
            .intern_signature(&function._type)
            .map(|signature| {
                signature
                    .params
                    .iter()
                    .map(|parameter| parameter.name.clone())
                    .collect::<Vec<_>>()
            })
            .unwrap_or_default();
        for (name, argument) in parameter_names.iter().zip(args) {
            if let Some(name) = name {
                builder.bind_named(name, argument);
            }
        }
        if let Some(name) = &postcondition.binding {
            builder.bind_named(name, value.clone());
        }
        let condition = lower_expression(builder, &postcondition.condition)?;
        builder.emit(MIRInstrKind::Assume { condition });
        builder.pop_named_scope();
    }

    Ok(value)
}

fn lower_cleanups(builder: &mut MIRBuilder<'_>, cleanups: &[THIRExpression]) -> CXResult<()> {
    for cleanup in cleanups {
        if builder.current_block_terminated() {
            break;
        }
        lower_expression(builder, cleanup)?;
    }
    Ok(())
}

fn assign_operand_to_place(
    builder: &mut MIRBuilder<'_>,
    value: MIRValue,
    ty: &THIRType,
    name: Option<cx_util::identifier::CXIdent>,
) -> cx_mir::MIRPlace {
    let type_id = builder.lower_type(ty);
    let place = builder.create_place_with_nodrop(type_id, name, ty.is_nodrop());
    builder.emit(MIRInstrKind::Assign {
        dest: place,
        value,
        ty: type_id,
    });
    place
}

fn ensure_place(builder: &mut MIRBuilder<'_>, value: MIRValue, ty: &THIRType) -> cx_mir::MIRPlace {
    match value {
        MIRValue::Place(place) | MIRValue::Move(place) => place,
        value => assign_operand_to_place(builder, value, ty, None),
    }
}

fn constant_from_expression(expression: &THIRExpression) -> MIRConstant {
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
                THIRTypeKind::Float { _type } => lower_float_type(_type),
                _ => cx_mir::MIRFloatType::F64,
            },
        },
        _ => MIRConstant::Undefined,
    }
}

fn constant_from_pattern(pattern: &THIRPattern) -> MIRConstant {
    match pattern {
        THIRPattern::Integer(value) => MIRConstant::Integer {
            value: *value as i128,
            ty: MIRIntType::I64,
            signed: true,
        },
        THIRPattern::Float(value, ty) => MIRConstant::Float {
            value: *value,
            ty: lower_float_type(*ty),
        },
        THIRPattern::TaggedUnionVariant { variant_index, .. } => MIRConstant::Integer {
            value: *variant_index as i128,
            ty: MIRIntType::I8,
            signed: false,
        },
    }
}

fn lower_binary_op(builder: &mut MIRBuilder<'_>, op: &THIRBinOp) -> MIRBinaryOp {
    match op {
        THIRBinOp::Integer { itype, op } => MIRBinaryOp::Integer {
            ty: lower_int_type(*itype),
            signed: matches!(
                op,
                THIRIntBinOp::IMUL
                    | THIRIntBinOp::IDIV
                    | THIRIntBinOp::IMOD
                    | THIRIntBinOp::ILT
                    | THIRIntBinOp::ILE
                    | THIRIntBinOp::IGT
                    | THIRIntBinOp::IGE
                    | THIRIntBinOp::ASHR
            ),
            op: match op {
                THIRIntBinOp::ADD => MIRIntBinaryOp::Add,
                THIRIntBinOp::SUB => MIRIntBinaryOp::Sub,
                THIRIntBinOp::MUL => MIRIntBinaryOp::Mul,
                THIRIntBinOp::DIV => MIRIntBinaryOp::Div,
                THIRIntBinOp::MOD => MIRIntBinaryOp::Mod,
                THIRIntBinOp::IMUL => MIRIntBinaryOp::SignedMul,
                THIRIntBinOp::IDIV => MIRIntBinaryOp::SignedDiv,
                THIRIntBinOp::IMOD => MIRIntBinaryOp::SignedMod,
                THIRIntBinOp::EQ => MIRIntBinaryOp::Eq,
                THIRIntBinOp::NE => MIRIntBinaryOp::Ne,
                THIRIntBinOp::LT => MIRIntBinaryOp::Lt,
                THIRIntBinOp::LE => MIRIntBinaryOp::Le,
                THIRIntBinOp::GT => MIRIntBinaryOp::Gt,
                THIRIntBinOp::GE => MIRIntBinaryOp::Ge,
                THIRIntBinOp::ILT => MIRIntBinaryOp::SignedLt,
                THIRIntBinOp::ILE => MIRIntBinaryOp::SignedLe,
                THIRIntBinOp::IGT => MIRIntBinaryOp::SignedGt,
                THIRIntBinOp::IGE => MIRIntBinaryOp::SignedGe,
                THIRIntBinOp::LAND => MIRIntBinaryOp::LogicalAnd,
                THIRIntBinOp::LOR => MIRIntBinaryOp::LogicalOr,
                THIRIntBinOp::BAND => MIRIntBinaryOp::BitAnd,
                THIRIntBinOp::BOR => MIRIntBinaryOp::BitOr,
                THIRIntBinOp::BXOR => MIRIntBinaryOp::BitXor,
                THIRIntBinOp::SHL => MIRIntBinaryOp::ShiftLeft,
                THIRIntBinOp::ASHR => MIRIntBinaryOp::ArithmeticShiftRight,
                THIRIntBinOp::LSHR => MIRIntBinaryOp::LogicalShiftRight,
            },
        },
        THIRBinOp::Float { ftype, op } => MIRBinaryOp::Float {
            ty: lower_float_type(*ftype),
            op: match op {
                THIRFloatBinOp::FADD => MIRFloatBinaryOp::Add,
                THIRFloatBinOp::FSUB => MIRFloatBinaryOp::Sub,
                THIRFloatBinOp::FMUL => MIRFloatBinaryOp::Mul,
                THIRFloatBinOp::FDIV => MIRFloatBinaryOp::Div,
                THIRFloatBinOp::FEQ => MIRFloatBinaryOp::Eq,
                THIRFloatBinOp::FNE => MIRFloatBinaryOp::Ne,
                THIRFloatBinOp::FLT => MIRFloatBinaryOp::Lt,
                THIRFloatBinOp::FLE => MIRFloatBinaryOp::Le,
                THIRFloatBinOp::FGT => MIRFloatBinaryOp::Gt,
                THIRFloatBinOp::FGE => MIRFloatBinaryOp::Ge,
            },
        },
        THIRBinOp::PtrDiff { op, ptr_inner } => MIRBinaryOp::PointerOffset {
            op: match op {
                THIRPtrDiffBinOp::ADD => MIRPointerOffsetOp::Add,
                THIRPtrDiffBinOp::SUB => MIRPointerOffsetOp::Sub,
            },
            pointee: builder.lower_type(ptr_inner.as_ref()),
        },
        THIRBinOp::Pointer { op } => MIRBinaryOp::Pointer(match op {
            THIRPtrBinOp::EQ => MIRPointerBinaryOp::Eq,
            THIRPtrBinOp::NE => MIRPointerBinaryOp::Ne,
            THIRPtrBinOp::LT => MIRPointerBinaryOp::Lt,
            THIRPtrBinOp::LE => MIRPointerBinaryOp::Le,
            THIRPtrBinOp::GT => MIRPointerBinaryOp::Gt,
            THIRPtrBinOp::GE => MIRPointerBinaryOp::Ge,
        }),
    }
}

fn lower_unary_op(op: &THIRUnOp, operand_type: &THIRType) -> MIRUnaryOp {
    match op {
        THIRUnOp::NEG | THIRUnOp::INEG => {
            let (ty, signed) = integer_type(operand_type);
            MIRUnaryOp::IntegerNeg { ty, signed }
        }
        THIRUnOp::FNEG => MIRUnaryOp::FloatNeg(match operand_type.kind {
            THIRTypeKind::Float { _type } => lower_float_type(_type),
            _ => cx_mir::MIRFloatType::F64,
        }),
        THIRUnOp::BNOT => MIRUnaryOp::BitNot(integer_type(operand_type).0),
        THIRUnOp::LNOT => MIRUnaryOp::LogicalNot,
        THIRUnOp::PreIncrement(amount) => MIRUnaryOp::Increment {
            amount: *amount,
            post: false,
        },
        THIRUnOp::PostIncrement(amount) => MIRUnaryOp::Increment {
            amount: *amount,
            post: true,
        },
    }
}

fn lower_coercion(
    coercion: &THIRCoercion,
    from_type: &THIRType,
    _to_type: &THIRType,
) -> MIRCoercion {
    match coercion {
        THIRCoercion::Integral {
            sextend,
            from_type,
            to_type,
        } => MIRCoercion::Integral {
            sign_extend: *sextend,
            from: lower_int_type(*from_type),
            to: lower_int_type(*to_type),
        },
        THIRCoercion::FloatCast { to_type } => MIRCoercion::FloatCast {
            from: match from_type.kind {
                THIRTypeKind::Float { _type } => lower_float_type(_type),
                _ => cx_mir::MIRFloatType::F64,
            },
            to: lower_float_type(*to_type),
        },
        THIRCoercion::IntToFloat { to_type, sextend } => MIRCoercion::IntToFloat {
            from: integer_type(from_type).0,
            to: lower_float_type(*to_type),
            signed: *sextend,
        },
        THIRCoercion::FloatToInt { to_type, sextend } => MIRCoercion::FloatToInt {
            from: match from_type.kind {
                THIRTypeKind::Float { _type } => lower_float_type(_type),
                _ => cx_mir::MIRFloatType::F64,
            },
            to: lower_int_type(*to_type),
            signed: *sextend,
        },
        THIRCoercion::PtrToInt { to_type } => MIRCoercion::PointerToInt {
            to: lower_int_type(*to_type),
        },
        THIRCoercion::IntToPtr { sextend } => MIRCoercion::IntToPointer {
            from: integer_type(from_type).0,
            sign_extend: *sextend,
        },
        THIRCoercion::GetFnPtr => MIRCoercion::FunctionToPointer,
        THIRCoercion::Typechange => MIRCoercion::TypeChange,
        THIRCoercion::ReinterpretBits => MIRCoercion::ReinterpretBits,
    }
}

fn lower_int_type(ty: THIRIntType) -> MIRIntType {
    match ty {
        THIRIntType::I1 => MIRIntType::I1,
        THIRIntType::I8 => MIRIntType::I8,
        THIRIntType::I16 => MIRIntType::I16,
        THIRIntType::I32 => MIRIntType::I32,
        THIRIntType::I64 => MIRIntType::I64,
        THIRIntType::I128 => MIRIntType::I128,
    }
}

fn lower_float_type(ty: THIRFloatType) -> cx_mir::MIRFloatType {
    match ty {
        THIRFloatType::F32 => cx_mir::MIRFloatType::F32,
        THIRFloatType::F64 => cx_mir::MIRFloatType::F64,
    }
}
