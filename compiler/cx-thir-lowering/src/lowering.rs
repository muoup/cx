mod calls;
mod control_flow;
mod memory;
mod operators;
mod staged;

pub(crate) mod aggregates;
pub(crate) mod comptime;
pub(crate) mod globals;
pub(crate) mod types;

use cx_log::{CXResult, catalogue::mir};
use cx_mir::{
    MIRBlockTarget, MIRConstant, MIRFunctionID, MIRInstructionKind::{self, IntrinsicOp}, MIRInstruction, MIRIntIntrinsic, MIRIntType, MIRIntrinsic, MIRPtrIntrinsic, MIRTarget, MIRTypeKind, MIRValue, ty::{interface::MTRegistry, layout::calculate_type_layout},
};
use cx_thir::{
    thir::{
        comptime::THIRComptimeFn,
        data::{THIRFunction, THIRTypeKind},
        expression::{THIRCoercion, THIRExpression, THIRExpressionKind},
    },
    type_context::THIRTypeContext,
};

use crate::{
    builder::MIRBuilder, lowering::{
        memory::allocate_variable, operators::{lower_binary_op, lower_coercion, lower_unary_op}, types::{lower_int_type, lower_type},
    },
};
use crate::{
    log::{log_mir_error, mir_error},
    lowering::{
        control_flow::{auto_cleanup, auto_pop_scope, lower_control_exit},
        types::lower_float_type,
    },
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
        let declaration = builder.fun().prototype().signature().params[index].clone();
        let scope = builder.fun().current_scope_id();
        let place = builder
            .fun_mut()
            .body_mut()
            .add_parameter(&declaration, scope);

        builder.fun_mut().bind_local(
            parameter.local_id,
            MIRValue::Reference(MIRTarget::Place(place)),
        );
        if let Some(name) = &parameter.name {
            builder
                .fun_mut()
                .bind_named_value(name, MIRValue::Reference(MIRTarget::Place(place)));
        }
    }

    lower_expression(builder, body)?;

    if !builder.fun_mut().current_block_terminated() {
        if matches!(
            function.prototype.signature().return_type.kind,
            THIRTypeKind::Void
        ) {
            builder.emit(MIRInstructionKind::Return { value: None });
        } else {
            if function.require_explicit_return
                && !function.prototype.signature().return_type.is_unreachable()
                && builder.fun().current_block_reachable()
            {
                return log_mir_error(
                    &body.token_range,
                    (
                        &mir::FUNCTION_RETURN,
                        function.prototype.pretty_name().to_string(),
                    ),
                );
            }

            builder.emit(MIRInstructionKind::Unreachable);
        }
    }

    builder.finish_function()
}

pub(crate) fn lower_comptime_function(
    builder: &mut MIRBuilder<'_>,
    id: MIRFunctionID,
    function: &THIRComptimeFn,
) -> CXResult<()> {
    let Some(body) = function.body.as_ref() else {
        return Ok(());
    };

    builder.start_comptime_function(id);
    builder.fun_mut().push_scope(body.token_range.clone());

    for (index, parameter) in function.prototype.params().iter().enumerate() {
        let declaration = builder.fun().prototype().signature().params[index].clone();
        let scope = builder.fun().current_scope_id();
        let place = builder
            .fun_mut()
            .body_mut()
            .add_parameter(&declaration, scope);
        let value = MIRValue::Reference(MIRTarget::Place(place));
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
        builder.emit(MIRInstructionKind::Return { value });
    }

    auto_pop_scope(builder)?;
    builder.finish_function()
}

pub(crate) fn lower_expression(
    builder: &mut MIRBuilder<'_>,
    expr: &THIRExpression,
) -> CXResult<MIRValue> {
    let previous_range = builder.set_source_range(expr.token_range.clone());
    let result = (|| -> CXResult<MIRValue> {
        let value = match &expr.kind {
            THIRExpressionKind::BoolLiteral(value) => MIRValue::Constant(MIRConstant::Integer {
                value: *value as i128,
                ty: MIRIntType::I1,
            }),

            THIRExpressionKind::IntLiteral(value) => {
                let ty = match &expr._type.kind {
                    THIRTypeKind::Integer { _type, .. } => lower_int_type(*_type),
                    _ => unreachable!("IntLiteral expression has non-integer type"),
                };

                MIRValue::Constant(MIRConstant::Integer {
                    value: *value as i128,
                    ty,
                })
            }

            THIRExpressionKind::FloatLiteral(value) => {
                let ty = match expr._type.kind {
                    THIRTypeKind::Float { _type } => lower_float_type(_type),
                    _ => unreachable!("FloatLiteral expression has non-float type"),
                };

                MIRValue::Constant(MIRConstant::Float { value: *value, ty })
            }

            THIRExpressionKind::StringLiteral { value } => {
                if builder.types().find_kind(&MIRTypeKind::Str).is_none() {
                    builder
                        .types_mut()
                        .intern(cx_mir::MIRType::new(MIRTypeKind::Str, None));
                }

                MIRValue::Constant(MIRConstant::String(value.clone()))
            }

            THIRExpressionKind::Unit => MIRValue::Constant(MIRConstant::Unit),

            THIRExpressionKind::SizeOf { _type } | THIRExpressionKind::AlignOf { _type } => {
                let type_id = lower_type(builder, _type)?;
                let layout = calculate_type_layout(builder.registry(), type_id)?;

                MIRValue::Constant(MIRConstant::Integer {
                    value: match expr.kind {
                        THIRExpressionKind::SizeOf { .. } => layout.size as i128,
                        THIRExpressionKind::AlignOf { .. } => layout.align as i128,

                        _ => unreachable!(),
                    },
                    ty: MIRIntType::I64,
                })
            }

            THIRExpressionKind::Variable { local_id, .. } => {
                builder.local_value(*local_id)?.ok_or_else(|| {
                    mir_error(
                        &expr.token_range,
                        (
                            &mir::MISSING_ENTITY,
                            (
                                format!("local {:?}", local_id),
                                "MIR lowering context".into(),
                            ),
                        ),
                    )
                })?
            }

            THIRExpressionKind::GlobalVariable { symbol } => {
                MIRValue::Reference(MIRTarget::Global(
                    builder
                        .module_mut()
                        .global_symbol(symbol.as_str())
                        .ok_or_else(|| {
                            mir_error(
                                &expr.token_range,
                                (
                                    &mir::MISSING_ENTITY,
                                    (format!("global '{symbol}'"), "MIR lowering context".into()),
                                ),
                            )
                        })?,
                ))
            }

            THIRExpressionKind::ContractVariable { name, .. } => builder
                .fun()
                .named(name)
                .unwrap_or(MIRValue::Constant(MIRConstant::Undefined)),

            THIRExpressionKind::FunctionReference { name, .. } => builder
                .module_mut()
                .function_symbol(name.as_str())
                .ok_or_else(|| {
                    mir_error(
                        &expr.token_range,
                        (
                            &mir::MISSING_ENTITY,
                            (format!("function '{name}'"), "MIR lowering context".into()),
                        ),
                    )
                })
                .map(|v| MIRValue::Constant(MIRConstant::Function(v)))?,

            THIRExpressionKind::BinaryOperation { lhs, rhs, op } => {
                lower_binary_op(builder, expr, lhs, rhs, op)?
            }

            THIRExpressionKind::UnaryOperation { operand, op } => {
                lower_unary_op(builder, expr, operand, op)?
            }

            THIRExpressionKind::Copy { source } => {
                let lowered = lower_expression(builder, source)?;

                match lowered {
                    MIRValue::PlaceRef(target) => {
                        let out = builder
                            .fun_mut()
                            .new_register(lower_type(builder, &source._type)?, None);

                        builder.fun_mut().emit(MIRInstruction {
                            kind: MIRInstructionKind::LiftPlace { out, place: target },
                            token_range: expr.token_range.clone(),
                        });

                        MIRValue::Register(out)
                    }

                    _ => lowered,
                }
            }

            THIRExpressionKind::Move { local_id, .. } => {
                let out = builder
                    .fun_mut()
                    .new_register(lower_type(builder, &expr._type)?, None);
                let local = builder.fun().local(*local_id).expect("local should exist");

                match local {
                    MIRValue::PlaceRef(target) => {
                        let out = builder
                            .fun_mut()
                            .new_register(lower_type(builder, &expr._type)?, None);

                        builder.fun_mut().emit(MIRInstruction {
                            kind: MIRInstructionKind::LiftPlace { out, place: target },
                            token_range: expr.token_range.clone(),
                        });

                        builder.fun_mut().emit(MIRInstruction {
                            kind: MIRInstructionKind::Invalidate {
                                place: MIRValue::Place(target),
                                leak: false,
                            },
                            token_range: expr.token_range.clone(),
                        });

                        MIRValue::Register(out)
                    }

                    _ => local,
                }
            }

            THIRExpressionKind::CreateLocalVariable {
                name,
                local_id,
                _type,
                initial_value,
            } => {
                let initial_value = initial_value
                    .as_deref()
                    .map(|value| lower_expression(builder, value))
                    .transpose()?;

                let type_id = lower_type(builder, _type)?;
                let place = builder.new_place(type_id, Some(name.clone()), _type.is_nodrop());

                MIRValue::PlaceRef(allocate_variable(
                    builder,
                    Some(name.clone()),
                    _type,
                    initial_value,
                )?)
            }

            THIRExpressionKind::AdoptRegion {
                binding_name,
                local_id,
                _type,
                initial_value,
            } => {
                let initial_value = lower_expression(builder, initial_value)?;

                match initial_value {
                    MIRValue::Reference(target) => {
                        builder
                            .fun_mut()
                            .bind_local(*local_id, MIRValue::Reference(target));
                        builder
                            .fun_mut()
                            .bind_named_value(name, MIRValue::Reference(target));
                        MIRValue::Reference(target)
                    }
                    value => {
                        let place = memory::assign_operand_to_place(
                            builder,
                            value,
                            _type,
                            Some(name.clone()),
                        )?;
                        builder.fun_mut().bind_local(
                            *local_id,
                            MIRValue::Reference(cx_mir::MIRTarget::Place(place)),
                        );
                        builder.fun_mut().bind_named_value(
                            name,
                            MIRValue::Reference(cx_mir::MIRTarget::Place(place)),
                        );
                        MIRValue::Reference(cx_mir::MIRTarget::Place(place))
                    }
                }
            }

            THIRExpressionKind::Assign { target, value } => {
                let assignment_type = lower_type(builder, &value._type)?;

                let mtarget = lower_expression(builder, target)?;
                let mvalue = lower_expression(builder, value)?;

                let ptarget = memory::ensure_place(builder, mtarget, &target._type)?;

                builder.set_source_range(target.token_range.clone());
                builder.emit(MIRInstructionKind::Store {
                    target: ptarget,
                    value: mvalue,
                    ty: assignment_type,
                });

                MIRValue::Reference(ptarget)
            }

            THIRExpressionKind::Typechange(inner) => lower_expression(builder, inner)?,

            THIRExpressionKind::MemberAccess {
                base,
                member_index,
                aggregate_type,
            } => {
                let base_value = lower_expression(builder, base)?;
                let base = memory::ensure_place(builder, base_value, &base._type)?;
                let type_id = lower_type(builder, &expr._type)?;
                let out = if expr._type.is_memory_reference() {
                    builder.fun_mut().new_register(type_id, None)
                } else {
                    memory::target_register(builder, type_id)
                };
                let aggregate_type_id = lower_type(builder, aggregate_type)?;
                builder.emit(MIRInstructionKind::AggregateOp(MIRAggregateOp::Target {
                    out,
                    op: MIRTargetAggregateOp::Field {
                        base,
                        field: *member_index,
                        aggregate_type: aggregate_type_id,
                    },
                }));
                MIRValue::Reference(cx_mir::MIRTarget::Register(out))
            }

            THIRExpressionKind::ArrayAccess {
                array,
                index,
                element_type,
            } => {
                let int_type = lower_type(builder, &index._type)?;
                let element_type_id = lower_type(builder, element_type)?;
                let return_type = lower_type(builder, &expr._type)?;

                let array = lower_expression(builder, array)?;
                let index = lower_expression(builder, index)?;

                let elem_size = todo!();

                let index_calc = builder.fun_mut().new_register(int_type, None);
                builder.emit(
                    MIRInstruction {
                        kind: MIRInstructionKind::IntrinsicOp(
                            MIRIntrinsic::Int(
                                MIRIntIntrinsic::UMul {
                                    out: MIRTarget::Register(index_calc),
                                    lhs: index,
                                    rhs: elem_size,
                                }
                            )
                        ),
                        token_range: expr.token_range.clone(),
                    }
                )?;

                let out = builder.fun_mut().new_register(return_type, None);
                builder.emit(MIRInstruction {
                    kind: MIRInstructionKind::IntrinsicOp(
                        MIRIntrinsic::Pointer(
                            MIRPtrIntrinsic::Add {
                                out: MIRTarget::Register(out),
                                ptr: array,
                                offset: MIRValue::Register(index_calc),
                            }
                        )
                    ),
                    token_range: expr.token_range.clone(),
                });

                MIRValue::Reference(cx_mir::MIRTarget::Register(out))
            }

            THIRExpressionKind::PatternIs { lhs, pattern } => {
                aggregates::lower_pattern_test(builder, lhs, pattern, &expr._type)?
            }

            THIRExpressionKind::Unpack {
                value, bindings, ..
            } => {
                let lowered_value = lower_expression(builder, value)?;

                let target = memory::ensure_place(builder, lowered_value, &value._type)?;
                let struct_type_id = lower_type(builder, &value._type)?;
                let base = builder.new_place(struct_type_id, None, false);

                let value = memory::move_value(
                    builder,
                    MIRValue::Reference(target),
                    struct_type_id,
                    &expr.token_range,
                )?;
                builder.emit(MIRInstructionKind::Store {
                    target: MIRTarget::Place(base),
                    value,
                    ty: struct_type_id,
                });

                for binding in bindings {
                    let field_type = lower_type(builder, &binding.field_type)?;
                    let field_place = memory::target_register(builder, field_type);

                    builder.emit(MIRInstructionKind::AggregateOp(MIRAggregateOp::Target {
                        out: field_place,
                        op: MIRTargetAggregateOp::Field {
                            base: MIRTarget::Place(base),
                            field: binding.field_index,
                            aggregate_type: struct_type_id,
                        },
                    }));
                    builder.fun_mut().bind_local(
                        binding.binding_local_id,
                        MIRValue::Reference(MIRTarget::Register(field_place)),
                    );
                }

                MIRValue::Constant(MIRConstant::Unit)
            }

            THIRExpressionKind::TaggedUnionTag { value, sum_type } => {
                let base = lower_expression(builder, value)?;
                let type_id = lower_type(builder, &expr._type)?;
                let out = builder.fun_mut().new_register(type_id, None);
                let sum_type_id = lower_type(builder, sum_type)?;
                builder.emit(MIRInstructionKind::AggregateOp(MIRAggregateOp::Value {
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
                let base_value = if value._type.is_memory_reference() {
                    MIRValue::Reference(memory::ensure_place(builder, base_value, &value._type)?)
                } else {
                    base_value
                };
                let sum_type = match &value._type.kind {
                    THIRTypeKind::MemoryReference { inner_type, .. } => {
                        builder.registry().resolve_type_id(*inner_type).clone()
                    }
                    _ => value._type.clone(),
                };
                let sum_type_id = lower_type(builder, &sum_type)?;
                let variant_type_id = lower_type(builder, variant_type)?;
                match base_value {
                    MIRValue::Reference(base) => {
                        let out = memory::target_register(builder, variant_type_id);
                        builder.emit(MIRInstructionKind::AggregateOp(MIRAggregateOp::Target {
                            out,
                            op: MIRTargetAggregateOp::Variant {
                                base,
                                variant: *variant_index,
                                sum_type: sum_type_id,
                            },
                        }));
                        MIRValue::Reference(MIRTarget::Register(out))
                    }
                    value => {
                        let out = builder.fun_mut().new_register(variant_type_id, None);
                        builder.emit(MIRInstructionKind::AggregateOp(MIRAggregateOp::Value {
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
                builder.emit(MIRInstructionKind::AggregateOp(MIRAggregateOp::Value {
                    out: constructed,
                    op: MIRValueAggregateOp::Variant {
                        variant: *variant_index,
                        value,
                        sum_type: sum_type_id,
                    },
                }));
                builder.emit(MIRInstructionKind::Store {
                    target,
                    value: MIRValue::Register(constructed),
                    ty: sum_type_id,
                });
                MIRValue::Reference(target)
            }
            THIRExpressionKind::TaggedUnionInitializer {
                variant_index,
                value,
                sum_type,
            } => {
                let value = lower_expression(builder, value)?;
                let sum_type_id = lower_type(builder, sum_type)?;
                let type_id = lower_type(builder, &expr._type)?;
                let out = builder.fun_mut().new_register(type_id, None);
                builder.emit(MIRInstructionKind::AggregateOp(MIRAggregateOp::Value {
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
                let type_id = lower_type(builder, &expr._type)?;
                if let Ok(MIRTypeKind::Array { length, .. }) = builder.types().kind(type_id)
                    && fields.len() > *length
                {
                    return log_mir_error(
                        &expr.token_range,
                        (&mir::ARRAY_TOO_LONG, (fields.len(), *length)),
                    );
                }
                let out = builder.fun_mut().new_register(type_id, None);
                builder.emit(MIRInstructionKind::AggregateOp(MIRAggregateOp::Value {
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
                let type_id = lower_type(builder, &expr._type)?;
                let out = builder.fun_mut().new_register(type_id, None);
                let aggregate_type_id = lower_type(builder, struct_type)?;
                builder.emit(MIRInstructionKind::AggregateOp(MIRAggregateOp::Value {
                    out,
                    op: MIRValueAggregateOp::Construct {
                        ty: aggregate_type_id,
                        fields,
                    },
                }));
                MIRValue::Register(out)
            }

            THIRExpressionKind::Break => lower_control_exit(builder, MIRStagedExitKind::Break)?,
            THIRExpressionKind::Continue => {
                lower_control_exit(builder, MIRStagedExitKind::Continue)?
            }
            THIRExpressionKind::Goto { name } => {
                let target = if let Some(target) = builder.fun_mut().label(name) {
                    target
                } else {
                    let target = builder.fun_mut().new_block(name.clone());
                    builder.fun_mut().declare_label(name, target);
                    target
                };
                builder.emit(MIRInstructionKind::Jump {
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
                builder.emit(MIRInstructionKind::Jump {
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
                &expr._type,
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
            } => control_flow::lower_match(builder, condition, *subject, arms, &expr._type)?,
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
                    (MIRValue::Reference(target), Some(expression))
                        if !expression._type.is_memory_reference() =>
                    {
                        let ty = lower_type(builder, &expression._type)?;
                        memory::copy(builder, target, ty)
                    }
                    (value, _) => value,
                };
                let value = value_expression.map(|_| lowered_value);
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
                builder.emit(MIRInstructionKind::Return { value });
                MIRValue::Constant(MIRConstant::Unit)
            }

            THIRExpressionKind::Unreachable => {
                builder.emit(MIRInstructionKind::Unreachable);
                MIRValue::Constant(MIRConstant::Unit)
            }

            THIRExpressionKind::Yield { value } => {
                let yield_type = value
                    .as_deref()
                    .map(|value| lower_type(builder, &value._type))
                    .transpose()?;
                let value = value
                    .as_deref()
                    .map(|value| {
                        let lowered = lower_expression(builder, value)?;
                        Ok(lowered)
                    })
                    .transpose()?;

                let target = builder
                    .fun()
                    .scope_stack()
                    .iter()
                    .rev()
                    .find_map(|scope| scope.yield_target.map(|block| (scope.id(), block)));
                if target.is_none() && builder.is_capturing() {
                    let root_scope = builder
                        .fun()
                        .scope_stack()
                        .first()
                        .expect("captured function has no root scope")
                        .id();
                    auto_cleanup(builder, root_scope)?;
                    builder.emit(cx_mir::MIRStagedInstrKind::Yield {
                        value,
                        ty: yield_type,
                    });
                    return Ok(MIRValue::Constant(MIRConstant::Unit));
                }

                let Some((scope_id, block_id)) = target else {
                    return log_mir_error(
                        &expr.token_range,
                        (
                            &mir::REQUIRED_CONTEXT,
                            ("yield".into(), "a yieldable scope".into()),
                        ),
                    );
                };

                let args = value.into_iter().collect();
                auto_cleanup(builder, scope_id)?;
                builder.emit(MIRInstructionKind::Jump {
                    target: MIRBlockTarget::with_args(block_id, args),
                });
                MIRValue::Constant(MIRConstant::Unit)
            }

            THIRExpressionKind::Assert { condition, message } => {
                let condition = lower_expression(builder, condition)?;
                builder.emit(MIRInstructionKind::Assert {
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
                yields,
            } => {
                let result_type = lower_type(builder, &expr._type)?;
                let returns_value =
                    !matches!(builder.types().kind(result_type), Ok(MIRTypeKind::Void));
                let merge = yields.then(|| builder.fun_mut().new_block("block.yield"));
                let yield_register = merge.and_then(|merge| {
                    returns_value.then(|| builder.fun_mut().set_yield_recipient(merge, result_type))
                });
                let mut result = MIRValue::Constant(MIRConstant::Unit);
                builder.fun_mut().push_invisible_scope();
                if let Some(merge) = merge {
                    builder
                        .fun_mut()
                        .current_scope_mut()
                        .set_yield_target(merge);
                }
                if *creates_scope {
                    builder.fun_mut().push_scope(expr.token_range.clone());
                }

                builder.fun_mut().push_invisible_scope();

                for statement in statements {
                    result = lower_expression(builder, statement)?;
                }

                control_flow::auto_pop_scope(builder)?;
                if *creates_scope {
                    control_flow::auto_pop_scope(builder)?;
                }
                control_flow::auto_pop_scope(builder)?;

                if let Some(merge) = merge {
                    if !builder.fun().current_block_terminated() {
                        builder.emit(MIRInstructionKind::Unreachable);
                    }
                    builder.fun_mut().set_current_block(merge);
                    yield_register
                        .map(MIRValue::Register)
                        .unwrap_or(MIRValue::Constant(MIRConstant::Unit))
                } else {
                    result
                }
            }

            THIRExpressionKind::CallFunction {
                function,
                arguments,
                contract,
            } => calls::lower_call(builder, function, arguments, contract, &expr._type)?,

            THIRExpressionKind::VaStart { list, last } => {
                let list = lower_expression(builder, list)?;
                let last = lower_expression(builder, last)?;
                builder.emit(MIRInstructionKind::Intrinsic(cx_mir::MIRIntrinsic::VaStart {
                    list,
                    last,
                }));
                MIRValue::Constant(MIRConstant::Unit)
            }

            THIRExpressionKind::VaEnd { list } => {
                let list = lower_expression(builder, list)?;
                builder.emit(MIRInstructionKind::Intrinsic(cx_mir::MIRIntrinsic::VaEnd {
                    list,
                }));
                MIRValue::Constant(MIRConstant::Unit)
            }

            THIRExpressionKind::VaArg { list, _type } => {
                let list = lower_expression(builder, list)?;
                let ty = lower_type(builder, _type)?;
                let out = builder.fun_mut().new_register(ty, None);
                builder.emit(MIRInstructionKind::Intrinsic(cx_mir::MIRIntrinsic::VaArg {
                    out,
                    list,
                    ty,
                }));
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
                        &expr._type.kind,
                        THIRTypeKind::MemoryReference { .. } | THIRTypeKind::PointerTo { .. }
                    )
                {
                    let value = lower_expression(builder, operand)?;
                    let type_id = lower_type(builder, &expr._type)?;
                    let is_str_reference = builder.registry().is_cx_str(&expr._type);
                    return Ok(match value {
                        MIRValue::Reference(cx_mir::MIRTarget::Place(place))
                            if !is_str_reference =>
                        {
                            let out = builder.fun_mut().new_register(type_id, None);
                            builder.emit(MIRInstructionKind::Let {
                                out,
                                value: MIRValue::Reference(MIRTarget::Place(place)),
                            });
                            MIRValue::Register(out)
                        }
                        value => {
                            lower_coercion(builder, value, conversion, &operand._type, &expr._type)?
                        }
                    });
                }

                let value = lower_expression(builder, operand)?;
                lower_coercion(builder, value, conversion, &operand._type, &expr._type)?
            }

            THIRExpressionKind::Leak { expression: inner } => {
                let value = lower_expression(builder, inner)?;
                if let MIRValue::Reference(cx_mir::MIRTarget::Place(place)) = value {
                    builder.emit(MIRInstructionKind::Invalidate { place, leak: true });
                    MIRValue::Reference(cx_mir::MIRTarget::Place(place))
                } else {
                    value
                }
            }
            THIRExpressionKind::Unsafe { expression: inner } => lower_expression(builder, inner)?,
            THIRExpressionKind::StagedExpression(staged) => {
                let params = staged
                    .params()
                    .iter()
                    .map(|parameter| (parameter.local_id, &parameter.ty))
                    .collect::<Vec<_>>();
                let (template, captures) = builder.capture_staged(staged.expr(), &params, None)?;
                let out = builder.fun_mut().new_register(template.result_type(), None);
                builder.emit(cx_mir::MIRComptimeOp::MakeStaged {
                    out,
                    template,
                    captures,
                });
                MIRValue::Register(out)
            }
            THIRExpressionKind::Materialize { expr, with_params } => {
                let staged = lower_expression(builder, expr)?;
                let mut args = Vec::with_capacity(with_params.len());
                for param in with_params {
                    args.push(lower_expression(builder, param)?);
                }
                let out = if expr._type.is_void() || expr._type.is_unreachable() {
                    None
                } else {
                    let ty = lower_type(builder, &expr._type)?;
                    Some(builder.fun_mut().new_register(ty, None))
                };
                let targets = crate::lowering::staged::exits::targets(builder)?;
                builder.emit(cx_mir::MIRComptimeOp::ApplyStaged {
                    out,
                    staged,
                    args,
                    targets,
                });
                out.map(MIRValue::Register)
                    .unwrap_or(MIRValue::Constant(MIRConstant::Unit))
            }
        };

        Ok(value)
    })();
    builder.restore_source_range(previous_range);
    result
}
