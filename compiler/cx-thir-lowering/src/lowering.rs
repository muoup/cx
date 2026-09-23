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
    MIRAggregateIntrinsic, MIRBindable, MIRBlockTarget, MIRConstant, MIRFunctionID, MIRInstruction,
    MIRInstructionKind, MIRIntType, MIRInternalIntrinsic, MIRTarget, MIRTypeKind, MIRVAIntrinsic,
    MIRValue,
    expr::instruction::MIRInvalidationKind,
    ty::{interface::MTRegistry, layout::calculate_type_layout},
};
use cx_thir::{
    thir::{
        data::{THIRFunction, THIRFunctionBody, THIRTypeKind},
        expression::{THIRBlockKind, THIRCoercion, THIRExpression, THIRExpressionKind},
    },
    type_context::THIRTypeContext,
};
use cx_tokens::TokenRange;

use crate::{
    builder::MIRBuilder,
    lowering::{
        memory::{allocate_variable, move_value},
        operators::{lower_binary_op, lower_coercion, lower_unary_op},
        types::{lower_int_type, lower_type},
    },
};
use crate::{
    log::{log_mir_error, mir_error},
    lowering::{
        control_flow::{auto_cleanup, auto_cleanup_before},
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
        let declaration = builder.fun().prototype().signature.params()[index].clone();
        let scope = builder.fun().current_scope_id();
        let place = builder
            .fun_mut()
            .body_mut()
            .add_parameter(&declaration, scope);

        builder.emit(MIRInstruction::new(
            MIRInstructionKind::Initialize {
                place: MIRBindable::Place(place),
            },
            TokenRange::internal(),
        ));

        builder
            .fun_mut()
            .bind_local(parameter.local_id, MIRValue::PlaceRef(place));
        if let Some(name) = &parameter.name {
            builder
                .fun_mut()
                .bind_named_value(name, MIRValue::PlaceRef(place));
        }
    }

    lower_function_block(builder, function, body)?;

    builder.finish_function()
}

pub(crate) fn lower_function_block(
    builder: &mut MIRBuilder<'_>,
    function: &THIRFunction,
    block: &THIRFunctionBody,
) -> CXResult<()> {
    let prototype = &function.prototype;
    match block {
        THIRFunctionBody::Expression(expr) => {
            let result = lower_expression(builder, expr)?;
            let result = if expr._type.is_void() {
                None
            } else {
                Some(match result {
                    MIRValue::PlaceRef(place) if !expr._type.is_memory_reference() => {
                        let ty = lower_type(builder, &expr._type)?;
                        memory::copy(builder, place, ty, &expr.token_range)
                    }
                    value => value,
                })
            };
            emit_implicit_return(builder, result, expr.token_range.clone())?;
        }
        THIRFunctionBody::Block { exprs, token_range } => {
            for statement in exprs {
                lower_expression(builder, statement)?;
            }

            if prototype.signature().return_type.is_void() {
                emit_implicit_return(builder, None, token_range.clone())?;
            } else if prototype.symbol_name() == "main" {
                emit_implicit_return(
                    builder,
                    Some(MIRValue::Constant(MIRConstant::Integer {
                        value: 0,
                        ty: MIRIntType::I32,
                    })),
                    token_range.clone(),
                )?;
            } else {
                if !builder.fun().current_block_terminated() {
                    if !prototype.signature().return_type.is_unreachable()
                        && function.reject_nonvoid_fallthrough
                        && builder.fun().body().current_block_reachable()
                    {
                        return log_mir_error(
                            token_range,
                            (&mir::FUNCTION_RETURN, prototype.symbol_name().to_owned()),
                        );
                    }

                    builder.emit(MIRInstruction::new(
                        MIRInstructionKind::Unreachable,
                        token_range.clone(),
                    ));
                }
            }
        }
    }

    Ok(())
}

pub(super) fn emit_implicit_return(
    builder: &mut MIRBuilder<'_>,
    value: Option<MIRValue>,
    range: TokenRange,
) -> CXResult<()> {
    if builder.fun().current_block_terminated() {
        return Ok(());
    }

    let root_scope = builder
        .fun()
        .scope_stack()
        .first()
        .expect("active function has no root scope")
        .id();
    auto_cleanup(builder, root_scope, range.clone())?;
    builder.emit_if_open(MIRInstruction::new(
        MIRInstructionKind::Return { value },
        range,
    ));
    Ok(())
}

pub(crate) fn lower_expression(
    builder: &mut MIRBuilder<'_>,
    expr: &THIRExpression,
) -> CXResult<MIRValue> {
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
            let layout = calculate_type_layout(builder.types(), type_id);

            MIRValue::Constant(MIRConstant::Integer {
                value: match expr.kind {
                    THIRExpressionKind::SizeOf { .. } => layout.size() as i128,
                    THIRExpressionKind::AlignOf { .. } => layout.alignment() as i128,

                    _ => unreachable!(),
                },
                ty: MIRIntType::I64,
            })
        }

        THIRExpressionKind::Variable { local_id, .. } => {
            builder.local_value(*local_id).ok_or_else(|| {
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

        THIRExpressionKind::GlobalVariable { symbol } => MIRValue::Global(
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
        ),

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
                    let value_type = match &source._type.kind {
                        THIRTypeKind::MemoryReference { inner_type, .. } => {
                            types::lower_type_id(builder, *inner_type)?
                        }
                        _ => lower_type(builder, &source._type)?,
                    };
                    let out = builder.fun_mut().new_register(value_type, None);

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
            let local = builder.fun().local(*local_id).expect("local should exist");
            let ty = lower_type(builder, &expr._type)?;

            move_value(builder, local, ty, &expr.token_range)?
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

            let place = allocate_variable(
                builder,
                Some(name.clone()),
                _type,
                initial_value,
                &expr.token_range,
            )?;
            builder
                .fun_mut()
                .bind_local(*local_id, MIRValue::PlaceRef(place));
            builder
                .fun_mut()
                .bind_named_value(name, MIRValue::PlaceRef(place));
            MIRValue::PlaceRef(place)
        }

        THIRExpressionKind::AdoptRegion {
            binding_name,
            local_id,
            _type,
            initial_value,
        } => {
            let initial_value = lower_expression(builder, initial_value)?;
            let place = memory::assign_operand_to_place(
                builder,
                initial_value,
                _type,
                Some(binding_name.clone()),
                &expr.token_range,
            )?;
            let value = MIRValue::PlaceRef(place);
            builder.fun_mut().bind_local(*local_id, value.clone());
            builder
                .fun_mut()
                .bind_named_value(binding_name, value.clone());
            value
        }

        THIRExpressionKind::Assign { target, value } => {
            let assignment_type = lower_type(builder, &value._type)?;

            let mtarget = lower_expression(builder, target)?;
            let mvalue = lower_expression(builder, value)?;

            let ptarget = memory::ensure_place(builder, mtarget, &target._type)?;

            builder.emit(MIRInstruction::new(
                MIRInstructionKind::Invalidate {
                    place: MIRBindable::Place(ptarget),
                    kind: MIRInvalidationKind::Drop,
                },
                target.token_range.clone(),
            ));

            builder.emit(MIRInstruction::new(
                MIRInstructionKind::Initialize {
                    place: MIRBindable::Place(ptarget),
                },
                target.token_range.clone(),
            ));

            builder.emit(MIRInstruction::new(
                MIRInstructionKind::Store {
                    target: ptarget,
                    value: mvalue,
                    ty: assignment_type,
                },
                target.token_range.clone(),
            ));

            MIRValue::PlaceRef(ptarget)
        }

        THIRExpressionKind::Typechange(inner) => lower_expression(builder, inner)?,

        THIRExpressionKind::MemberAccess {
            base,
            member_index,
            aggregate_type,
        } => {
            let base_value = lower_expression(builder, base)?;
            let type_id = lower_type(builder, &expr._type)?;
            let aggregate_type_id = lower_type(builder, aggregate_type)?;
            let out = builder.fun_mut().new_register(type_id, None);
            builder.fun_mut().emit_intrinsic(
                MIRAggregateIntrinsic::StructField {
                    out: MIRTarget::Register(out),
                    base: base_value,
                    field: *member_index,
                    struct_ty: aggregate_type_id,
                },
                expr.token_range.clone(),
            );
            MIRValue::Register(out)
        }

        THIRExpressionKind::ArrayAccess {
            array,
            index,
            element_type,
        } => {
            let element_type_id = lower_type(builder, element_type)?;
            let return_type = lower_type(builder, &expr._type)?;

            let array = lower_expression(builder, array)?;
            let index = lower_expression(builder, index)?;

            let out = builder.fun_mut().new_register(return_type, None);
            builder.fun_mut().emit_intrinsic(
                MIRAggregateIntrinsic::ArrayIndex {
                    out: MIRTarget::Register(out),
                    base: array,
                    index,
                    element_ty: element_type_id,
                },
                expr.token_range.clone(),
            );

            MIRValue::Register(out)
        }

        THIRExpressionKind::PatternIs { lhs, pattern } => {
            aggregates::lower_pattern_test(builder, lhs, pattern, &expr._type)?
        }

        THIRExpressionKind::Unpack {
            value, bindings, ..
        } => {
            let lowered_value = lower_expression(builder, value)?;

            let source_type = match &value._type.kind {
                THIRTypeKind::MemoryReference { inner_type, .. } => {
                    builder.registry().resolve_type_id(*inner_type).clone()
                }
                _ => value._type.clone(),
            };
            let struct_type_id = lower_type(builder, &source_type)?;
            let moved =
                memory::move_value(builder, lowered_value, struct_type_id, &expr.token_range)?;
            let base =
                allocate_variable(builder, None, &source_type, Some(moved), &expr.token_range)?;

            for binding in bindings {
                let field_type = lower_type(builder, &binding.field_type)?;
                let aggregate_type = lower_type(builder, &source_type)?;
                let field_register = builder.fun_mut().new_register(field_type, None);
                builder.fun_mut().emit_intrinsic(
                    MIRAggregateIntrinsic::StructField {
                        out: MIRTarget::Register(field_register),
                        base: MIRValue::PlaceRef(base),
                        field: binding.field_index,
                        struct_ty: aggregate_type,
                    },
                    expr.token_range.clone(),
                );
                builder
                    .fun_mut()
                    .bind_local(binding.binding_local_id, MIRValue::Register(field_register));
            }

            MIRValue::Constant(MIRConstant::Unit)
        }

        THIRExpressionKind::TaggedUnionTag { value, sum_type } => {
            let base = lower_expression(builder, value)?;
            let type_id = lower_type(builder, &expr._type)?;
            let out = builder.fun_mut().new_register(type_id, None);
            let sum_type_id = lower_type(builder, sum_type)?;
            builder.fun_mut().emit_intrinsic(
                MIRAggregateIntrinsic::SumIndex {
                    out: MIRTarget::Register(out),
                    value: base,
                    sum_ty: sum_type_id,
                },
                expr.token_range.clone(),
            );
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
            let out = builder.fun_mut().new_register(variant_type_id, None);
            builder.fun_mut().emit_intrinsic(
                MIRAggregateIntrinsic::SumVariantL {
                    out: MIRTarget::Register(out),
                    base: base_value,
                    variant: *variant_index,
                    sum_ty: sum_type_id,
                },
                expr.token_range.clone(),
            );
            MIRValue::Register(out)
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
            builder.fun_mut().emit_intrinsic(
                MIRAggregateIntrinsic::SumVariantL {
                    out: MIRTarget::Register(constructed),
                    base: value,
                    variant: *variant_index,
                    sum_ty: sum_type_id,
                },
                expr.token_range.clone(),
            );
            builder.emit(MIRInstruction::new(
                MIRInstructionKind::Store {
                    target,
                    value: MIRValue::Register(constructed),
                    ty: sum_type_id,
                },
                expr.token_range.clone(),
            ));
            MIRValue::PlaceRef(target)
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
            builder.fun_mut().emit_intrinsic(
                MIRAggregateIntrinsic::SumVariantL {
                    out: MIRTarget::Register(out),
                    base: value,
                    variant: *variant_index,
                    sum_ty: sum_type_id,
                },
                expr.token_range.clone(),
            );
            MIRValue::Register(out)
        }

        THIRExpressionKind::ArrayInitializer { elements, .. } => {
            let mut fields = Vec::with_capacity(elements.len());
            for (index, element) in elements.iter().enumerate() {
                fields.push((index, lower_expression(builder, element)?));
            }
            let type_id = lower_type(builder, &expr._type)?;
            if let Some(MIRTypeKind::Array { length, .. }) =
                builder.types().definition(type_id).map(|ty| ty.kind())
                && fields.len() > *length
            {
                return log_mir_error(
                    &expr.token_range,
                    (&mir::ARRAY_TOO_LONG, (fields.len(), *length)),
                );
            }
            let out = builder.fun_mut().new_register(type_id, None);
            builder.fun_mut().emit_intrinsic(
                MIRAggregateIntrinsic::StructInit {
                    out: MIRTarget::Register(out),
                    ty: type_id,
                    fields,
                },
                expr.token_range.clone(),
            );
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
            builder.fun_mut().emit_intrinsic(
                MIRAggregateIntrinsic::StructInit {
                    out: MIRTarget::Register(out),
                    ty: aggregate_type_id,
                    fields,
                },
                expr.token_range.clone(),
            );
            MIRValue::Register(out)
        }

        THIRExpressionKind::Break => {
            let target = builder
                .fun()
                .control_stack()
                .iter()
                .rev()
                .find_map(|scope| {
                    scope
                        .break_target()
                        .map(|block| (scope.cleanup_boundary(), block))
                });

            let Some((scope, target)) = target else {
                unreachable!("break statement outside of loop or switch")
            };

            auto_cleanup_before(builder, scope, expr.token_range.clone())?;
            builder.emit(MIRInstruction::new(
                MIRInstructionKind::Jump {
                    target: MIRBlockTarget::new(target),
                },
                expr.token_range.clone(),
            ));

            MIRValue::Constant(MIRConstant::Unit)
        }

        THIRExpressionKind::Continue => {
            let target = builder
                .fun()
                .control_stack()
                .iter()
                .rev()
                .find_map(|scope| {
                    scope
                        .continue_target()
                        .map(|block| (scope.cleanup_boundary(), block))
                });

            let Some((scope, target)) = target else {
                unreachable!("continue statement outside of loop")
            };

            auto_cleanup_before(builder, scope, expr.token_range.clone())?;
            builder.emit(MIRInstruction::new(
                MIRInstructionKind::Jump {
                    target: MIRBlockTarget::new(target),
                },
                expr.token_range.clone(),
            ));

            MIRValue::Constant(MIRConstant::Unit)
        }

        THIRExpressionKind::Goto { name } => {
            let target = if let Some(target) = builder.fun_mut().label(name) {
                target
            } else {
                let target = builder.fun_mut().new_block(name.clone());
                builder.fun_mut().declare_label(name, target);
                target
            };
            builder.emit(MIRInstruction::new(
                MIRInstructionKind::Jump {
                    target: MIRBlockTarget::new(target),
                },
                expr.token_range.clone(),
            ));
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
            builder.emit(MIRInstruction::new(
                MIRInstructionKind::Jump {
                    target: MIRBlockTarget::new(target),
                },
                expr.token_range.clone(),
            ));
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
                (MIRValue::PlaceRef(target), Some(expression))
                    if !expression._type.is_memory_reference() =>
                {
                    let ty = lower_type(builder, &expression._type)?;
                    memory::copy(builder, target, ty, &expression.token_range)
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
                control_flow::auto_pop_scope(builder)?;
            }

            let root_scope = builder
                .fun()
                .scope_stack()
                .first()
                .expect("active function has no root scope")
                .id();
            auto_cleanup(builder, root_scope, expr.token_range.clone())?;
            builder.emit(MIRInstruction::new(
                MIRInstructionKind::Return { value },
                expr.token_range.clone(),
            ));
            MIRValue::Constant(MIRConstant::Unit)
        }

        THIRExpressionKind::Unreachable => {
            builder.emit(MIRInstruction::new(
                MIRInstructionKind::Unreachable,
                expr.token_range.clone(),
            ));
            MIRValue::Constant(MIRConstant::Unit)
        }

        THIRExpressionKind::Yield { value } => {
            let value = value
                .as_deref()
                .map(|value| {
                    let lowered = lower_expression(builder, value)?;
                    Ok(lowered)
                })
                .transpose()?;

            let target = builder
                .fun()
                .control_stack()
                .iter()
                .rev()
                .find_map(|scope| {
                    scope
                        .yield_target()
                        .map(|block| (scope.cleanup_boundary(), block))
                });

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
            auto_cleanup_before(builder, scope_id, expr.token_range.clone())?;
            builder.emit(MIRInstruction::new(
                MIRInstructionKind::Jump {
                    target: MIRBlockTarget::with_args(block_id, args),
                },
                expr.token_range.clone(),
            ));
            MIRValue::Constant(MIRConstant::Unit)
        }

        THIRExpressionKind::Assert { condition, message } => {
            let condition = lower_expression(builder, condition)?;
            builder.fun_mut().emit_intrinsic(
                MIRInternalIntrinsic::Assert {
                    condition,
                    message: Some(message.clone()),
                },
                expr.token_range.clone(),
            );
            MIRValue::Constant(MIRConstant::Unit)
        }
        THIRExpressionKind::Defer {
            expression: deferred,
        } => {
            builder
                .fun_mut()
                .current_scope_mut()
                .add_deferred_expression((**deferred).clone());
            MIRValue::Constant(MIRConstant::Unit)
        }
        THIRExpressionKind::Block {
            statements,
            kind,
            yields,
        } => {
            let result_type = lower_type(builder, &expr._type)?;
            let returns_value = !matches!(
                builder.types().definition(result_type).map(|ty| ty.kind()),
                Some(MIRTypeKind::Void)
            );
            let merge = (*kind == THIRBlockKind::Expression && *yields)
                .then(|| builder.fun_mut().new_block("block.yield"));
            let yield_register = merge.and_then(|merge| {
                returns_value.then(|| builder.fun_mut().set_yield_recipient(merge, result_type))
            });
            let mut result = MIRValue::Constant(MIRConstant::Unit);
            if let Some(merge) = merge {
                builder.fun_mut().push_control_scope();
                builder
                    .fun_mut()
                    .current_control_mut()
                    .set_yield_target(merge);
            }
            if *kind != THIRBlockKind::Sequence {
                builder.fun_mut().push_scope(expr.token_range.clone());
            }

            for statement in statements {
                result = lower_expression(builder, statement)?;
            }

            if merge.is_some() {
                builder.fun_mut().pop_control_scope();
            }
            if *kind != THIRBlockKind::Sequence {
                control_flow::auto_pop_scope(builder)?;
            }

            if let Some(merge) = merge {
                if !builder.fun().current_block_terminated() {
                    builder.emit(MIRInstruction::new(
                        MIRInstructionKind::Unreachable,
                        expr.token_range.clone(),
                    ));
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
        } => calls::lower_call(
            builder,
            function,
            arguments,
            contract,
            &expr._type,
            expr.token_range.clone(),
        )?,

        THIRExpressionKind::VaStart { list, last } => {
            let list = lower_expression(builder, list)?;
            let last = lower_expression(builder, last)?;
            builder.fun_mut().emit_intrinsic(
                MIRVAIntrinsic::VaStart { list, last },
                expr.token_range.clone(),
            );
            MIRValue::Constant(MIRConstant::Unit)
        }

        THIRExpressionKind::VaEnd { list } => {
            let list = lower_expression(builder, list)?;
            builder
                .fun_mut()
                .emit_intrinsic(MIRVAIntrinsic::VaEnd { list }, expr.token_range.clone());
            MIRValue::Constant(MIRConstant::Unit)
        }

        THIRExpressionKind::VaArg { list, _type } => {
            let list = lower_expression(builder, list)?;
            let ty = lower_type(builder, _type)?;
            let out = builder.fun_mut().new_register(ty, None);
            builder.fun_mut().emit_intrinsic(
                MIRVAIntrinsic::VaArg {
                    out: MIRTarget::Register(out),
                    list,
                    ty,
                },
                expr.token_range.clone(),
            );
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
            let value = lower_expression(builder, operand)?;
            lower_coercion(
                builder,
                expr,
                value,
                conversion,
                &operand._type,
                &expr._type,
            )?
        }

        THIRExpressionKind::Leak { expression: inner } => {
            let value = lower_expression(builder, inner)?;
            if let MIRValue::PlaceRef(place) = value {
                builder.emit(MIRInstruction::new(
                    MIRInstructionKind::Invalidate {
                        place: cx_mir::MIRBindable::Place(place),
                        kind: MIRInvalidationKind::Leak,
                    },
                    expr.token_range.clone(),
                ));
                MIRValue::PlaceRef(place)
            } else {
                value
            }
        }
        THIRExpressionKind::Unsafe { expression: inner } => lower_expression(builder, inner)?,
        THIRExpressionKind::StagedExpression(_) | THIRExpressionKind::Materialize { .. } => {
            return log_mir_error(
                &expr.token_range,
                (
                    &mir::COMPTIME_INVALID_OPERATION,
                    "staged expressions".into(),
                ),
            );
        }
    };

    Ok(value)
}
