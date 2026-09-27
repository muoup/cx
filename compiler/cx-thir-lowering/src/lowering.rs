mod calls;
mod control_flow;
mod memory;
mod operators;
mod staged;

pub(crate) mod aggregates;
pub(crate) mod comptime;
pub(crate) mod globals;
pub(crate) mod types;

use cx_log::{CXResult, catalogue::mir, error::CXError};
use cx_mir::{
    MIRAggregateIntrinsic, MIRBindable, MIRBitfieldAccess, MIRBlockTarget, MIRComptimeType,
    MIRConstant, MIRFieldLayout, MIRFunctionID, MIRGlobalRef, MIRInstruction, MIRInstructionKind,
    MIRIntType, MIRInternalIntrinsic, MIRStoreBitfield, MIRTarget, MIRTypeKind, MIRVAIntrinsic,
    MIRValue,
    expr::instruction::MIRInvalidationKind,
    ty::{
        interface::MTRegistry,
        layout::{calculate_field_layout, calculate_type_layout},
    },
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
        operators::{lower_address_of, lower_binary_op, lower_coercion, lower_unary_op},
        types::{bitfield_access, is_signed_integer, lower_int_type, lower_type, reject_comptime},
    },
};

pub(crate) enum LowerStop {
    Diverged,
    Diagnostic(CXError),
}

pub(crate) type LowerResult<T> = Result<T, LowerStop>;

use crate::{
    log::{log_mir_error, mir_error},
    lowering::{control_flow::auto_cleanup, types::lower_float_type},
};

pub(crate) fn lower_function<'thir>(
    builder: &mut MIRBuilder<'thir>,
    id: MIRFunctionID,
    function: &'thir THIRFunction,
) -> CXResult<()> {
    let Some(body) = function.body() else {
        return Ok(());
    };

    builder.start_function(id);

    for (index, parameter) in function.prototype().signature().params().iter().enumerate() {
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
            .bind_local(parameter.local_id(), MIRValue::PlaceRef(place));
        if let Some(name) = parameter.name() {
            builder
                .fun_mut()
                .bind_named_value(name, MIRValue::PlaceRef(place));
        }
    }

    lower_function_block(builder, function, body)?;

    builder.finish_function()
}

pub(crate) fn lower_function_block<'thir>(
    builder: &mut MIRBuilder<'thir>,
    function: &'thir THIRFunction,
    block: &'thir THIRFunctionBody,
) -> CXResult<()> {
    let prototype = function.prototype();
    match block {
        THIRFunctionBody::Expression(expr) => {
            let result = match lower_expression(builder, expr) {
                Ok(value) => value,
                Err(LowerStop::Diverged) => return Ok(()),
                Err(LowerStop::Diagnostic(error)) => return Err(error),
            };
            let result = if expr.ty.is_void() {
                None
            } else {
                Some(match result {
                    // TODO: I don't think this is necessary
                    MIRValue::PlaceRef(place) if !expr.ty.is_memory_reference() => {
                        let ty = lower_type(builder, &expr.ty)?;
                        memory::copy(
                            builder,
                            MIRValue::PlaceRef(place),
                            ty,
                            None,
                            &expr.token_range,
                        )
                    }
                    value => value,
                })
            };
            emit_implicit_return(builder, result, expr.token_range.clone())?;
        }
        THIRFunctionBody::Block { exprs, token_range } => {
            if let Err(LowerStop::Diagnostic(error)) =
                control_flow::lower_sequence(builder, exprs, true)
            {
                return Err(error);
            }

            if prototype.signature().return_type().is_void() {
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
                    if !prototype.signature().return_type().is_unreachable()
                        && function.reject_nonvoid_fallthrough()
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
    match auto_cleanup(builder, root_scope, true, range.clone()) {
        Ok(()) | Err(LowerStop::Diverged) => {}
        Err(LowerStop::Diagnostic(error)) => return Err(error),
    }
    builder.emit(MIRInstruction::new(
        MIRInstructionKind::Return { value },
        range,
    ));
    Ok(())
}

pub(crate) fn lower_expression<'thir>(
    builder: &mut MIRBuilder<'thir>,
    expr: &'thir THIRExpression,
) -> LowerResult<MIRValue> {
    if builder
        .try_fun()
        .is_some_and(|function| function.body().comptime_prototype().is_some())
    {
        reject_comptime(builder, &expr.ty).map_err(LowerStop::Diagnostic)?;
    }

    let value = match &expr.kind {
        THIRExpressionKind::BoolLiteral(value) => MIRValue::Constant(MIRConstant::Integer {
            value: *value as i128,
            ty: MIRIntType::I1,
        }),

        THIRExpressionKind::IntLiteral(value) => {
            let ty = match &expr.ty.kind {
                THIRTypeKind::Integer { ty, .. } => lower_int_type(*ty),
                _ => unreachable!("IntLiteral expression has non-integer type"),
            };

            MIRValue::Constant(MIRConstant::Integer {
                value: *value as i128,
                ty,
            })
        }

        THIRExpressionKind::FloatLiteral(value) => {
            let ty = match expr.ty.kind {
                THIRTypeKind::Float { ty } => lower_float_type(ty),
                _ => unreachable!("FloatLiteral expression has non-float type"),
            };

            MIRValue::Constant(MIRConstant::Float { value: *value, ty })
        }

        THIRExpressionKind::StringLiteral { value } => {
            if builder.types().find_kind(&MIRTypeKind::Str).is_none() {
                builder
                    .types_mut()
                    .intern(cx_mir::MIRType::new(MIRTypeKind::Str));
            }

            MIRValue::Constant(MIRConstant::String(value.clone()))
        }

        THIRExpressionKind::Unit => MIRValue::Constant(MIRConstant::Unit),

        THIRExpressionKind::SizeOf { ty } | THIRExpressionKind::AlignOf { ty } => {
            let type_id = lower_type(builder, ty).map_err(LowerStop::Diagnostic)?;
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
            if let Some(value) = builder.fun().comptime_local(*local_id) {
                staged::runtime_value(builder, value, &expr.token_range)?
            } else {
                builder
                    .local_value(*local_id)
                    .ok_or_else(|| {
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
                    })
                    .map_err(LowerStop::Diagnostic)?
            }
        }

        THIRExpressionKind::GlobalVariable { symbol } => {
            let global = builder
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
                })
                .map_err(LowerStop::Diagnostic)?;
            let ty = match &expr.ty.kind {
                THIRTypeKind::MemoryReference { inner_type, .. } => {
                    types::lower_type_id(builder, *inner_type).map_err(LowerStop::Diagnostic)?
                }
                _ => lower_type(builder, &expr.ty).map_err(LowerStop::Diagnostic)?,
            };

            MIRValue::Constant(MIRConstant::GlobalRef(MIRGlobalRef {
                global,
                offset: 0,
                ty,
            }))
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
            .map(|v| MIRValue::Constant(MIRConstant::Function(v)))
            .map_err(LowerStop::Diagnostic)?,

        THIRExpressionKind::BinaryOperation { lhs, rhs, op } => {
            lower_binary_op(builder, expr, lhs, rhs, op)?
        }

        THIRExpressionKind::UnaryOperation { operand, op } => {
            lower_unary_op(builder, expr, operand, op)?
        }

        THIRExpressionKind::Copy { source } => {
            let lowered = lower_expression(builder, source)?;
            let value_type = lower_type(builder, &expr.ty).map_err(LowerStop::Diagnostic)?;
            let bitfield = bitfield_access(builder, &source.ty).map_err(LowerStop::Diagnostic)?;
            memory::copy(builder, lowered, value_type, bitfield, &expr.token_range)
        }

        THIRExpressionKind::Move { local_id, .. } => {
            let local = builder.fun().local(*local_id).expect("local should exist");
            let ty = lower_type(builder, &expr.ty).map_err(LowerStop::Diagnostic)?;

            move_value(builder, local, ty, &expr.token_range).map_err(LowerStop::Diagnostic)?
        }

        THIRExpressionKind::CreateLocalVariable {
            name,
            local_id,
            ty,
            initial_value,
        } => {
            let initial_value = initial_value
                .as_deref()
                .map(|value| lower_expression(builder, value))
                .transpose()?;

            let place = allocate_variable(
                builder,
                Some(name.clone()),
                ty,
                initial_value,
                &expr.token_range,
            )
            .map_err(LowerStop::Diagnostic)?;

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
            ty,
            initial_value,
        } => {
            let address = lower_expression(builder, initial_value)?;
            let type_id = lower_type(builder, ty).map_err(LowerStop::Diagnostic)?;
            let place = builder.new_place(type_id, Some(binding_name.clone()), ty.is_nodrop());
            builder.fun_mut().body_mut().mark_adopted(place);
            builder.fun_mut().emit_intrinsic(
                MIRInternalIntrinsic::AdoptPlace { place, address },
                expr.token_range.clone(),
            );
            builder.emit(MIRInstruction::new(
                MIRInstructionKind::Initialize {
                    place: MIRBindable::Place(place),
                },
                expr.token_range.clone(),
            ));
            let value = MIRValue::PlaceRef(place);
            builder.fun_mut().bind_local(*local_id, value.clone());
            builder
                .fun_mut()
                .bind_named_value(binding_name, value.clone());
            value
        }

        THIRExpressionKind::Assign { target, value } => {
            let assignment_type = lower_type(builder, &value.ty).map_err(LowerStop::Diagnostic)?;
            let bitfield = bitfield_access(builder, &target.ty).map_err(LowerStop::Diagnostic)?;

            let mlhs = lower_expression(builder, target)?;
            let mvalue = lower_expression(builder, value)?;

            let mtarget = memory::expect_target(&mlhs);
            if let MIRTarget::Place(place) = mtarget {
                builder.emit(MIRInstruction::new(
                    MIRInstructionKind::Invalidate {
                        place: MIRBindable::Place(place),
                        kind: MIRInvalidationKind::Drop,
                    },
                    target.token_range.clone(),
                ));

                builder.emit(MIRInstruction::new(
                    MIRInstructionKind::Initialize {
                        place: MIRBindable::Place(place),
                    },
                    target.token_range.clone(),
                ));
            }

            builder.emit(MIRInstruction::new(
                MIRInstructionKind::Store {
                    target: mtarget,
                    value: mvalue,
                    ty: assignment_type,
                    bitfield: bitfield.map(MIRStoreBitfield::Target),
                },
                target.token_range.clone(),
            ));

            mlhs
        }

        THIRExpressionKind::AddressOf { operand } => lower_address_of(builder, expr, operand)?,

        THIRExpressionKind::MemberAccess {
            base,
            member_index,
            aggregate_type,
        } => {
            let base_value = lower_expression(builder, base)?;
            let type_id = lower_type(builder, &expr.ty).map_err(LowerStop::Diagnostic)?;
            let aggregate_type_id =
                lower_type(builder, aggregate_type).map_err(LowerStop::Diagnostic)?;
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
            let element_type_id =
                lower_type(builder, element_type).map_err(LowerStop::Diagnostic)?;
            let return_type = lower_type(builder, &expr.ty).map_err(LowerStop::Diagnostic)?;

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
            aggregates::lower_pattern_test(builder, lhs, pattern, &expr.ty, None)?
        }

        THIRExpressionKind::Unpack {
            value, bindings, ..
        } => {
            let lowered_value = lower_expression(builder, value)?;

            let source_type = match &value.ty.kind {
                THIRTypeKind::MemoryReference { inner_type, .. } => {
                    builder.registry().resolve_type_id(*inner_type)
                }
                _ => &value.ty,
            };
            let struct_type_id = lower_type(builder, source_type).map_err(LowerStop::Diagnostic)?;
            let moved =
                memory::move_value(builder, lowered_value, struct_type_id, &expr.token_range)
                    .map_err(LowerStop::Diagnostic)?;
            let base =
                allocate_variable(builder, None, source_type, Some(moved), &expr.token_range)
                    .map_err(LowerStop::Diagnostic)?;

            for binding in bindings {
                let field_type =
                    lower_type(builder, &binding.field_type).map_err(LowerStop::Diagnostic)?;
                let aggregate_type =
                    lower_type(builder, source_type).map_err(LowerStop::Diagnostic)?;
                // A bitfield projects to its storage unit, so it is extracted through a reference
                let bitfield = match calculate_field_layout(
                    builder.types(),
                    aggregate_type,
                    binding.field_index,
                ) {
                    Some(MIRFieldLayout::Bitfield {
                        bit_offset,
                        bit_width,
                        ..
                    }) => Some(MIRBitfieldAccess {
                        bit_offset,
                        bit_width,
                        signed: is_signed_integer(builder, &binding.field_type),
                    }),
                    _ => None,
                };
                let projection_type = match bitfield {
                    Some(_) => builder
                        .types_mut()
                        .reference_to(field_type)
                        .map_err(LowerStop::Diagnostic)?,
                    None => field_type,
                };
                let field_register = builder.fun_mut().new_register(projection_type, None);
                builder.fun_mut().emit_intrinsic(
                    MIRAggregateIntrinsic::StructField {
                        out: MIRTarget::Register(field_register),
                        base: MIRValue::PlaceRef(base),
                        field: binding.field_index,
                        struct_ty: aggregate_type,
                    },
                    expr.token_range.clone(),
                );
                let field_value = match bitfield {
                    Some(access) => memory::copy(
                        builder,
                        MIRValue::Register(field_register),
                        field_type,
                        Some(access),
                        &expr.token_range,
                    ),
                    None => MIRValue::Register(field_register),
                };
                builder
                    .fun_mut()
                    .bind_local(binding.binding_local_id, field_value);
            }

            builder.emit(MIRInstruction::new(
                MIRInstructionKind::Invalidate {
                    place: MIRBindable::Place(base),
                    kind: MIRInvalidationKind::Move,
                },
                expr.token_range.clone(),
            ));

            MIRValue::Constant(MIRConstant::Unit)
        }

        THIRExpressionKind::TaggedUnionTag { value, sum_type } => {
            let base = lower_expression(builder, value)?;
            let type_id = lower_type(builder, &expr.ty).map_err(LowerStop::Diagnostic)?;
            let out = builder.fun_mut().new_register(type_id, None);
            let sum_type_id = lower_type(builder, sum_type).map_err(LowerStop::Diagnostic)?;
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

        THIRExpressionKind::TaggedUnionSet {
            target,
            variant_index,
            inner_value,
            sum_type,
        } => {
            let target_value = lower_expression(builder, target)?;
            let mtarget = memory::expect_target(&target_value);
            let value = lower_expression(builder, inner_value)?;
            let sum_type_id = lower_type(builder, sum_type).map_err(LowerStop::Diagnostic)?;
            let constructed = builder.fun_mut().new_register(sum_type_id, None);
            builder.fun_mut().emit_intrinsic(
                MIRAggregateIntrinsic::AggregateInit {
                    out: MIRTarget::Register(constructed),
                    ty: sum_type_id,
                    fields: vec![(*variant_index, value)],
                },
                expr.token_range.clone(),
            );
            builder.emit(MIRInstruction::new(
                MIRInstructionKind::Store {
                    target: mtarget,
                    value: MIRValue::Register(constructed),
                    ty: sum_type_id,
                    bitfield: None,
                },
                expr.token_range.clone(),
            ));
            target_value
        }

        THIRExpressionKind::TaggedUnionInitializer {
            variant_index,
            value,
            sum_type,
        } => {
            let value = lower_expression(builder, value)?;
            let sum_type_id = lower_type(builder, sum_type).map_err(LowerStop::Diagnostic)?;
            let type_id = lower_type(builder, &expr.ty).map_err(LowerStop::Diagnostic)?;
            let out = builder.fun_mut().new_register(type_id, None);
            builder.fun_mut().emit_intrinsic(
                MIRAggregateIntrinsic::AggregateInit {
                    out: MIRTarget::Register(out),
                    ty: sum_type_id,
                    fields: vec![(*variant_index, value)],
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
            let type_id = lower_type(builder, &expr.ty).map_err(LowerStop::Diagnostic)?;
            if let Some(MIRTypeKind::Array { length, .. }) =
                builder.types().definition(type_id).map(|ty| ty.kind())
                && fields.len() > *length
            {
                return log_mir_error(
                    &expr.token_range,
                    (&mir::ARRAY_TOO_LONG, (fields.len(), *length)),
                )
                .map_err(LowerStop::Diagnostic);
            }
            let out = builder.fun_mut().new_register(type_id, None);
            builder.fun_mut().emit_intrinsic(
                MIRAggregateIntrinsic::AggregateInit {
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
            let type_id = lower_type(builder, &expr.ty).map_err(LowerStop::Diagnostic)?;
            let out = builder.fun_mut().new_register(type_id, None);
            let aggregate_type_id =
                lower_type(builder, struct_type).map_err(LowerStop::Diagnostic)?;
            builder.fun_mut().emit_intrinsic(
                MIRAggregateIntrinsic::AggregateInit {
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
                return log_mir_error(
                    &expr.token_range,
                    (
                        &mir::REQUIRED_CONTEXT,
                        ("break".into(), "a loop or switch".into()),
                    ),
                )
                .map_err(LowerStop::Diagnostic);
            };

            auto_cleanup(builder, scope, false, expr.token_range.clone())?;
            builder.emit(MIRInstruction::new(
                MIRInstructionKind::Jump {
                    target: MIRBlockTarget::new(target),
                },
                expr.token_range.clone(),
            ));

            return Err(LowerStop::Diverged);
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
                return log_mir_error(
                    &expr.token_range,
                    (&mir::REQUIRED_CONTEXT, ("continue".into(), "a loop".into())),
                )
                .map_err(LowerStop::Diagnostic);
            };

            auto_cleanup(builder, scope, false, expr.token_range.clone())?;
            builder.emit(MIRInstruction::new(
                MIRInstructionKind::Jump {
                    target: MIRBlockTarget::new(target),
                },
                expr.token_range.clone(),
            ));

            return Err(LowerStop::Diverged);
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
            return Err(LowerStop::Diverged);
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
            &expr.ty,
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
        } => control_flow::lower_match(builder, condition, *subject, arms, &expr.ty)?,
        THIRExpressionKind::Return {
            postcondition,
            value,
        } => {
            let staged_return =
                builder
                    .fun()
                    .body()
                    .comptime_prototype()
                    .is_some_and(|prototype| {
                        matches!(
                            prototype.signature().return_type(),
                            MIRComptimeType::StagedExpression { .. }
                        )
                    });
            if staged_return {
                let operand = value
                    .as_deref()
                    .map(|value| staged::lower_operand(builder, value))
                    .transpose()?;
                if postcondition.is_some() {
                    return log_mir_error(
                        &expr.token_range,
                        (
                            &mir::COMPTIME_INVALID_OPERATION,
                            "staged return postcondition".into(),
                        ),
                    )
                    .map_err(LowerStop::Diagnostic);
                }
                let root_scope = builder
                    .fun()
                    .scope_stack()
                    .first()
                    .expect("active function has no root scope")
                    .id();
                auto_cleanup(builder, root_scope, true, expr.token_range.clone())?;
                builder.emit_comptime(
                    cx_mir::MIRComptimeOp::Return { value: operand },
                    expr.token_range.clone(),
                );
                return Err(LowerStop::Diverged);
            }
            let value_expression = value.as_deref();
            let lowered_value = value_expression
                .map(|value| lower_expression(builder, value))
                .transpose()?;
            let lowered_value = lowered_value.unwrap_or(MIRValue::Constant(MIRConstant::Unit));
            let lowered_value = match (lowered_value, value_expression) {
                (MIRValue::PlaceRef(target), Some(expression))
                    if !expression.ty.is_memory_reference() =>
                {
                    let ty = lower_type(builder, &expression.ty).map_err(LowerStop::Diagnostic)?;
                    memory::copy(
                        builder,
                        MIRValue::PlaceRef(target),
                        ty,
                        None,
                        &expression.token_range,
                    )
                }
                (value, _) => value,
            };
            let value = value_expression.map(|_| lowered_value);
            if let Some(postcondition) = postcondition {
                builder
                    .fun_mut()
                    .push_scope(postcondition.condition().token_range.clone());
                if let (Some(name), Some(value)) = (postcondition.binding(), value.clone()) {
                    builder.fun_mut().bind_named_value(name, value);
                }
                lower_expression(builder, postcondition.condition())?;
                control_flow::auto_pop_scope(builder)?;
            }

            let root_scope = builder
                .fun()
                .scope_stack()
                .first()
                .expect("active function has no root scope")
                .id();
            auto_cleanup(builder, root_scope, true, expr.token_range.clone())?;
            builder.emit(MIRInstruction::new(
                MIRInstructionKind::Return { value },
                expr.token_range.clone(),
            ));
            return Err(LowerStop::Diverged);
        }

        THIRExpressionKind::Unreachable => {
            builder.emit(MIRInstruction::new(
                MIRInstructionKind::Unreachable,
                expr.token_range.clone(),
            ));
            return Err(LowerStop::Diverged);
        }

        THIRExpressionKind::Yield { value } => {
            let value = value
                .as_deref()
                .map(|value| lower_expression(builder, value))
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
                )
                .map_err(LowerStop::Diagnostic);
            };

            if let Some(value) = &value
                && let Some(expected) = builder.fun().body().block_parameter_type(block_id, 0)
            {
                memory::check_block_argument(builder, value, expected, &expr.token_range)
                    .map_err(LowerStop::Diagnostic)?;
            }

            let args = value.into_iter().collect();
            auto_cleanup(builder, scope_id, false, expr.token_range.clone())?;
            builder.emit(MIRInstruction::new(
                MIRInstructionKind::Jump {
                    target: MIRBlockTarget::with_args(block_id, args),
                },
                expr.token_range.clone(),
            ));
            return Err(LowerStop::Diverged);
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
            builder.fun_mut().add_deferred_expression(deferred);
            MIRValue::Constant(MIRConstant::Unit)
        }
        THIRExpressionKind::Block {
            statements,
            kind,
            yields,
        } => {
            let result_type = lower_type(builder, &expr.ty).map_err(LowerStop::Diagnostic)?;
            let returns_value = !matches!(
                builder.types().definition(result_type).map(|ty| ty.kind()),
                Some(MIRTypeKind::Void)
            );
            let merge = (*kind == THIRBlockKind::Expression && *yields)
                .then(|| builder.fun_mut().new_block("block.yield"));
            let yield_register = merge.and_then(|merge| {
                returns_value.then(|| builder.fun_mut().set_yield_recipient(merge, result_type))
            });
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

            let result = match control_flow::lower_sequence(builder, statements, true) {
                Err(LowerStop::Diagnostic(error)) => return Err(LowerStop::Diagnostic(error)),
                result => result,
            };

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
                let reaches_merge = builder.fun().body().block_has_predecessor(merge);
                builder.fun_mut().set_current_block(merge);
                if !reaches_merge {
                    builder.emit(MIRInstruction::new(
                        MIRInstructionKind::Unreachable,
                        expr.token_range.clone(),
                    ));
                    return Err(LowerStop::Diverged);
                }
                yield_register
                    .map(MIRValue::Register)
                    .unwrap_or(MIRValue::Constant(MIRConstant::Unit))
            } else {
                result?
            }
        }

        THIRExpressionKind::CallFunction {
            function,
            arguments,
            contract,
        } => {
            let value = calls::lower_call(
                builder,
                function,
                arguments,
                contract,
                &expr.ty,
                expr.token_range.clone(),
            )?;
            staged::runtime_value(builder, value, &expr.token_range)?
        }

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

        THIRExpressionKind::VaArg { list, ty } => {
            let list = lower_expression(builder, list)?;
            let ty = lower_type(builder, ty).map_err(LowerStop::Diagnostic)?;
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
                builder.emit(MIRInstruction::new(
                    MIRInstructionKind::Unreachable,
                    expr.token_range.clone(),
                ));
                return Err(LowerStop::Diverged);
            }
            let value = lower_expression(builder, operand)?;
            lower_coercion(builder, expr, value, conversion, &expr.ty)?
        }

        THIRExpressionKind::Leak { expression: inner } => {
            let value = lower_expression(builder, inner)?;
            let nodrop = builder
                .registry()
                .mem_ref_inner(&inner.ty)
                .is_some_and(|ty| ty.is_nodrop());
            match value {
                MIRValue::PlaceRef(place) if nodrop => {
                    builder.emit(MIRInstruction::new(
                        MIRInstructionKind::Invalidate {
                            place: cx_mir::MIRBindable::Place(place),
                            kind: MIRInvalidationKind::Leak,
                        },
                        expr.token_range.clone(),
                    ));
                    MIRValue::PlaceRef(place)
                }
                value => value,
            }
        }
        THIRExpressionKind::Unsafe { expression: inner } => lower_expression(builder, inner)?,
        THIRExpressionKind::StagedExpression(_) => {
            return log_mir_error(
                &expr.token_range,
                (
                    &mir::COMPTIME_INVALID_OPERATION,
                    "staged value requires a comptime destination".into(),
                ),
            )
            .map_err(LowerStop::Diagnostic);
        }
        THIRExpressionKind::Materialize {
            expr: staged_expr,
            with_params,
        } => {
            if builder.fun().body().is_comptime() {
                return log_mir_error(
                    &expr.token_range,
                    (
                        &mir::COMPTIME_INVALID_OPERATION,
                        "materialization in a comptime function".into(),
                    ),
                )
                .map_err(LowerStop::Diagnostic);
            }
            let staged = staged::lower_operand(builder, staged_expr)?;
            staged::materialize(builder, staged, with_params, &expr.token_range)?
        }
    };

    Ok(value)
}
