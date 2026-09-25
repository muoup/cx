use cx_mir::{
    MIRAggregateIntrinsic, MIRBindable, MIRBlockTarget, MIRConstant, MIRInstruction,
    MIRInstructionKind, MIRIntType, MIRScopeID, MIRTarget, MIRType, MIRTypeKind, MIRValue,
    expr::instruction::MIRInvalidationKind, ty::interface::MTRegistry,
};
use cx_thir::thir::{
    data::{THIRType, THIRTypeKind},
    expression::{THIRExpression, THIRExpressionKind, THIRLocalID},
    pattern::THIRPattern,
};
use cx_thir::type_context::THIRTypeContext;
use cx_tokens::TokenRange;

use crate::{
    builder::{DeferredExpression, MIRBuilder},
    log::log_mir_error,
    lowering::{
        LowerResult, LowerStop, aggregates, comptime, lower_expression, memory, operators,
        types::lower_type,
    },
};

pub(super) fn lower_sequence<'thir>(
    builder: &mut MIRBuilder<'thir>,
    statements: &'thir [THIRExpression],
    mut live: bool,
) -> LowerResult<MIRValue> {
    let mut result = MIRValue::Constant(MIRConstant::Unit);
    for statement in statements {
        if live {
            match lower_expression(builder, statement) {
                Ok(value) => result = value,
                Err(LowerStop::Diverged) => live = false,
                Err(error) => return Err(error),
            }
        } else {
            live = lower_dead_labels(builder, statement)?;
            result = MIRValue::Constant(MIRConstant::Unit);
        }
    }
    if live {
        Ok(result)
    } else {
        Err(LowerStop::Diverged)
    }
}

fn lower_dead_labels<'thir>(
    builder: &mut MIRBuilder<'thir>,
    expression: &'thir THIRExpression,
) -> LowerResult<bool> {
    match &expression.kind {
        THIRExpressionKind::Label { name, statement } => {
            let Some(target) = builder.fun_mut().label(name) else {
                return Ok(false);
            };
            builder.fun_mut().set_current_block(target);
            match lower_expression(builder, statement) {
                Ok(_) => Ok(!builder.fun().current_block_terminated()),
                Err(LowerStop::Diverged) => Ok(false),
                Err(error) => Err(error),
            }
        }
        THIRExpressionKind::Block {
            statements, kind, ..
        } => {
            if *kind != cx_thir::thir::expression::THIRBlockKind::Sequence {
                builder.fun_mut().push_scope(expression.token_range.clone());
            }
            let result = lower_sequence(builder, statements, false);
            if *kind != cx_thir::thir::expression::THIRBlockKind::Sequence {
                auto_pop_scope(builder)?;
            }
            match result {
                Ok(_) => Ok(true),
                Err(LowerStop::Diverged) => Ok(false),
                Err(error) => Err(error),
            }
        }
        THIRExpressionKind::If {
            then_branch,
            else_branch,
            ..
        } => {
            let then_live = lower_dead_labels(builder, then_branch)?;
            let then_end = then_live.then(|| builder.fun().current_block());
            let else_live = match else_branch {
                Some(branch) => lower_dead_labels(builder, branch)?,
                None => false,
            };
            if then_live && else_live {
                let else_end = builder.fun().current_block();
                let merge = builder.fun_mut().new_block("label.merge");
                for end in [then_end.unwrap(), else_end] {
                    builder.fun_mut().set_current_block(end);
                    builder.emit(MIRInstruction::new(
                        MIRInstructionKind::Jump {
                            target: MIRBlockTarget::new(merge),
                        },
                        expression.token_range.clone(),
                    ));
                }
                builder.fun_mut().set_current_block(merge);
            } else if let Some(end) = then_end {
                builder.fun_mut().set_current_block(end);
            }
            Ok(then_live || else_live)
        }
        _ => Ok(false),
    }
}

pub fn lower_scoped<'thir>(
    builder: &mut MIRBuilder<'thir>,
    expression: &'thir THIRExpression,
) -> LowerResult<MIRValue> {
    builder.fun_mut().push_scope(expression.token_range.clone());
    let result = lower_expression(builder, expression);
    let cleanup = auto_pop_scope(builder);
    match result {
        Ok(value) => {
            cleanup?;
            Ok(value)
        }
        Err(error) => Err(error),
    }
}

pub fn auto_cleanup<'thir>(
    builder: &mut MIRBuilder<'thir>,
    to_scope: MIRScopeID,
    include_target: bool,
    range: TokenRange,
) -> LowerResult<()> {
    let mut pending = Vec::new();
    for scope in builder.fun().scope_stack().iter().rev() {
        if scope.id() == to_scope && !include_target {
            break;
        }
        pending.push((scope.id(), scope.deferred_expressions().to_vec()));
        if scope.id() == to_scope {
            break;
        }
    }
    for (scope, defers) in &pending {
        for defer in defers.iter().rev() {
            lower_deferred(builder, defer)?;
        }
        emit_scope_end(builder, *scope, range.clone());
    }
    Ok(())
}

pub fn auto_pop_scope<'thir>(builder: &mut MIRBuilder<'thir>) -> LowerResult<()> {
    let scope = builder.fun().current_scope_id();
    let range = builder.fun().current_scope_range();

    let cleanup = (|| -> LowerResult<()> {
        if builder.fun().current_block_terminated() {
            return Ok(());
        }
        let defers = builder
            .fun()
            .current_scope()
            .deferred_expressions()
            .to_vec();
        for defer in defers.into_iter().rev() {
            lower_deferred(builder, &defer)?;
        }
        emit_scope_end(builder, scope, range);
        Ok(())
    })();

    let _ = builder.fun_mut().pop_scope();
    cleanup
}

fn lower_deferred<'thir>(
    builder: &mut MIRBuilder<'thir>,
    defer: &DeferredExpression<'thir>,
) -> LowerResult<()> {
    let saved = builder
        .fun_mut()
        .replace_local_bindings(defer.locals.clone(), defer.comptime.clone());
    let result = lower_expression(builder, defer.expression).map(|_| ());
    builder.fun_mut().replace_local_bindings(saved.0, saved.1);
    result
}

fn emit_scope_end(builder: &mut MIRBuilder, scope: MIRScopeID, range: TokenRange) {
    for place in builder.fun().places_in_scope(scope) {
        builder.emit(MIRInstruction::new(
            MIRInstructionKind::Invalidate {
                place: MIRBindable::Place(place),
                kind: MIRInvalidationKind::Drop,
            },
            range.clone(),
        ));
    }
}

pub(super) fn lower_if<'thir>(
    builder: &mut MIRBuilder<'thir>,
    condition: &'thir THIRExpression,
    then_branch: &'thir THIRExpression,
    else_branch: Option<&'thir THIRExpression>,
    result_type: &'thir THIRType,
) -> LowerResult<MIRValue> {
    let (condition_value, pattern_subject) = match lower_pattern_if_condition(builder, condition) {
        Some(result) => {
            let (value, subject, pattern, ty) = result?;
            (value, Some((subject, pattern, ty)))
        }
        None => (lower_scoped(builder, condition)?, None),
    };
    let then_block = builder.fun_mut().new_block("if.then");
    let else_block = if else_branch.is_some() {
        Some(builder.fun_mut().new_block("if.else"))
    } else {
        None
    };
    let merge = builder.fun_mut().new_block("if.merge");
    let result_type_id = lower_type(builder, result_type).map_err(LowerStop::Diagnostic)?;
    let yielding = !matches!(
        builder
            .types()
            .definition(result_type_id)
            .map(|ty| ty.kind()),
        Some(MIRTypeKind::Void)
    );
    let yield_register =
        yielding.then(|| builder.fun_mut().set_yield_recipient(merge, result_type_id));

    builder.emit(MIRInstruction::new(
        MIRInstructionKind::Branch {
            cond: condition_value,
            true_target: MIRBlockTarget::new(then_block),
            false_target: MIRBlockTarget::new(else_block.unwrap_or(merge)),
        },
        condition.token_range.clone(),
    ));

    builder.fun_mut().set_current_block(then_block);

    if let Some((subject, pattern, ty)) = pattern_subject {
        aggregates::bind_pattern_payload(builder, pattern, subject, ty)
            .map_err(LowerStop::Diagnostic)?;
    }

    builder.fun_mut().push_control_scope();
    if yielding {
        builder
            .fun_mut()
            .current_control_mut()
            .set_yield_target(merge);
    }
    let then_result = lower_expression(builder, then_branch);
    builder.fun_mut().pop_control_scope();
    match then_result {
        Ok(_) if !builder.fun().current_block_terminated() => builder.emit(MIRInstruction::new(
            if yielding {
                MIRInstructionKind::Unreachable
            } else {
                MIRInstructionKind::Jump {
                    target: MIRBlockTarget::new(merge),
                }
            },
            then_branch.token_range.clone(),
        )),
        Err(LowerStop::Diagnostic(error)) => return Err(LowerStop::Diagnostic(error)),
        _ => {}
    }

    if let Some(else_branch) = else_branch {
        builder.fun_mut().set_current_block(else_block.unwrap());

        builder.fun_mut().push_control_scope();
        if yielding {
            builder
                .fun_mut()
                .current_control_mut()
                .set_yield_target(merge);
        }
        let else_result = lower_expression(builder, else_branch);
        builder.fun_mut().pop_control_scope();

        match else_result {
            Ok(_) if !builder.fun().current_block_terminated() => {
                builder.emit(MIRInstruction::new(
                    if yielding {
                        MIRInstructionKind::Unreachable
                    } else {
                        MIRInstructionKind::Jump {
                            target: MIRBlockTarget::new(merge),
                        }
                    },
                    else_branch.token_range.clone(),
                ))
            }
            Err(LowerStop::Diagnostic(error)) => return Err(LowerStop::Diagnostic(error)),
            _ => {}
        }
    }

    let reaches_merge = builder.fun().body().block_has_predecessor(merge);
    builder.fun_mut().set_current_block(merge);
    if !reaches_merge {
        builder.emit(MIRInstruction::new(
            MIRInstructionKind::Unreachable,
            condition.token_range.clone(),
        ));
        return Err(LowerStop::Diverged);
    }
    Ok(match yield_register {
        Some(reg) => MIRValue::Register(reg),
        None => MIRValue::Constant(MIRConstant::Unit),
    })
}

fn lower_pattern_if_condition<'thir>(
    builder: &mut MIRBuilder<'thir>,
    condition: &'thir THIRExpression,
) -> Option<LowerResult<(MIRValue, MIRValue, &'thir THIRPattern, &'thir THIRType)>> {
    match &condition.kind {
        THIRExpressionKind::PatternIs { lhs, pattern } => Some((|| {
            let subject = lower_expression(builder, lhs)?;
            let result = aggregates::lower_pattern_test(
                builder,
                lhs,
                pattern,
                &condition._type,
                Some(subject.clone()),
            )?;
            Ok((result, subject, pattern, &lhs._type))
        })()),
        THIRExpressionKind::TypeConversion {
            operand,
            conversion,
        } => lower_pattern_if_condition(builder, operand).map(|result| {
            let (value, subject, pattern, ty) = result?;
            let value = operators::lower_coercion(
                builder,
                condition,
                value,
                conversion,
                &operand._type,
                &condition._type,
            )?;
            Ok((value, subject, pattern, ty))
        }),
        THIRExpressionKind::Typechange(operand) => lower_pattern_if_condition(builder, operand),
        _ => None,
    }
}

pub(super) fn lower_while<'thir>(
    builder: &mut MIRBuilder<'thir>,
    condition: &'thir THIRExpression,
    body: &'thir THIRExpression,
    pre_eval: bool,
) -> LowerResult<()> {
    let condition_block = builder.fun_mut().new_block("while.condition");
    let body_block = builder.fun_mut().new_block("while.body");
    let exit_block = builder.fun_mut().new_block("while.exit");

    builder.emit(MIRInstruction::new(
        MIRInstructionKind::Jump {
            target: MIRBlockTarget::new(if pre_eval {
                condition_block
            } else {
                body_block
            }),
        },
        condition.token_range.clone(),
    ));

    builder.fun_mut().set_current_block(condition_block);
    let condition = match lower_scoped(builder, condition) {
        Ok(value) => value,
        Err(LowerStop::Diverged) => {
            for block in [body_block, exit_block] {
                builder.fun_mut().set_current_block(block);
                builder.emit(MIRInstruction::new(
                    MIRInstructionKind::Unreachable,
                    body.token_range.clone(),
                ));
            }
            return Err(LowerStop::Diverged);
        }
        Err(error) => return Err(error),
    };

    builder.emit(MIRInstruction::new(
        MIRInstructionKind::Branch {
            cond: condition,
            true_target: MIRBlockTarget::new(body_block),
            false_target: MIRBlockTarget::new(exit_block),
        },
        body.token_range.clone(),
    ));

    builder.fun_mut().set_current_block(body_block);
    builder.fun_mut().push_control_scope();
    builder
        .fun_mut()
        .current_control_mut()
        .set_break_target(exit_block)
        .set_continue_target(condition_block);

    let body_result = lower_expression(builder, body);
    builder.fun_mut().pop_control_scope();
    if let Err(LowerStop::Diagnostic(error)) = body_result {
        return Err(LowerStop::Diagnostic(error));
    }

    builder.emit(MIRInstruction::new(
        MIRInstructionKind::Jump {
            target: MIRBlockTarget::new(condition_block),
        },
        body.token_range.clone(),
    ));

    builder.fun_mut().set_current_block(exit_block);
    Ok(())
}

pub(super) fn lower_for<'thir>(
    builder: &mut MIRBuilder<'thir>,
    init: &'thir THIRExpression,
    condition: &'thir THIRExpression,
    increment: &'thir THIRExpression,
    body: &'thir THIRExpression,
) -> LowerResult<()> {
    lower_expression(builder, init)?;

    let condition_block = builder.fun_mut().new_block("for.condition");
    let body_block = builder.fun_mut().new_block("for.body");
    let increment_block = builder.fun_mut().new_block("for.increment");
    let exit_block = builder.fun_mut().new_block("for.exit");

    builder.emit(MIRInstruction::new(
        MIRInstructionKind::Jump {
            target: MIRBlockTarget::new(condition_block),
        },
        init.token_range.clone(),
    ));

    builder.fun_mut().set_current_block(condition_block);
    let condition = match lower_expression(builder, condition) {
        Ok(value) => value,
        Err(LowerStop::Diverged) => {
            for block in [body_block, increment_block, exit_block] {
                builder.fun_mut().set_current_block(block);
                builder.emit(MIRInstruction::new(
                    MIRInstructionKind::Unreachable,
                    condition.token_range.clone(),
                ));
            }
            return Err(LowerStop::Diverged);
        }
        Err(error) => return Err(error),
    };
    builder.emit(MIRInstruction::new(
        MIRInstructionKind::Branch {
            cond: condition,
            true_target: MIRBlockTarget::new(body_block),
            false_target: MIRBlockTarget::new(exit_block),
        },
        body.token_range.clone(),
    ));

    builder.fun_mut().set_current_block(body_block);
    builder.fun_mut().push_control_scope();
    builder
        .fun_mut()
        .current_control_mut()
        .set_break_target(exit_block)
        .set_continue_target(increment_block);

    let body_result = lower_expression(builder, body);

    builder.fun_mut().pop_control_scope();
    if let Err(LowerStop::Diagnostic(error)) = body_result {
        return Err(LowerStop::Diagnostic(error));
    }
    builder.emit(MIRInstruction::new(
        MIRInstructionKind::Jump {
            target: MIRBlockTarget::new(increment_block),
        },
        body.token_range.clone(),
    ));

    builder.fun_mut().set_current_block(increment_block);
    let increment_result = lower_expression(builder, increment);
    if let Err(LowerStop::Diagnostic(error)) = increment_result {
        return Err(LowerStop::Diagnostic(error));
    }
    builder.emit(MIRInstruction::new(
        MIRInstructionKind::Jump {
            target: MIRBlockTarget::new(condition_block),
        },
        increment.token_range.clone(),
    ));
    builder.fun_mut().set_current_block(exit_block);
    Ok(())
}

pub(super) fn lower_switch<'thir>(
    builder: &mut MIRBuilder<'thir>,
    condition: &'thir THIRExpression,
    cases: &'thir [(Box<THIRExpression>, Box<THIRExpression>)],
    default: Option<&'thir THIRExpression>,
) -> LowerResult<()> {
    let value = lower_expression(builder, condition)?;
    let exit = builder.fun_mut().new_block("switch.exit");
    let default_block = default
        .map(|_| builder.fun_mut().new_block("switch.default"))
        .unwrap_or(exit);
    let mut targets = Vec::with_capacity(cases.len());
    let mut bodies = Vec::with_capacity(cases.len());

    for (case, _) in cases {
        let block = builder.fun_mut().new_block("switch.case");
        let case_value = comptime::evaluate(builder, case).map_err(LowerStop::Diagnostic)?;

        let MIRConstant::Integer { value, .. } = case_value else {
            return log_mir_error(
                &case.token_range,
                (
                    &cx_log::catalogue::mir::ENTITY_REQUIREMENT,
                    ("switch case".into(), "an integer constant".into(), None),
                ),
            )
            .map_err(LowerStop::Diagnostic);
        };

        targets.push((value, MIRBlockTarget::new(block)));
        bodies.push(block);
    }

    builder.emit(MIRInstruction::new(
        MIRInstructionKind::CaseBranch {
            value,
            cases: targets,
            default: Some(MIRBlockTarget::new(default_block)),
        },
        condition.token_range.clone(),
    ));

    for ((_, body), block) in cases.iter().zip(bodies) {
        builder.fun_mut().set_current_block(block);
        builder.fun_mut().push_control_scope();
        builder
            .fun_mut()
            .current_control_mut()
            .set_break_target(exit);
        builder.fun_mut().push_scope(body.token_range.clone());
        let body_result = lower_expression(builder, body);
        auto_pop_scope(builder)?;
        builder.fun_mut().pop_control_scope();
        if let Err(LowerStop::Diagnostic(error)) = body_result {
            return Err(LowerStop::Diagnostic(error));
        }

        builder.emit(MIRInstruction::new(
            MIRInstructionKind::Jump {
                target: MIRBlockTarget::new(exit),
            },
            body.token_range.clone(),
        ));
    }

    if let Some(default) = default {
        builder.fun_mut().set_current_block(default_block);
        builder.fun_mut().push_control_scope();
        builder
            .fun_mut()
            .current_control_mut()
            .set_break_target(exit);
        builder.fun_mut().push_scope(default.token_range.clone());
        let default_result = lower_expression(builder, default);
        auto_pop_scope(builder)?;
        builder.fun_mut().pop_control_scope();
        if let Err(LowerStop::Diagnostic(error)) = default_result {
            return Err(LowerStop::Diagnostic(error));
        }
        builder.emit(MIRInstruction::new(
            MIRInstructionKind::Jump {
                target: MIRBlockTarget::new(exit),
            },
            default.token_range.clone(),
        ));
    }

    builder.fun_mut().set_current_block(exit);
    Ok(())
}

pub(super) fn lower_match<'thir>(
    builder: &mut MIRBuilder<'thir>,
    condition: &'thir THIRExpression,
    subject: THIRLocalID,
    arms: &'thir [(THIRPattern, Box<THIRExpression>)],
    result_type: &'thir THIRType,
) -> LowerResult<MIRValue> {
    let subject_value = lower_expression(builder, condition)?;
    let subject_value = if condition._type.is_memory_reference() {
        subject_value
    } else {
        let place = memory::move_operand_to_place(
            builder,
            subject_value,
            &condition._type,
            None,
            &condition.token_range,
        )
        .map_err(LowerStop::Diagnostic)?;
        MIRValue::PlaceRef(place)
    };
    builder.fun_mut().bind_local(subject, subject_value.clone());
    let subject_type = match &condition._type.kind {
        THIRTypeKind::MemoryReference { inner_type, .. } => {
            builder.registry().resolve_type_id(*inner_type)
        }
        _ => &condition._type,
    };
    let variant_match = matches!(&subject_type.kind, THIRTypeKind::TaggedUnion { .. });
    let dispatch_value = if variant_match {
        let sum_type = lower_type(builder, subject_type).map_err(LowerStop::Diagnostic)?;
        let tag_type = builder.types_mut().intern(MIRType::new(
            MIRTypeKind::Integer {
                ty: MIRIntType::I8,
                signed: false,
            },
            None,
        ));
        let out = builder.fun_mut().new_register(tag_type, None);
        builder.fun_mut().emit_intrinsic(
            MIRAggregateIntrinsic::SumIndex {
                out: MIRTarget::Register(out),
                value: subject_value.clone(),
                sum_ty: sum_type,
            },
            condition.token_range.clone(),
        );
        MIRValue::Register(out)
    } else {
        if condition._type.is_memory_reference() {
            let ty = lower_type(builder, subject_type).map_err(LowerStop::Diagnostic)?;
            memory::copy(builder, subject_value.clone(), ty, &condition.token_range)
        } else {
            subject_value.clone()
        }
    };

    let result_type_id = lower_type(builder, result_type).map_err(LowerStop::Diagnostic)?;
    let value_match = !matches!(
        builder
            .types()
            .definition(result_type_id)
            .map(|ty| ty.kind()),
        Some(MIRTypeKind::Void)
    );
    let exit = builder.fun_mut().new_block("match.exit");
    let output = value_match.then(|| builder.fun_mut().block_param(exit, result_type_id, None));
    let blocks = (0..arms.len())
        .map(|_| builder.fun_mut().new_block("match.arm"))
        .collect::<Vec<_>>();
    let binding_block = arms.iter().zip(&blocks).find_map(|((pattern, _), block)| {
        matches!(pattern, THIRPattern::Binding { .. }).then_some(*block)
    });
    let default_block =
        binding_block.unwrap_or_else(|| builder.fun_mut().new_block("match.unreachable"));
    let mut cases = Vec::with_capacity(arms.len());
    for ((pattern, _), block) in arms.iter().zip(&blocks) {
        let value = match pattern {
            THIRPattern::Binding { .. } => continue,
            THIRPattern::Integer(value) if !variant_match => *value as i128,
            THIRPattern::TaggedUnionVariant { variant_index, .. } if variant_match => {
                *variant_index as i128
            }
            THIRPattern::Float(_, _) => {
                return log_mir_error(
                    &condition.token_range,
                    (
                        &cx_log::catalogue::mir::INVALID_CONTEXT,
                        ("floating-point patterns".into(), "MIR case branches".into()),
                    ),
                )
                .map_err(LowerStop::Diagnostic);
            }
            _ => unreachable!("match pattern does not match its subject type"),
        };
        cases.push((value, MIRBlockTarget::new(*block)));
    }

    builder.emit(MIRInstruction::new(
        MIRInstructionKind::CaseBranch {
            value: dispatch_value,
            cases,
            default: Some(MIRBlockTarget::new(default_block)),
        },
        condition.token_range.clone(),
    ));

    for ((pattern, body), block) in arms.iter().zip(blocks) {
        builder.fun_mut().set_current_block(block);
        builder.fun_mut().push_control_scope();
        builder
            .fun_mut()
            .current_control_mut()
            .set_yield_target(exit);
        builder.fun_mut().push_scope(body.token_range.clone());
        aggregates::bind_pattern_payload(builder, pattern, subject_value.clone(), &condition._type)
            .map_err(LowerStop::Diagnostic)?;
        let body_result = lower_expression(builder, body);
        if let Ok(body_value) = &body_result {
            if output.is_some() {
                memory::check_block_argument(
                    builder,
                    body_value,
                    result_type_id,
                    &body.token_range,
                )
                .map_err(LowerStop::Diagnostic)?;
            }
        }
        auto_pop_scope(builder)?;
        builder.fun_mut().pop_control_scope();
        match body_result {
            Ok(body_value) if !builder.fun().current_block_terminated() => {
                let args = output.map(|_| vec![body_value]).unwrap_or_default();
                builder.emit(MIRInstruction::new(
                    MIRInstructionKind::Jump {
                        target: MIRBlockTarget::with_args(exit, args),
                    },
                    body.token_range.clone(),
                ));
            }
            Err(LowerStop::Diagnostic(error)) => return Err(LowerStop::Diagnostic(error)),
            _ => {}
        }
    }

    if binding_block.is_none() {
        builder.fun_mut().set_current_block(default_block);
        builder.emit(MIRInstruction::new(
            MIRInstructionKind::Unreachable,
            condition.token_range.clone(),
        ));
    }

    let reaches_exit = builder.fun().body().block_has_predecessor(exit);
    builder.fun_mut().set_current_block(exit);
    if !reaches_exit {
        builder.emit(MIRInstruction::new(
            MIRInstructionKind::Unreachable,
            condition.token_range.clone(),
        ));
        return Err(LowerStop::Diverged);
    }
    Ok(output
        .map(MIRValue::Register)
        .unwrap_or(MIRValue::Constant(MIRConstant::Unit)))
}
