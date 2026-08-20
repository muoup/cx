use cx_log::CXResult;
use cx_mir::{MIRBlockTarget, MIRConstant, MIRInstrKind, MIRScopeID, MIRValue};
use cx_thir::thir::{
    data::{THIRType, THIRTypeKind},
    expression::{THIRBinOp, THIRExpression, THIRExpressionKind, THIRIntBinOp, THIRLocalID},
    pattern::THIRPattern,
};
use cx_thir::type_context::THIRTypeContext;

use crate::{
    builder::MIRBuilder,
    lowering::{aggregates, lower_expression, types::lower_type},
};

pub(super) fn contains_label(expression: &THIRExpression) -> bool {
    match &expression.kind {
        THIRExpressionKind::Label { .. } => true,
        THIRExpressionKind::Block { statements, .. } => statements.iter().any(contains_label),
        THIRExpressionKind::If {
            then_branch,
            else_branch,
            ..
        } => contains_label(then_branch) || else_branch.as_deref().is_some_and(contains_label),
        THIRExpressionKind::While { body, .. } | THIRExpressionKind::For { body, .. } => {
            contains_label(body)
        }
        THIRExpressionKind::CSwitch { cases, default, .. } => {
            cases.iter().any(|(_, body)| contains_label(body))
                || default.as_deref().is_some_and(contains_label)
        }
        THIRExpressionKind::Match { arms, default, .. } => {
            arms.iter().any(|(_, body)| contains_label(body))
                || default.as_deref().is_some_and(contains_label)
        }
        THIRExpressionKind::Unsafe { expression } => contains_label(expression),
        _ => false,
    }
}

fn move_value(builder: &mut MIRBuilder<'_>, value: MIRValue, ty: &THIRType) -> MIRValue {
    let value = match value {
        MIRValue::Place(place) => MIRValue::Move(place),
        value => value,
    };
    super::materialize_value(builder, value, ty)
}

fn lower_scoped(builder: &mut MIRBuilder<'_>, expression: &THIRExpression) -> CXResult<MIRValue> {
    builder.push_lexical_scope(expression.token_range.clone());
    let result = super::lower_expression(builder, expression);
    let (scope, defers) = builder.pop_lexical_scope();
    if result.is_ok() && !builder.current_block_terminated() {
        lower_scope_exit(builder, scope, &defers)?;
    }
    result
}

fn lower_scoped_value(
    builder: &mut MIRBuilder<'_>,
    expression: &THIRExpression,
    ty: &THIRType,
) -> CXResult<MIRValue> {
    builder.push_lexical_scope(expression.token_range.clone());
    let result = super::lower_expression(builder, expression)?;
    let (scope, defers) = builder.pop_lexical_scope();
    if builder.current_block_terminated() {
        return Ok(result);
    }
    finish_value_cleanup(builder, result, ty, vec![(Some(scope), defers)])
}

fn rvalue(value: MIRValue, ty: &THIRType) -> MIRValue {
    if ty.is_void() {
        return MIRValue::Constant(MIRConstant::Unit);
    }
    if ty.is_memory_reference() {
        return value;
    }
    match value {
        MIRValue::Place(place) => MIRValue::Copy(place),
        value => value,
    }
}

fn lower_defers(builder: &mut MIRBuilder<'_>, defers: &[THIRExpression]) -> CXResult<()> {
    for defer in defers.iter().rev() {
        if builder.current_block_terminated() {
            break;
        }
        super::lower_expression(builder, defer)?;
    }
    Ok(())
}

pub(super) fn lower_scope_exit(
    builder: &mut MIRBuilder<'_>,
    scope: cx_mir::MIRScopeID,
    defers: &[THIRExpression],
) -> CXResult<()> {
    lower_cleanup_step(builder, Some(scope), defers)
}

fn lower_cleanup_step(
    builder: &mut MIRBuilder<'_>,
    scope: Option<MIRScopeID>,
    defers: &[THIRExpression],
) -> CXResult<()> {
    lower_defers(builder, defers)?;
    if let Some(scope) = scope
        && !builder.current_block_terminated()
    {
        builder.emit(MIRInstrKind::ScopeExit { scope });
    }
    Ok(())
}

pub(super) fn finish_value_cleanup(
    builder: &mut MIRBuilder<'_>,
    value: MIRValue,
    ty: &THIRType,
    steps: Vec<(Option<MIRScopeID>, Vec<THIRExpression>)>,
) -> CXResult<MIRValue> {
    let value = if !steps.is_empty() && ty.is_memory_reference() {
        match value {
            MIRValue::Place(place) => {
                let type_id = lower_type(builder, ty);
                let out = builder.register(type_id, None);
                builder.emit(MIRInstrKind::AddressOf { out, place });
                MIRValue::Register(out)
            }
            value => value,
        }
    } else {
        rvalue(value, ty)
    };
    if steps.is_empty() {
        return Ok(value);
    }

    if !matches!(value, MIRValue::Copy(_) | MIRValue::Move(_)) {
        for (scope, defers) in steps {
            if builder.current_block_terminated() {
                break;
            }
            lower_cleanup_step(builder, scope, &defers)?;
        }
        return Ok(value);
    }

    let type_id = lower_type(builder, ty);
    let cleanup_blocks = steps
        .iter()
        .map(|_| {
            let block = builder.new_block("cleanup");
            let parameter = builder.block_param(block, type_id, None);
            (block, parameter)
        })
        .collect::<Vec<_>>();
    builder.emit(MIRInstrKind::Jump {
        target: MIRBlockTarget::with_args(cleanup_blocks[0].0, vec![value]),
    });

    let mut result = None;
    for (index, ((scope, defers), (block, parameter))) in
        steps.into_iter().zip(cleanup_blocks.iter()).enumerate()
    {
        builder.set_current_block(*block);
        lower_cleanup_step(builder, scope, &defers)?;
        if builder.current_block_terminated() {
            continue;
        }
        let value = MIRValue::Register(*parameter);
        if let Some((next, _)) = cleanup_blocks.get(index + 1) {
            builder.emit(MIRInstrKind::Jump {
                target: MIRBlockTarget::with_args(*next, vec![value]),
            });
        } else {
            result = Some(value);
        }
    }
    Ok(result.unwrap_or(MIRValue::Constant(MIRConstant::Undefined)))
}

pub(super) fn cleanup_value_to(
    builder: &mut MIRBuilder<'_>,
    depth: usize,
    value: MIRValue,
    ty: &THIRType,
) -> CXResult<MIRValue> {
    let steps = builder
        .lexical_scope_exits_to(depth)
        .into_iter()
        .map(|(scope, defers)| (Some(scope), defers))
        .collect();
    finish_value_cleanup(builder, value, ty, steps)
}

pub(super) fn cleanup_value_for_return(
    builder: &mut MIRBuilder<'_>,
    value: MIRValue,
    ty: &THIRType,
) -> CXResult<MIRValue> {
    let mut steps = builder
        .lexical_scope_exits_to(1)
        .into_iter()
        .map(|(scope, defers)| (Some(scope), defers))
        .collect::<Vec<_>>();
    let root_defers = builder.root_defers();
    if !root_defers.is_empty() {
        steps.push((None, root_defers));
    }
    finish_value_cleanup(builder, value, ty, steps)
}

pub(super) fn lower_root_defers(builder: &mut MIRBuilder<'_>) -> CXResult<()> {
    let defers = builder.root_defers();
    lower_defers(builder, &defers)
}

pub(super) fn unwind_lexical_scopes_to(builder: &mut MIRBuilder<'_>, depth: usize) -> CXResult<()> {
    for (scope, defers) in builder.lexical_scope_exits_to(depth) {
        if builder.current_block_terminated() {
            break;
        }
        lower_scope_exit(builder, scope, &defers)?;
    }
    Ok(())
}

pub(super) fn lower_if(
    builder: &mut MIRBuilder<'_>,
    condition: &THIRExpression,
    then_branch: &THIRExpression,
    else_branch: Option<&THIRExpression>,
    result_type: &THIRType,
) -> CXResult<MIRValue> {
    if builder.current_block_terminated() {
        let then_block = builder.new_block("if.unreachable.then");
        let else_block = builder.new_block("if.unreachable.else");
        let continuation = builder.new_block("if.unreachable.continuation");
        let value_match = !matches!(result_type.kind, THIRTypeKind::Void);

        builder.set_current_block(then_block);
        if value_match {
            lower_scoped_value(builder, then_branch, result_type)?;
        } else {
            lower_scoped(builder, then_branch)?;
        }

        builder.set_current_block(else_block);
        if let Some(else_branch) = else_branch {
            if value_match {
                lower_scoped_value(builder, else_branch, result_type)?;
            } else {
                lower_scoped(builder, else_branch)?;
            }
        }

        builder.set_current_block(continuation);
        return Ok(if value_match {
            MIRValue::Constant(MIRConstant::Undefined)
        } else {
            MIRValue::Constant(MIRConstant::Unit)
        });
    }

    let condition = super::lower_expression(builder, condition)?;
    let then_block = builder.new_block("if.then");
    let else_block = builder.new_block("if.else");
    let merge_block = builder.new_block("if.merge");
    let result = (!matches!(result_type.kind, THIRTypeKind::Void)).then(|| {
        let type_id = lower_type(builder, result_type);
        builder.block_param(merge_block, type_id, None)
    });
    builder.emit(MIRInstrKind::Branch {
        cond: condition,
        true_target: MIRBlockTarget::new(then_block),
        false_target: MIRBlockTarget::new(else_block),
    });

    let value_match = !matches!(result_type.kind, THIRTypeKind::Void);
    let mut has_incoming = false;
    builder.set_current_block(then_block);
    let then_value = if value_match {
        lower_scoped_value(builder, then_branch, result_type)?
    } else {
        lower_scoped(builder, then_branch)?
    };
    if !builder.current_block_terminated() {
        let args = result.map(|_| vec![then_value.clone()]).unwrap_or_default();
        builder.emit(MIRInstrKind::Jump {
            target: MIRBlockTarget::with_args(merge_block, args),
        });
        has_incoming = true;
    }

    builder.set_current_block(else_block);
    let else_value = else_branch
        .map(|branch| {
            if value_match {
                lower_scoped_value(builder, branch, result_type)
            } else {
                lower_scoped(builder, branch)
            }
        })
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

pub(super) fn lower_short_circuit(
    builder: &mut MIRBuilder<'_>,
    lhs: &THIRExpression,
    rhs: &THIRExpression,
    op: &THIRBinOp,
    result_type: &THIRType,
) -> CXResult<MIRValue> {
    let lhs_value = super::lower_expression(builder, lhs)?;
    let rhs_block = builder.new_block("logical.rhs");
    let merge_block = builder.new_block("logical.merge");
    let result_type_id = lower_type(builder, result_type);
    let result = builder.block_param(merge_block, result_type_id, None);
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
    let rhs_value = super::lower_expression(builder, rhs)?;
    if !builder.current_block_terminated() {
        builder.emit(MIRInstrKind::Jump {
            target: MIRBlockTarget::with_args(merge_block, vec![rhs_value]),
        });
    }

    builder.set_current_block(merge_block);
    Ok(MIRValue::Register(result))
}

pub(super) fn lower_while(
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
    let condition = super::lower_expression(builder, condition)?;
    builder.emit(MIRInstrKind::Branch {
        cond: condition,
        true_target: MIRBlockTarget::new(body_block),
        false_target: MIRBlockTarget::new(exit_block),
    });

    builder.set_current_block(body_block);
    builder.push_contextual_scope(exit_block, Some(condition_block));
    lower_scoped(builder, body)?;
    builder.pop_loop();
    if !builder.current_block_terminated() {
        builder.emit(MIRInstrKind::Jump {
            target: MIRBlockTarget::new(condition_block),
        });
    }
    builder.set_current_block(exit_block);
    Ok(())
}

pub(super) fn lower_for(
    builder: &mut MIRBuilder<'_>,
    init: &THIRExpression,
    condition: &THIRExpression,
    increment: &THIRExpression,
    body: &THIRExpression,
) -> CXResult<()> {
    super::lower_expression(builder, init)?;
    let condition_block = builder.new_block("for.condition");
    let body_block = builder.new_block("for.body");
    let increment_block = builder.new_block("for.increment");
    let exit_block = builder.new_block("for.exit");
    builder.emit(MIRInstrKind::Jump {
        target: MIRBlockTarget::new(condition_block),
    });

    builder.set_current_block(condition_block);
    let condition = super::lower_expression(builder, condition)?;
    builder.emit(MIRInstrKind::Branch {
        cond: condition,
        true_target: MIRBlockTarget::new(body_block),
        false_target: MIRBlockTarget::new(exit_block),
    });

    builder.set_current_block(body_block);
    builder.push_contextual_scope(exit_block, Some(increment_block));
    lower_scoped(builder, body)?;
    builder.pop_loop();
    if !builder.current_block_terminated() {
        builder.emit(MIRInstrKind::Jump {
            target: MIRBlockTarget::new(increment_block),
        });
    }

    builder.set_current_block(increment_block);
    super::lower_expression(builder, increment)?;
    if !builder.current_block_terminated() {
        builder.emit(MIRInstrKind::Jump {
            target: MIRBlockTarget::new(condition_block),
        });
    }
    builder.set_current_block(exit_block);
    Ok(())
}

pub(super) fn lower_switch(
    builder: &mut MIRBuilder<'_>,
    condition: &THIRExpression,
    cases: &[(Box<THIRExpression>, Box<THIRExpression>)],
    default: Option<&THIRExpression>,
) -> CXResult<()> {
    let value = super::lower_expression(builder, condition)?;
    let exit = builder.new_block("switch.exit");
    let default_block = default
        .map(|_| builder.new_block("switch.default"))
        .unwrap_or(exit);
    let mut targets = Vec::with_capacity(cases.len());
    let mut bodies = Vec::with_capacity(cases.len());
    for (case, _) in cases {
        let block = builder.new_block("switch.case");
        targets.push((
            super::aggregates::constant_from_expression(case),
            MIRBlockTarget::new(block),
        ));
        bodies.push(block);
    }
    builder.emit(MIRInstrKind::IntSwitch {
        value,
        cases: targets,
        default: Some(MIRBlockTarget::new(default_block)),
    });

    builder.push_contextual_scope(exit, None);
    for ((_, body), block) in cases.iter().zip(bodies) {
        builder.set_current_block(block);
        lower_scoped(builder, body)?;
        if !builder.current_block_terminated() {
            builder.emit(MIRInstrKind::Jump {
                target: MIRBlockTarget::new(exit),
            });
        }
    }
    if let Some(default) = default {
        builder.set_current_block(default_block);
        lower_scoped(builder, default)?;
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

pub(super) fn lower_match(
    builder: &mut MIRBuilder<'_>,
    condition: &THIRExpression,
    subject: THIRLocalID,
    arms: &[(THIRPattern, Box<THIRExpression>)],
    default: Option<&THIRExpression>,
    exhaustive: bool,
    result_type: &THIRType,
) -> CXResult<MIRValue> {
    let subject_value = super::lower_expression(builder, condition)?;
    let subject_type =
        if let THIRTypeKind::MemoryReference { inner_type, .. } = &condition._type.kind {
            builder.registry().resolve_type_id(*inner_type).clone()
        } else {
            condition._type.clone()
        };
    let variant_match = matches!(subject_type.kind, THIRTypeKind::TaggedUnion { .. });
    let consuming_subject =
        variant_match && !matches!(condition._type.kind, THIRTypeKind::MemoryReference { .. });
    let subject_value = if variant_match {
        if consuming_subject {
            move_value(builder, subject_value, &subject_type)
        } else {
            match subject_value {
                MIRValue::Place(place) | MIRValue::Copy(place) | MIRValue::Move(place) => {
                    MIRValue::Place(place)
                }
                value => value,
            }
        }
    } else {
        match subject_value {
            MIRValue::Place(place) => MIRValue::Copy(place),
            value => value,
        }
    };
    match &subject_value {
        MIRValue::Place(place) => builder.bind_local(subject, *place),
        value => builder.bind_local_value(subject, value.clone()),
    }
    let subject_place = match &subject_value {
        MIRValue::Place(place) => Some(*place),
        _ => None,
    };
    let value_match = !matches!(result_type.kind, THIRTypeKind::Void);

    let exit = builder.new_block("match.exit");
    let synthetic_unreachable = default.is_none() && (exhaustive || value_match);
    let default_block = default
        .map(|_| builder.new_block("match.default"))
        .or_else(|| synthetic_unreachable.then(|| builder.new_block("match.unreachable")))
        .unwrap_or(exit);

    let result_type_id = value_match.then(|| lower_type(builder, result_type));
    builder.push_yield(exit, result_type_id);

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
        let sum_type_id = lower_type(builder, &subject_type);
        builder.emit(MIRInstrKind::VariantSwitch {
            subject: subject_value.clone(),
            sum_type: sum_type_id,
            cases,
            default: default_target,
        });
    } else {
        let cases = arms
            .iter()
            .zip(&blocks)
            .map(|((pattern, _), block)| {
                (
                    aggregates::constant_from_pattern(pattern),
                    MIRBlockTarget::new(*block),
                )
            })
            .collect();
        builder.emit(MIRInstrKind::IntSwitch {
            value: subject_value,
            cases,
            default: default_target,
        });
    }

    builder.push_contextual_scope(exit, None);

    for ((pattern, body), block) in arms.iter().zip(blocks) {
        builder.set_current_block(block);

        builder.push_lexical_scope(body.token_range.clone());
        if let Some(subject_place) = subject_place {
            aggregates::bind_pattern_payload(builder, pattern, subject_place, &condition._type);
        }
        let body_value = lower_expression(builder, body)?;
        let (scope, defers) = builder.pop_lexical_scope();

        if !builder.current_block_terminated() {
            let body_value = if value_match {
                finish_value_cleanup(
                    builder,
                    body_value,
                    result_type,
                    vec![(Some(scope), defers)],
                )?
            } else {
                lower_scope_exit(builder, scope, &defers)?;
                MIRValue::Constant(MIRConstant::Unit)
            };
            builder.emit(MIRInstrKind::Jump {
                target: MIRBlockTarget::with_args(
                    exit,
                    if value_match {
                        vec![body_value]
                    } else {
                        Vec::new()
                    },
                ),
            });
        }
    }

    if let Some(default) = default {
        builder.set_current_block(default_block);
        let default_value = if value_match {
            lower_scoped_value(builder, default, result_type)?
        } else {
            lower_scoped(builder, default)?
        };
        if !builder.current_block_terminated() {
            builder.emit(MIRInstrKind::Jump {
                target: MIRBlockTarget::with_args(
                    exit,
                    if value_match {
                        vec![default_value]
                    } else {
                        Vec::new()
                    },
                ),
            });
        }
    }

    if synthetic_unreachable {
        builder.set_current_block(default_block);
        builder.emit(MIRInstrKind::Unreachable);
    }

    builder.pop_loop();
    let yields = builder.pop_yield();
    builder.set_current_block(exit);

    match yields.result {
        Some(result) => Ok(MIRValue::Register(result)),
        None => Ok(MIRValue::Constant(MIRConstant::Unit)),
    }
}
