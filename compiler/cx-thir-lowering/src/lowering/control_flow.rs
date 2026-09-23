use cx_log::CXResult;
use cx_mir::{
    MIRAggregateIntrinsic, MIRBindable, MIRBlockTarget, MIRConstant, MIRInstructionKind,
    MIRScopeID, MIRTarget, MIRTypeKind, MIRValue, expr::instruction::MIRInvalidationKind,
    ty::interface::MTRegistry,
};
use cx_thir::thir::{
    data::{THIRType, THIRTypeKind},
    expression::{THIRExpression, THIRLocalID},
    pattern::THIRPattern,
};
use cx_thir::type_context::THIRTypeContext;
use cx_tokens::TokenRange;

use crate::{
    builder::MIRBuilder,
    log::log_mir_error,
    lowering::{aggregates, comptime, lower_expression, memory, types::lower_type},
};

pub fn lower_scoped(
    builder: &mut MIRBuilder<'_>,
    expression: &THIRExpression,
) -> CXResult<MIRValue> {
    builder.fun_mut().push_scope(expression.token_range.clone());
    let expr = lower_expression(builder, expression)?;
    auto_pop_scope(builder)?;

    Ok(expr)
}

pub fn auto_cleanup(
    builder: &mut MIRBuilder,
    to_scope: MIRScopeID,
    range: TokenRange,
) -> CXResult<()> {
    auto_cleanup_inner(builder, to_scope, true, range)
}

pub fn auto_cleanup_before(
    builder: &mut MIRBuilder,
    to_scope: MIRScopeID,
    range: TokenRange,
) -> CXResult<()> {
    auto_cleanup_inner(builder, to_scope, false, range)
}

fn auto_cleanup_inner(
    builder: &mut MIRBuilder,
    to_scope: MIRScopeID,
    include_target: bool,
    range: TokenRange,
) -> CXResult<()> {
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
    let result = (|| {
        for (scope, defers) in &pending {
            for defer in defers.iter().rev() {
                lower_expression(builder, defer.as_ref())?;
            }
            emit_scope_end(builder, *scope, range.clone());
        }
        Ok(())
    })();
    result
}

pub fn auto_pop_scope(builder: &mut MIRBuilder) -> CXResult<()> {
    let scope = builder.fun().current_scope_id();
    let range = builder.fun().current_scope_range();

    if !builder.fun().current_block_terminated() {
        let defers = builder
            .fun()
            .current_scope()
            .deferred_expressions()
            .to_vec();
        for defer in defers.into_iter().rev() {
            lower_expression(builder, defer.as_ref())?;
        }
        emit_scope_end(builder, scope, range);
    }

    let _ = builder.fun_mut().pop_scope();
    Ok(())
}

fn emit_scope_end(builder: &mut MIRBuilder, scope: MIRScopeID, range: TokenRange) {
    for place in builder.fun().places_in_scope(scope) {
        builder.emit_if_open(
            MIRInstructionKind::Invalidate {
                place: MIRBindable::Place(place),
                kind: MIRInvalidationKind::Drop,
            },
            range.clone(),
        );
    }
}

pub(super) fn lower_if(
    builder: &mut MIRBuilder<'_>,
    condition: &THIRExpression,
    then_branch: &THIRExpression,
    else_branch: Option<&THIRExpression>,
    result_type: &THIRType,
) -> CXResult<MIRValue> {
    let then_block = builder.fun_mut().new_block("if.then");
    let else_block = if else_branch.is_some() {
        Some(builder.fun_mut().new_block("if.else"))
    } else {
        None
    };
    let merge = builder.fun_mut().new_block("if.merge");
    let result_type_id = lower_type(builder, result_type)?;
    let yielding = !matches!(
        builder
            .types()
            .definition(result_type_id)
            .map(|ty| ty.kind()),
        Some(MIRTypeKind::Void)
    );
    let yield_register =
        yielding.then(|| builder.fun_mut().set_yield_recipient(merge, result_type_id));

    let condition_value = lower_scoped(builder, condition)?;
    builder.emit(
        MIRInstructionKind::Branch {
            cond: condition_value,
            true_target: MIRBlockTarget::new(then_block),
            false_target: MIRBlockTarget::new(else_block.unwrap_or(merge)),
        },
        condition.token_range.clone(),
    );

    builder.fun_mut().set_current_block(then_block);

    builder.fun_mut().push_control_scope();
    if yielding {
        builder
            .fun_mut()
            .current_control_mut()
            .set_yield_target(merge);
    }
    lower_expression(builder, then_branch)?;
    builder.fun_mut().pop_control_scope();
    builder.emit_if_open(
        if yielding {
            MIRInstructionKind::Unreachable
        } else {
            MIRInstructionKind::Jump {
                target: MIRBlockTarget::new(merge),
            }
        },
        then_branch.token_range.clone(),
    );

    if let Some(else_branch) = else_branch {
        builder.fun_mut().set_current_block(else_block.unwrap());

        builder.fun_mut().push_control_scope();
        if yielding {
            builder
                .fun_mut()
                .current_control_mut()
                .set_yield_target(merge);
        }
        lower_expression(builder, else_branch)?;
        builder.fun_mut().pop_control_scope();

        builder.emit_if_open(
            if yielding {
                MIRInstructionKind::Unreachable
            } else {
                MIRInstructionKind::Jump {
                    target: MIRBlockTarget::new(merge),
                }
            },
            else_branch.token_range.clone(),
        );
    }

    builder.fun_mut().set_current_block(merge);
    Ok(match yield_register {
        Some(reg) => MIRValue::Register(reg),
        None => MIRValue::Constant(MIRConstant::Unit),
    })
}

pub(super) fn lower_while(
    builder: &mut MIRBuilder<'_>,
    condition: &THIRExpression,
    body: &THIRExpression,
    pre_eval: bool,
) -> CXResult<()> {
    let condition_block = builder.fun_mut().new_block("while.condition");
    let body_block = builder.fun_mut().new_block("while.body");
    let exit_block = builder.fun_mut().new_block("while.exit");

    builder.emit(
        MIRInstructionKind::Jump {
            target: MIRBlockTarget::new(if pre_eval {
                condition_block
            } else {
                body_block
            }),
        },
        condition.token_range.clone(),
    );

    builder.fun_mut().set_current_block(condition_block);
    let condition = lower_scoped(builder, condition)?;

    builder.emit(
        MIRInstructionKind::Branch {
            cond: condition,
            true_target: MIRBlockTarget::new(body_block),
            false_target: MIRBlockTarget::new(exit_block),
        },
        body.token_range.clone(),
    );

    builder.fun_mut().set_current_block(body_block);
    builder.fun_mut().push_control_scope();
    builder
        .fun_mut()
        .current_control_mut()
        .set_break_target(exit_block)
        .set_continue_target(condition_block);

    lower_expression(builder, body)?;
    builder.fun_mut().pop_control_scope();

    builder.emit_if_open(
        MIRInstructionKind::Jump {
            target: MIRBlockTarget::new(condition_block),
        },
        body.token_range.clone(),
    );

    builder.fun_mut().set_current_block(exit_block);
    Ok(())
}

pub(super) fn lower_for(
    builder: &mut MIRBuilder<'_>,
    init: &THIRExpression,
    condition: &THIRExpression,
    increment: &THIRExpression,
    body: &THIRExpression,
) -> CXResult<()> {
    lower_expression(builder, init)?;

    let condition_block = builder.fun_mut().new_block("for.condition");
    let body_block = builder.fun_mut().new_block("for.body");
    let increment_block = builder.fun_mut().new_block("for.increment");
    let exit_block = builder.fun_mut().new_block("for.exit");

    builder.emit(
        MIRInstructionKind::Jump {
            target: MIRBlockTarget::new(condition_block),
        },
        init.token_range.clone(),
    );

    builder.fun_mut().set_current_block(condition_block);
    let condition = lower_expression(builder, condition)?;
    builder.emit(
        MIRInstructionKind::Branch {
            cond: condition,
            true_target: MIRBlockTarget::new(body_block),
            false_target: MIRBlockTarget::new(exit_block),
        },
        body.token_range.clone(),
    );

    builder.fun_mut().set_current_block(body_block);
    builder.fun_mut().push_control_scope();
    builder
        .fun_mut()
        .current_control_mut()
        .set_break_target(exit_block)
        .set_continue_target(increment_block);

    lower_expression(builder, body)?;

    builder.fun_mut().pop_control_scope();
    builder.emit_if_open(
        MIRInstructionKind::Jump {
            target: MIRBlockTarget::new(increment_block),
        },
        body.token_range.clone(),
    );

    builder.fun_mut().set_current_block(increment_block);
    lower_expression(builder, increment)?;
    builder.emit_if_open(
        MIRInstructionKind::Jump {
            target: MIRBlockTarget::new(condition_block),
        },
        increment.token_range.clone(),
    );
    builder.fun_mut().set_current_block(exit_block);
    Ok(())
}

pub(super) fn lower_switch(
    builder: &mut MIRBuilder<'_>,
    condition: &THIRExpression,
    cases: &[(Box<THIRExpression>, Box<THIRExpression>)],
    default: Option<&THIRExpression>,
) -> CXResult<()> {
    let value = lower_expression(builder, condition)?;
    let exit = builder.fun_mut().new_block("switch.exit");
    let default_block = default
        .map(|_| builder.fun_mut().new_block("switch.default"))
        .unwrap_or(exit);
    let mut targets = Vec::with_capacity(cases.len());
    let mut bodies = Vec::with_capacity(cases.len());

    for (case, _) in cases {
        let block = builder.fun_mut().new_block("switch.case");
        let case_value = comptime::evaluate(builder, case)?;

        let MIRConstant::Integer { value, .. } = case_value else {
            return log_mir_error(
                &case.token_range,
                (
                    &cx_log::catalogue::mir::ENTITY_REQUIREMENT,
                    ("switch case".into(), "an integer constant".into(), None),
                ),
            );
        };

        targets.push((value, MIRBlockTarget::new(block)));
        bodies.push(block);
    }

    builder.emit(
        MIRInstructionKind::CaseBranch {
            value,
            cases: targets,
            default: Some(MIRBlockTarget::new(default_block)),
        },
        condition.token_range.clone(),
    );

    for ((_, body), block) in cases.iter().zip(bodies) {
        builder.fun_mut().set_current_block(block);
        builder.fun_mut().push_control_scope();
        builder
            .fun_mut()
            .current_control_mut()
            .set_break_target(exit);
        builder.fun_mut().push_scope(body.token_range.clone());
        lower_expression(builder, body)?;
        auto_pop_scope(builder)?;
        builder.fun_mut().pop_control_scope();

        builder.emit_if_open(
            MIRInstructionKind::Jump {
                target: MIRBlockTarget::new(exit),
            },
            body.token_range.clone(),
        );
    }

    if let Some(default) = default {
        builder.fun_mut().set_current_block(default_block);
        builder.fun_mut().push_control_scope();
        builder
            .fun_mut()
            .current_control_mut()
            .set_break_target(exit);
        builder.fun_mut().push_scope(default.token_range.clone());
        lower_expression(builder, default)?;
        auto_pop_scope(builder)?;
        builder.fun_mut().pop_control_scope();
        builder.emit_if_open(
            MIRInstructionKind::Jump {
                target: MIRBlockTarget::new(exit),
            },
            default.token_range.clone(),
        );
    }

    builder.fun_mut().set_current_block(exit);
    Ok(())
}

pub(super) fn lower_match(
    builder: &mut MIRBuilder<'_>,
    condition: &THIRExpression,
    subject: THIRLocalID,
    arms: &[(THIRPattern, Box<THIRExpression>)],
    result_type: &THIRType,
) -> CXResult<MIRValue> {
    let subject_value = lower_expression(builder, condition)?;
    builder.fun_mut().bind_local(subject, subject_value.clone());
    let subject_type = match &condition._type.kind {
        THIRTypeKind::MemoryReference { inner_type, .. } => {
            builder.registry().resolve_type_id(*inner_type).clone()
        }
        _ => condition._type.clone(),
    };
    let variant_match = matches!(subject_type.kind, THIRTypeKind::TaggedUnion { .. });
    let dispatch_value = if variant_match {
        let sum_type = lower_type(builder, &subject_type)?;
        let tag_type = lower_type(
            builder,
            &THIRType::from(THIRTypeKind::Integer {
                _type: cx_thir::thir::data::THIRIntType::I8,
                signed: false,
            }),
        )?;
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
        match subject_value.clone() {
            MIRValue::PlaceRef(place) => {
                let ty = lower_type(builder, &subject_type)?;
                memory::copy(builder, place, ty, &condition.token_range)
            }
            value => value,
        }
    };

    let result_type_id = lower_type(builder, result_type)?;
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
                );
            }
            _ => unreachable!("match pattern does not match its subject type"),
        };
        cases.push((value, MIRBlockTarget::new(*block)));
    }

    builder.emit(
        MIRInstructionKind::CaseBranch {
            value: dispatch_value,
            cases,
            default: Some(MIRBlockTarget::new(default_block)),
        },
        condition.token_range.clone(),
    );

    for ((pattern, body), block) in arms.iter().zip(blocks) {
        builder.fun_mut().set_current_block(block);
        builder.fun_mut().push_control_scope();
        builder
            .fun_mut()
            .current_control_mut()
            .set_yield_target(exit);
        builder.fun_mut().push_scope(body.token_range.clone());
        aggregates::bind_pattern_payload(
            builder,
            pattern,
            subject_value.clone(),
            &condition._type,
        )?;
        let body_value = lower_expression(builder, body)?;
        auto_pop_scope(builder)?;
        builder.fun_mut().pop_control_scope();
        let args = output.map(|_| vec![body_value]).unwrap_or_default();
        builder.emit_if_open(
            MIRInstructionKind::Jump {
                target: MIRBlockTarget::with_args(exit, args),
            },
            body.token_range.clone(),
        );
    }

    if binding_block.is_none() {
        builder.fun_mut().set_current_block(default_block);
        builder.emit(
            MIRInstructionKind::Unreachable,
            condition.token_range.clone(),
        );
    }

    builder.fun_mut().set_current_block(exit);
    Ok(output
        .map(MIRValue::Register)
        .unwrap_or(MIRValue::Constant(MIRConstant::Unit)))
}
