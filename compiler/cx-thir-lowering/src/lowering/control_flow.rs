use cx_log::{
    CXResult,
    error::{CXErr, context::CXInternalContext, message::CXStdErrMessage},
};
use cx_mir::{MIRBlockTarget, MIRConstant, MIRInstrKind, MIRScopeID, MIRValue};
use cx_thir::thir::{
    data::{THIRType, THIRTypeKind},
    expression::{THIRBinOp, THIRExpression, THIRIntBinOp, THIRLocalID},
    pattern::THIRPattern,
};
use cx_thir::type_context::THIRTypeContext;

use crate::{
    builder::MIRBuilder,
    lowering::{
        aggregates::{self, move_value},
        comptime, lower_expression, materialize_value,
        types::lower_type,
    },
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

pub fn auto_cleanup(builder: &mut MIRBuilder, to_scope: MIRScopeID) -> CXResult<()> {
    let defers = builder
        .fun()
        .scope_stack()
        .iter()
        .rev()
        .take_while(|scope| scope.id() != to_scope)
        .flat_map(|scope| scope.deferred_expressions().iter().rev().cloned())
        .collect::<Vec<_>>();

    for defer in defers {
        lower_expression(builder, defer.as_ref())?;
    }

    Ok(())
}

pub fn auto_pop_scope(builder: &mut MIRBuilder) -> CXResult<()> {
    let defers = builder
        .fun()
        .current_scope()
        .deferred_expressions()
        .to_vec();

    for defer in defers.into_iter().rev() {
        lower_expression(builder, defer.as_ref())?;
    }

    let _ = builder.fun_mut().pop_scope();
    Ok(())
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

    builder.fun_mut().push_scope(condition.token_range.clone());

    let condition_value = lower_expression(builder, condition)?;
    builder.emit(MIRInstrKind::Branch {
        cond: condition_value,
        true_target: MIRBlockTarget::new(then_block),
        false_target: MIRBlockTarget::new(else_block.unwrap_or(merge)),
    });

    // TODO: This should be strengthened in the future if we want to support Rust style unit-values.
    let yielding = !matches!(result_type.kind, THIRTypeKind::Void);
    let yield_register = yielding
        .then(|| {
            let yield_type = lower_type(builder, result_type)?;
            Ok(builder
                .fun_mut()
                .set_yield_recipient(merge, yield_type))
        })
        .transpose()?;

    builder.fun_mut().set_current_block(then_block);
 
    builder.fun_mut().push_scope(then_branch.token_range.clone());
    if yielding {
        builder.fun_mut().current_scope_mut().set_yield_target(merge);
    }
    lower_scoped(builder, then_branch)?;
    auto_pop_scope(builder)?;

    builder.emit(MIRInstrKind::Jump {
        target: MIRBlockTarget::new(merge),
    });

    if let Some(else_branch) = else_branch {
        builder.fun_mut().set_current_block(else_block.unwrap());

        builder.fun_mut().push_scope(else_branch.token_range.clone());
        if yielding {
            builder.fun_mut().current_scope_mut().set_yield_target(merge);
        }
        lower_expression(builder, else_branch)?;
        auto_pop_scope(builder)?;

        builder.emit(MIRInstrKind::Jump {
            target: MIRBlockTarget::new(merge),
        });
    }

    auto_pop_scope(builder)?;

    builder.fun_mut().set_current_block(merge);
    return Ok(match yield_register {
        Some(reg) => MIRValue::Register(reg),
        None => MIRValue::Constant(MIRConstant::Unit),
    });
}

pub(super) fn lower_short_circuit(
    builder: &mut MIRBuilder<'_>,
    lhs: &THIRExpression,
    rhs: &THIRExpression,
    op: &THIRBinOp,
    result_type: &THIRType,
) -> CXResult<MIRValue> {
    let lhs_value = lower_expression(builder, lhs)?;
    let rhs_block = builder.fun_mut().new_block("logical.rhs");
    let merge_block = builder.fun_mut().new_block("logical.merge");
    let result_type_id = lower_type(builder, result_type)?;
    let result = builder
        .fun_mut()
        .block_param(merge_block, result_type_id, None);
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

    builder.fun_mut().set_current_block(rhs_block);
    let rhs_value = super::lower_expression(builder, rhs)?;
    if !builder.fun().current_block_terminated() {
        builder.emit(MIRInstrKind::Jump {
            target: MIRBlockTarget::with_args(merge_block, vec![rhs_value]),
        });
    }

    builder.fun_mut().set_current_block(merge_block);
    Ok(MIRValue::Register(result))
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

    builder.emit(MIRInstrKind::Jump {
        target: MIRBlockTarget::new(if pre_eval {
            condition_block
        } else {
            body_block
        }),
    });

    builder.fun_mut().set_current_block(condition_block);
    let condition = lower_scoped(builder, condition)?;

    builder.emit(MIRInstrKind::Branch {
        cond: condition,
        true_target: MIRBlockTarget::new(body_block),
        false_target: MIRBlockTarget::new(exit_block),
    });

    builder.fun_mut().set_current_block(body_block);
    builder.fun_mut().push_invisible_scope();
    builder
        .fun_mut()
        .current_scope_mut()
        .set_break_target(exit_block)
        .set_continue_target(condition_block);

    lower_expression(builder, body)?;
    auto_pop_scope(builder)?;

    builder.emit(MIRInstrKind::Jump {
        target: MIRBlockTarget::new(condition_block),
    });

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

    builder.emit(MIRInstrKind::Jump {
        target: MIRBlockTarget::new(condition_block),
    });

    builder.fun_mut().set_current_block(condition_block);
    let condition = lower_expression(builder, condition)?;
    builder.emit(MIRInstrKind::Branch {
        cond: condition,
        true_target: MIRBlockTarget::new(body_block),
        false_target: MIRBlockTarget::new(exit_block),
    });

    builder.fun_mut().set_current_block(body_block);
    builder.fun_mut().push_scope(body.token_range.clone());
    builder
        .fun_mut()
        .current_scope_mut()
        .set_break_target(exit_block)
        .set_continue_target(increment_block);

    lower_expression(builder, body)?;

    auto_pop_scope(builder)?;
    builder.emit(MIRInstrKind::Jump {
        target: MIRBlockTarget::new(increment_block),
    });

    builder.fun_mut().set_current_block(increment_block);
    lower_expression(builder, increment)?;
    builder.emit(MIRInstrKind::Jump {
        target: MIRBlockTarget::new(condition_block),
    });
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

        if !matches!(case_value, MIRConstant::Integer { .. }) {
            return Err(CXErr::new(
                CXStdErrMessage::error(
                    "COMPTIME ERROR",
                    "switch case expression must evaluate to an integer",
                ),
                CXInternalContext::error("MIR switch case did not produce an integer constant"),
            ));
        }

        targets.push((case_value, MIRBlockTarget::new(block)));
        bodies.push(block);
    }

    builder.emit(MIRInstrKind::IntSwitch {
        value,
        cases: targets,
        default: Some(MIRBlockTarget::new(default_block)),
    });

    builder.fun_mut().push_scope(condition.token_range.clone());
    builder.fun_mut().current_scope_mut().set_break_target(exit);

    for ((_, body), block) in cases.iter().zip(bodies) {
        builder.fun_mut().set_current_block(block);
        lower_scoped(builder, body)?;

        builder.emit(MIRInstrKind::Jump {
            target: MIRBlockTarget::new(exit),
        });
    }

    if let Some(default) = default {
        builder.fun_mut().set_current_block(default_block);
        lower_scoped(builder, default)?;
        builder.emit(MIRInstrKind::Jump {
            target: MIRBlockTarget::new(exit),
        });
    }

    auto_pop_scope(builder)?;
    builder.fun_mut().set_current_block(exit);
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
    let subject_value = lower_expression(builder, condition)?;
    let subject_type =
        if let THIRTypeKind::MemoryReference { inner_type, .. } = &condition._type.kind {
            builder.registry().resolve_type_id(*inner_type).clone()
        } else {
            condition._type.clone()
        };
    let variant_match = matches!(subject_type.kind, THIRTypeKind::TaggedUnion { .. });
    let consuming_subject =
        variant_match && !matches!(condition._type.kind, THIRTypeKind::MemoryReference { .. });

    let subject_value = match (variant_match, consuming_subject) {
        (false, _) => subject_value,
        (true, true) => {
            materialize_value(builder, move_value(subject_value)?, &condition._type)?
        }
        (true, false) => materialize_value(builder, subject_value, &condition._type)?,
    };

    builder.fun_mut().bind_local(subject, subject_value.clone());
    let value_match = !matches!(result_type.kind, THIRTypeKind::Void);

    let exit = builder.fun_mut().new_block("match.exit");
    let synthetic_unreachable = default.is_none() && (exhaustive || value_match);
    let default_block = default
        .map(|_| builder.fun_mut().new_block("match.default"))
        .or_else(|| synthetic_unreachable.then(|| builder.fun_mut().new_block("match.unreachable")))
        .unwrap_or(exit);

    let yield_register = if value_match {
        let result_type_id = lower_type(builder, result_type)?;
        Some(builder.fun_mut().set_yield_recipient(exit, result_type_id))
    } else {
        None
    };

    let mut blocks = Vec::with_capacity(arms.len());
    for _ in arms {
        blocks.push(builder.fun_mut().new_block("match.arm"));
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
        let sum_type_id = lower_type(builder, &subject_type)?;
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
            value: subject_value.clone(),
            cases,
            default: default_target,
        });
    }

    builder.fun_mut().push_invisible_scope();
    builder.fun_mut().current_scope_mut().set_yield_target(exit);

    for ((pattern, body), block) in arms.iter().zip(blocks) {
        builder.fun_mut().set_current_block(block);
        builder.fun_mut().push_invisible_scope();
        aggregates::bind_pattern_payload(builder, pattern, subject_value.clone(), &condition._type)?;

        let body_value = lower_expression(builder, body)?;
        auto_pop_scope(builder)?;

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

    if let Some(default) = default {
        builder.fun_mut().set_current_block(default_block);
        let default_value = lower_scoped(builder, default)?;
        if !builder.fun().current_block_terminated() {
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
        builder.fun_mut().set_current_block(default_block);
        builder.emit(MIRInstrKind::Unreachable);
    }

    auto_pop_scope(builder)?;
    builder.fun_mut().set_current_block(exit);

    Ok(yield_register
        .map(MIRValue::Register)
        .unwrap_or(MIRValue::Constant(MIRConstant::Unit)))
}
