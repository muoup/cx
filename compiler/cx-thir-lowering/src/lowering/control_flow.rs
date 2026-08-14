use cx_log::CXResult;
use cx_mir::{MIRBlockTarget, MIRConstant, MIRInstrKind, MIRValue};
use cx_thir::thir::{
    data::{THIRType, THIRTypeKind},
    expression::{THIRBinOp, THIRExpression, THIRIntBinOp},
    pattern::THIRPattern,
};
use cx_thir::type_context::THIRTypeContext;

use crate::builder::MIRBuilder;

pub(super) fn lower_if(
    builder: &mut MIRBuilder<'_>,
    condition: &THIRExpression,
    then_branch: &THIRExpression,
    else_branch: Option<&THIRExpression>,
    result_type: &THIRType,
) -> CXResult<MIRValue> {
    let condition = super::expression::lower_expression(builder, condition)?;
    let then_block = builder.new_block("if.then");
    let else_block = builder.new_block("if.else");
    let merge_block = builder.new_block("if.merge");
    let result = (!matches!(result_type.kind, THIRTypeKind::Void)).then(|| {
        let type_id = builder.lower_type(result_type);
        builder.block_param(merge_block, type_id, None)
    });
    builder.emit(MIRInstrKind::Branch {
        cond: condition,
        true_target: MIRBlockTarget::new(then_block),
        false_target: MIRBlockTarget::new(else_block),
    });

    let mut has_incoming = false;
    builder.set_current_block(then_block);
    let then_value = super::expression::lower_expression(builder, then_branch)?;
    if !builder.current_block_terminated() {
        let args = result.map(|_| vec![then_value]).unwrap_or_default();
        builder.emit(MIRInstrKind::Jump {
            target: MIRBlockTarget::with_args(merge_block, args),
        });
        has_incoming = true;
    }

    builder.set_current_block(else_block);
    let else_value = else_branch
        .map(|branch| super::expression::lower_expression(builder, branch))
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
    let lhs_value = super::expression::lower_expression(builder, lhs)?;
    let rhs_block = builder.new_block("logical.rhs");
    let merge_block = builder.new_block("logical.merge");
    let result_type_id = builder.lower_type(result_type);
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
    let rhs_value = super::expression::lower_expression(builder, rhs)?;
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
    let condition = super::expression::lower_expression(builder, condition)?;
    builder.emit(MIRInstrKind::Branch {
        cond: condition,
        true_target: MIRBlockTarget::new(body_block),
        false_target: MIRBlockTarget::new(exit_block),
    });

    builder.set_current_block(body_block);
    builder.push_loop(exit_block, Some(condition_block));
    super::expression::lower_expression(builder, body)?;
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
    super::expression::lower_expression(builder, init)?;
    let condition_block = builder.new_block("for.condition");
    let body_block = builder.new_block("for.body");
    let increment_block = builder.new_block("for.increment");
    let exit_block = builder.new_block("for.exit");
    builder.emit(MIRInstrKind::Jump {
        target: MIRBlockTarget::new(condition_block),
    });

    builder.set_current_block(condition_block);
    let condition = super::expression::lower_expression(builder, condition)?;
    builder.emit(MIRInstrKind::Branch {
        cond: condition,
        true_target: MIRBlockTarget::new(body_block),
        false_target: MIRBlockTarget::new(exit_block),
    });

    builder.set_current_block(body_block);
    builder.push_loop(exit_block, Some(increment_block));
    super::expression::lower_expression(builder, body)?;
    builder.pop_loop();
    if !builder.current_block_terminated() {
        builder.emit(MIRInstrKind::Jump {
            target: MIRBlockTarget::new(increment_block),
        });
    }

    builder.set_current_block(increment_block);
    super::expression::lower_expression(builder, increment)?;
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
    let value = super::expression::lower_expression(builder, condition)?;
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

    builder.push_loop(exit, None);
    for ((_, body), block) in cases.iter().zip(bodies) {
        builder.set_current_block(block);
        super::expression::lower_expression(builder, body)?;
        if !builder.current_block_terminated() {
            builder.emit(MIRInstrKind::Jump {
                target: MIRBlockTarget::new(exit),
            });
        }
    }
    if let Some(default) = default {
        builder.set_current_block(default_block);
        super::expression::lower_expression(builder, default)?;
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
    subject: cx_thir::thir::expression::THIRLocalID,
    arms: &[(THIRPattern, Box<THIRExpression>)],
    default: Option<&THIRExpression>,
    exhaustive: bool,
    result_type: &THIRType,
) -> CXResult<MIRValue> {
    let subject_value = super::expression::lower_expression(builder, condition)?;
    let subject_place =
        super::memory::ensure_place(builder, subject_value.clone(), &condition._type);
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
    let value_match = !matches!(result_type.kind, THIRTypeKind::Void);
    if value_match {
        let result_type_id = builder.lower_type(result_type);
        builder.push_yield(exit, result_type_id);
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
                (
                    super::aggregates::constant_from_pattern(pattern),
                    MIRBlockTarget::new(*block),
                )
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
        super::aggregates::bind_pattern_payload(builder, pattern, subject_place, &condition._type);
        let value = super::expression::lower_expression(builder, body)?;
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
        let value = super::expression::lower_expression(builder, default)?;
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

pub(super) fn lower_cleanups(
    builder: &mut MIRBuilder<'_>,
    cleanups: &[THIRExpression],
) -> CXResult<()> {
    for cleanup in cleanups {
        if builder.current_block_terminated() {
            break;
        }
        super::expression::lower_expression(builder, cleanup)?;
    }
    Ok(())
}
