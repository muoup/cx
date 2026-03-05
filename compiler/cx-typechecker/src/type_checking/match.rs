use crate::environment::TypeEnvironment;
use crate::log_typecheck_error;
use crate::type_checking::structured_initialization::{
    TypeConstructor, deconstruct_type_constructor,
};
use crate::type_checking::typechecker::{
    control_flow_snapshot, expr_may_fall_through, join_tracked_bindings,
    restore_control_flow_snapshot, typecheck_expr, ControlFlowSnapshot,
};
use crate::type_checking::{accumulation::TypecheckResult, casting::coerce_value};
use cx_ast::ast::{CXExpr, CXExprKind};
use cx_mir::mir::{
    expression::{MIRExpression, MIRExpressionKind},
    program::MIRBaseMappings,
    types::{MIRIntegerType, MIRType, MIRTypeKind},
};
use cx_util::CXResult;

pub fn typecheck_switch(
    env: &mut TypeEnvironment,
    base_data: &MIRBaseMappings,
    condition: &CXExpr,
    block: &[CXExpr],
    cases: &[(u64, usize)],
    default_case: Option<&usize>,
) -> CXResult<TypecheckResult> {
    let outer_snapshot = control_flow_snapshot(env);
    env.push_scope(true, false);
    let condition_value = typecheck_expr(env, base_data, condition, None)
        .and_then(|val| coerce_value(env, condition, val.into_expression()))?;
    let base_snapshot = control_flow_snapshot(env);

    // Build match arms from the cases
    // Each case maps a constant value to a range of expressions in the block
    let mut arms = Vec::new();
    let mut exit_snapshots = Vec::new();

    for (case_index, case_value) in cases {
        // Find the expression at this case index
        let Some(case_expr) = block.get(*case_index as usize) else {
            return log_typecheck_error!(
                env,
                condition,
                "Switch case index {} out of bounds (block has {} expressions)",
                *case_index,
                block.len()
            );
        };

        // Typecheck the case body
        let case_body = typecheck_expr(env, base_data, case_expr, None)?;
        let case_body_expr = case_body.into_expression();
        if expr_may_fall_through(&case_body_expr) {
            exit_snapshots.push(control_flow_snapshot(env));
        }
        restore_control_flow_snapshot(env, &base_snapshot);

        // Create a pattern expression that matches the constant value
        // Use the condition's integer type for the pattern
        let MIRTypeKind::Integer { _type, signed } = &condition_value.get_type().kind else {
            return log_typecheck_error!(
                env,
                condition,
                "Switch condition must be an integer type, found {}",
                condition_value.get_type()
            );
        };

        let pattern_expr = MIRExpression {
            source_range: None,
            kind: MIRExpressionKind::IntLiteral(*case_value as i64, *_type, *signed),
            _type: MIRType::from(MIRTypeKind::Integer {
                signed: *signed,
                _type: *_type,
            }),
        };

        arms.push((
            Box::new(pattern_expr),
            Box::new(case_body_expr),
        ));
    }

    // Handle default case
    let default_body = match default_case {
        Some(&idx) => {
            let Some(expr) = block.get(idx) else {
                return log_typecheck_error!(
                    env,
                    condition,
                    "Switch default case index {} out of bounds (block has {} expressions)",
                    idx,
                    block.len()
                );
            };
            let body = typecheck_expr(env, base_data, expr, None)?;
            let body_expr = body.into_expression();
            if expr_may_fall_through(&body_expr) {
                exit_snapshots.push(control_flow_snapshot(env));
            }
            restore_control_flow_snapshot(env, &base_snapshot);

            Some(Box::new(body_expr))
        }
        None => None,
    };

    if default_case.is_none() {
        exit_snapshots.push(base_snapshot.clone());
    }

    join_tracked_bindings(env, condition, &base_snapshot, &exit_snapshots, "switch join")?;
    let merged_snapshot = control_flow_snapshot(env);
    env.pop_scope();
    restore_control_flow_snapshot(env, &outer_snapshot);
    for (name, _) in outer_snapshot.tracked_bindings.iter() {
        if let Some(binding) = merged_snapshot.tracked_bindings.get(name) {
            env.tracked_bindings.insert(name.clone(), binding.clone());
        }
    }

    // Build the match expression
    Ok(TypecheckResult::expr(
        MIRType::unit(),
        MIRExpressionKind::CSwitch {
            condition: Box::new(condition_value),
            cases: arms,
            default: default_body,
        },
    ))
}

pub fn typecheck_match(
    env: &mut TypeEnvironment,
    base_data: &MIRBaseMappings,
    _: &CXExpr,
    condition: &CXExpr,
    arms: &[(CXExpr, CXExpr)],
    default: Option<&Box<CXExpr>>,
) -> CXResult<TypecheckResult> {
    let mut expr_value = typecheck_expr(env, base_data, condition, None)?.into_expression();
    let mut expr_type = expr_value.get_type();
    let base_snapshot = control_flow_snapshot(env);
    let mut exit_snapshots: Vec<ControlFlowSnapshot> = Vec::new();

    if let Some(inner) = expr_type.mem_ref_inner() {
        expr_type = inner.clone();

        if !expr_type.is_memory_resident() {
            expr_value = MIRExpression {
                source_range: None,
                kind: MIRExpressionKind::MemoryRead {
                    source: Box::new(expr_value),
                },
                _type: expr_type.clone(),
            }
        }
    }

    let match_arms = match &expr_type.kind {
        MIRTypeKind::Integer { .. } => {
            // Integer matching: each arm has an integer literal pattern
            let mut result_arms = Vec::new();

            // Derive integer type from condition
            let MIRTypeKind::Integer { _type, signed } = &expr_type.kind else {
                return log_typecheck_error!(
                    env,
                    condition,
                    "Match condition must be an integer type, found {}",
                    expr_type
                );
            };

            for (pattern, body) in arms.iter() {
                let CXExprKind::IntLiteral {
                    val: pattern_value, ..
                } = &pattern.kind
                else {
                    return log_typecheck_error!(
                        env,
                        pattern,
                        "Match pattern must be an integer literal"
                    );
                };

                // Create a pattern expression that matches this value
                // Use the condition's integer type for the pattern
                let pattern_expr = MIRExpression {
                    source_range: None,
                    kind: MIRExpressionKind::IntLiteral(*pattern_value, *_type, *signed),
                    _type: MIRType::from(MIRTypeKind::Integer {
                        signed: *signed,
                        _type: *_type,
                    }),
                };

                // Typecheck the body
                let body_expr = typecheck_expr(env, base_data, body, None)?.into_expression();
                if expr_may_fall_through(&body_expr) {
                    exit_snapshots.push(control_flow_snapshot(env));
                }
                restore_control_flow_snapshot(env, &base_snapshot);

                result_arms.push((Box::new(pattern_expr), Box::new(body_expr)));
            }

            result_arms
        }

        MIRTypeKind::TaggedUnion {
            name: expected_union_name,
            variants,
        } => {
            // Tagged union matching: each arm has a type constructor pattern
            let mut result_arms = Vec::new();

            for (pattern, body) in arms.iter() {
                let TypeConstructor {
                    union_name,
                    variant_name,
                    inner,
                } = deconstruct_type_constructor(env, pattern)?;

                if union_name.as_str() != expected_union_name.as_str() {
                    return log_typecheck_error!(
                        env,
                        pattern,
                        "Tagged union variant does not match the type being matched"
                    );
                }

                let Some((variant_id, variant_type)) = variants
                    .iter()
                    .enumerate()
                    .find(|(_, (name, _))| name.as_str() == variant_name.as_str())
                    .map(|(id, (_, _type))| (id, _type))
                else {
                    return log_typecheck_error!(
                        env,
                        pattern,
                        "Variant '{}' not found in tagged union '{}'",
                        variant_name,
                        expected_union_name
                    );
                };

                // Create a pattern that matches the tag value
                let pattern_expr = MIRExpression {
                    source_range: None,
                    kind: MIRExpressionKind::IntLiteral(
                        variant_id as i64,
                        MIRIntegerType::I8,
                        false,
                    ),
                    _type: MIRType::from(MIRTypeKind::Integer {
                        signed: false,
                        _type: MIRIntegerType::I8,
                    }),
                };

                // Extract the variant value and bind it
                let variant_value_expr = TypecheckResult::tagged_union_get(
                    TypecheckResult::expr2(expr_value.clone()),
                    variant_type.clone(),
                    variant_type.clone().mem_ref_to(),
                )
                .into_expression();

                let CXExprKind::Identifier(name) = &inner.kind else {
                    return log_typecheck_error!(
                        env,
                        inner,
                        "Tagged union variant pattern must bind to an identifier"
                    );
                };

                // Typecheck the body with the variant value bound
                env.insert_symbol(name.as_string(), variant_value_expr);
                let body_expr = typecheck_expr(env, base_data, body, None)?.into_expression();
                if expr_may_fall_through(&body_expr) {
                    exit_snapshots.push(control_flow_snapshot(env));
                }
                restore_control_flow_snapshot(env, &base_snapshot);

                result_arms.push((Box::new(pattern_expr), Box::new(body_expr)));
            }

            result_arms
        }

        _ => {
            return log_typecheck_error!(
                env,
                condition,
                "Match condition must be an integer or tagged union type, found {}",
                expr_type
            );
        }
    };

    // Handle default case
    let default_body = match default {
        Some(default_expr) => {
            let body = typecheck_expr(env, base_data, default_expr, None)?.into_expression();
            if expr_may_fall_through(&body) {
                exit_snapshots.push(control_flow_snapshot(env));
            }
            restore_control_flow_snapshot(env, &base_snapshot);
            Some(Box::new(body))
        }
        None => None,
    };

    if default.is_none() {
        exit_snapshots.push(base_snapshot.clone());
    }

    join_tracked_bindings(env, condition, &base_snapshot, &exit_snapshots, "match join")?;

    // Build the match expression
    Ok(TypecheckResult::expr(
        MIRType::unit(),
        MIRExpressionKind::Match {
            condition: Box::new(expr_value),
            arms: match_arms,
            default: default_body,
        },
    ))
}
