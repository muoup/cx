use std::collections::HashSet;

use crate::environment::ScopeArrowSink;
use crate::environment::ScopeExitTarget;
use crate::environment::TypeEnvironment;
use crate::environment::symbols::SymbolValueOrigin;
use crate::log_typecheck_error;
use crate::type_checking::coercion::implicit::promotion::std_rval_promotion;
use crate::type_checking::control_flow::expr_may_fall_through;
use crate::type_checking::pattern::tagged_union::{TypeConstructor, deconstruct_type_constructor};
use crate::type_checking::result::TypecheckResult;
use crate::type_checking::typechecker::typecheck_expr;
use cx_ast::ast::{CXExprKind, CXExpression};
use cx_mir::mir::{
    data::{MIRIntegerType, MIRType, MIRTypeKind},
    expression::{MIRExpression, MIRExpressionKind},
    program::MIRBaseMappings,
};
use cx_util::CXResult;

pub fn typecheck_match(
    env: &mut TypeEnvironment,
    base_data: &MIRBaseMappings,
    condition: &CXExpression,
    arms: &[(CXExpression, CXExpression)],
    default: Option<&Box<CXExpression>>,
) -> CXResult<TypecheckResult> {
    let mut expr_value = typecheck_expr(env, base_data, condition, None)
        .and_then(|val| std_rval_promotion(env, val.into_expression()?))?;
    let mut expr_type = expr_value.get_type();

    env.push_scope(false, false);
    env.function.set_scope_anchor(condition);
    env.function
        .configure_merge_scope(condition, "match join", None, false);

    let join_scope_idx = env.function.current_scope_index();
    let base_snapshot = env.function.current_snapshot();
    let mut condition_owned = false;

    match &expr_type.kind {
        MIRTypeKind::MemoryReference { inner_type, .. }
        | MIRTypeKind::PointerTo { inner_type, .. } => {
            expr_type = env
                .get_named_type_definition(*inner_type)
                .expect("Memory reference or pointer inner type should be a named type")
                .clone();

            expr_value = MIRExpression {
                token_range: None,
                kind: MIRExpressionKind::RegionDuplicate {
                    source: Box::new(expr_value),
                },
                _type: expr_type.clone(),
            };
        }
        _ => condition_owned = true
    }

    let mut match_is_exhaustive = false;
    let match_arms = match &expr_type.kind {
        MIRTypeKind::Integer { .. } => {
            // Integer matching: each arm has an integer literal pattern
            let mut result_arms = Vec::new();

            // Derive integer type from condition
            let MIRTypeKind::Integer { _type, signed } = &expr_type.kind else {
                return log_typecheck_error!(
                    env,
                    Some(condition.token_range()),
                    "Match condition must be an integer type, found {}",
                    expr_type.display_with(&env.symbols.context)
                );
            };

            for (pattern, body) in arms.iter() {
                let CXExprKind::IntLiteral {
                    val: pattern_value, ..
                } = &pattern.kind
                else {
                    return log_typecheck_error!(
                        env,
                        Some(pattern.token_range()),
                        "Match pattern must be an integer literal"
                    );
                };

                // Create a pattern expression that matches this value
                // Use the condition's integer type for the pattern
                let pattern_expr = MIRExpression {
                    token_range: None,
                    kind: MIRExpressionKind::IntLiteral(*pattern_value, *_type, *signed),
                    _type: MIRType::from(MIRTypeKind::Integer {
                        signed: *signed,
                        _type: *_type,
                    }),
                };

                let body_expr = typecheck_expr(env, base_data, body, None)?.into_expression()?;
                if expr_may_fall_through(&body_expr) {
                    env.function.enqueue_scope_arrow(
                        &ScopeExitTarget {
                            target_scope: join_scope_idx,
                            sink: crate::environment::ScopeArrowSink::Merge,
                            label: "arm".to_string(),
                        },
                        env.function.current_snapshot(),
                    );
                }
                env.function.restore_snapshot(&base_snapshot);

                result_arms.push((Box::new(pattern_expr), Box::new(body_expr)));
            }

            result_arms
        }

        MIRTypeKind::TaggedUnion { .. } => {
            let expected_union_name = expr_type.get_name().unwrap();
            let variants = expr_type
                .aggregate_fields(&env.symbols.context)
                .expect("Tagged union match requires completed variants")
                .clone();
            // Tagged union matching: each arm has a type constructor pattern
            let mut result_arms = Vec::new();
            let mut matched_variants = HashSet::new();

            for (pattern, body) in arms.iter() {
                let TypeConstructor {
                    union_type,
                    variant_name,
                    inner,
                } = deconstruct_type_constructor(env, base_data, pattern)?;
                let inner = inner.filter(|inner| !matches!(inner.kind, CXExprKind::Unit));

                if union_type.get_name().unwrap().as_str() != expected_union_name.as_str() {
                    return log_typecheck_error!(
                        env,
                        Some(pattern.token_range()),
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
                        Some(pattern.token_range()),
                        "Variant '{}' not found in tagged union '{}'",
                        variant_name,
                        expected_union_name
                    );
                };
                matched_variants.insert(variant_id);

                // Create a pattern that matches the tag value
                let pattern_expr = MIRExpression {
                    token_range: None,
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

                let variant_get_type = if !expr_type.is_memory_reference() {
                    variant_type.clone()
                } else {
                    env.symbols.context.mem_ref_to(variant_type.clone())
                };

                // Extract the variant value and bind it
                let variant_value_expr = TypecheckResult::new_base(
                    variant_get_type,
                    MIRExpressionKind::TaggedUnionGet {
                        value: Box::new(expr_value.clone()),
                        variant_type: variant_type.clone(),
                    },
                )
                .into_expression()?;

                let body_expr = if let Some(inner) = inner {
                    let CXExprKind::Identifier(name) = &inner.kind else {
                        return log_typecheck_error!(
                            env,
                            variant_value_expr.token_range.as_ref(),
                            "Tagged union variant pattern must bind to an identifier"
                        );
                    };

                    let body_expr = if condition_owned {
                        let variant_ref_type = env.symbols.context.mem_ref_to(variant_type.clone());
                        let variant_region = MIRExpression {
                            token_range: None,
                            _type: variant_ref_type.clone(),
                            kind: MIRExpressionKind::TaggedUnionGet {
                                value: Box::new(expr_value.clone()),
                                variant_type: variant_type.clone(),
                            },
                        };
                        let bind_region = MIRExpression {
                            token_range: None,
                            _type: variant_ref_type.clone(),
                            kind: MIRExpressionKind::BindRegion {
                                name: name.name.clone(),
                                _type: variant_type.clone(),
                                initial_region: Box::new(variant_region),
                                adopting: true,
                            },
                        };

                        env.push_scope(false, false);
                        env.function.set_scope_anchor(body);
                        env.symbols.insert_value(
                            name.name.clone(),
                            MIRExpression {
                                token_range: None,
                                kind: MIRExpressionKind::Variable(name.name.clone()),
                                _type: variant_ref_type,
                            },
                            Some(SymbolValueOrigin::Local),
                        );
                        if env.symbols.is_nocopy(variant_type) {
                            env.function
                                .track_binding(name.name.as_string(), env.symbols.is_nodrop(variant_type));
                        }

                        let body_expr = typecheck_expr(env, base_data, body, None)?.into_expression()?;
                        env.pop_scope()?;

                        MIRExpression {
                            token_range: None,
                            _type: MIRType::unit(),
                            kind: MIRExpressionKind::Block {
                                statements: vec![bind_region, body_expr],
                            },
                        }
                    } else {
                        // Typecheck the body with the borrowed variant value bound.
                        env.symbols.push_scope();
                        env.symbols.insert_value(
                            name.name.clone(),
                            variant_value_expr,
                            Some(SymbolValueOrigin::Local),
                        );
                        let body_expr = typecheck_expr(env, base_data, body, None)?.into_expression()?;
                        env.symbols.pop_scope();
                        body_expr
                    };
                    if expr_may_fall_through(&body_expr) {
                        env.function.enqueue_scope_arrow(
                            &ScopeExitTarget {
                                target_scope: join_scope_idx,
                                sink: crate::environment::ScopeArrowSink::Merge,
                                label: "arm".to_string(),
                            },
                            env.function.current_snapshot(),
                        );
                    }
                    env.function.restore_snapshot(&base_snapshot);
                    body_expr
                } else {
                    let body_expr = typecheck_expr(env, base_data, body, None)?.into_expression()?;
                    if expr_may_fall_through(&body_expr) {
                        env.function.enqueue_scope_arrow(
                            &ScopeExitTarget {
                                target_scope: join_scope_idx,
                                sink: crate::environment::ScopeArrowSink::Merge,
                                label: "arm".to_string(),
                            },
                            env.function.current_snapshot(),
                        );
                    }
                    body_expr
                };

                result_arms.push((Box::new(pattern_expr), Box::new(body_expr)));
            }

            match_is_exhaustive = matched_variants.len() == variants.len();
            result_arms
        }

        _ => {
            return log_typecheck_error!(
                env,
                Some(condition.token_range()),
                "Match condition must be an integer or tagged union type, found {}",
                expr_type.display_with(&env.symbols.context)
            );
        }
    };

    // Handle default case
    let default_body = match default {
        Some(default_expr) => {
            let body = typecheck_expr(env, base_data, default_expr, None)?.into_expression()?;
            if expr_may_fall_through(&body) {
                env.function.enqueue_scope_arrow(
                    &ScopeExitTarget {
                        target_scope: join_scope_idx,
                        sink: ScopeArrowSink::Merge,
                        label: "default".to_string(),
                    },
                    env.function.current_snapshot(),
                );
            }
            env.function.restore_snapshot(&base_snapshot);
            Some(Box::new(body))
        }
        None => None,
    };

    if default.is_none() && !match_is_exhaustive {
        env.function.enqueue_scope_arrow(
            &ScopeExitTarget {
                target_scope: join_scope_idx,
                sink: ScopeArrowSink::Merge,
                label: "default".to_string(),
            },
            env.function.current_snapshot(),
        );
    }

    env.pop_scope()?;

    // Build the match expression
    Ok(TypecheckResult::new_base(
        MIRType::unit(),
        MIRExpressionKind::Match {
            condition: Box::new(expr_value),
            arms: match_arms,
            default: default_body,
            exhaustive: match_is_exhaustive || default.is_some(),
        },
    ))
}
