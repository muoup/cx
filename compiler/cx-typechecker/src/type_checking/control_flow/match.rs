use std::collections::HashSet;

use crate::environment::ScopeArrowSink;
use crate::environment::ScopeExitTarget;
use crate::environment::TypeEnvironment;
use crate::log_typecheck_error;
use crate::type_checking::coercion::implicit::promotion::std_rval_promotion;
use crate::type_checking::control_flow::expr_may_fall_through;
use crate::type_checking::pattern::tagged_union::{
    TypeConstructor, resolve_type_constructor_pattern,
};
use crate::type_checking::result::TypecheckResult;
use crate::type_checking::typechecker::typecheck_expr;
use cx_ast::ast::{expression::CXExpression, pattern::CXPattern};
use cx_mir::mir::{
    data::{MIRType, MIRTypeKind},
    expression::{MIRExpression, MIRExpressionKind},
    pattern::MIRPattern,
};
use cx_mir::program::EnvironmentNamespace;
use cx_mir::type_context::MIRTypeContext;
use cx_util::CXResult;

pub fn typecheck_match(
    env: &mut TypeEnvironment,
    namespace: &EnvironmentNamespace,
    condition: &CXExpression,
    arms: &[(CXPattern, CXExpression)],
    default: Option<&Box<CXExpression>>,
) -> CXResult<TypecheckResult> {
    let mut expr_value = typecheck_expr(env, namespace, condition, None)
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
        _ => condition_owned = true,
    }

    let mut match_is_exhaustive = false;
    let match_arms = match &expr_type.kind {
        MIRTypeKind::Integer { .. } => {
            // Integer matching: each arm has an integer literal pattern
            let mut result_arms = Vec::new();

            for (pattern, body) in arms.iter() {
                let CXPattern::Integer(pattern_value) = pattern else {
                    return log_typecheck_error!(
                        env,
                        Some(condition.token_range()),
                        "Match pattern must be an integer literal"
                    );
                };

                let body_expr = typecheck_expr(env, namespace, body, None)?.into_expression()?;
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

                result_arms.push((MIRPattern::Integer(*pattern_value), Box::new(body_expr)));
            }

            result_arms
        }

        MIRTypeKind::TaggedUnion { variants, .. } => {
            let expected_union_name = expr_type.get_base_identifier().unwrap();

            // Tagged union matching: each arm has a type constructor pattern
            let mut result_arms = Vec::new();
            let mut matched_variants = HashSet::new();

            for (pattern, body) in arms.iter() {
                let TypeConstructor {
                    union_name,
                    variant_name,
                    inner_name,
                } = resolve_type_constructor_pattern(env, namespace, condition, pattern)?;

                let union_name = union_name.as_flat_name();
                let expected_union_name = expected_union_name.as_str();
                if expected_union_name != union_name
                    && union_name.rsplit("::").next() != Some(expected_union_name)
                {
                    return log_typecheck_error!(
                        env,
                        Some(condition.token_range()),
                        "Tagged union variant does not match the type being matched"
                    );
                }

                let variant_idx = variants.iter().position(|field| {
                    let Some(name) = field.name() else {
                        return false;
                    };

                    name == variant_name.as_str()
                });

                let Some(variant_id) = variant_idx else {
                    return log_typecheck_error!(
                        env,
                        Some(condition.token_range()),
                        "Variant '{}' not found in tagged union '{}'",
                        variant_name,
                        expected_union_name
                    );
                };

                let variant_type = env.symbols.resolve_type_id(variants[variant_id].type_id());

                matched_variants.insert(variant_id);

                let variant_get_type = if !expr_type.is_memory_reference() {
                    variant_type.clone()
                } else {
                    env.symbols.mem_ref_to(variant_type.clone())
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

                let body_expr = if let Some(inner_name) = &inner_name {
                    let body_expr = if condition_owned {
                        let variant_ref_type = env.symbols.mem_ref_to(variant_type.clone());
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
                                name: inner_name.clone(),
                                _type: variant_type.clone(),
                                initial_region: Box::new(variant_region),
                                adopting: true,
                            },
                        };

                        env.push_scope(false, false);
                        env.function.set_scope_anchor(body);
                        env.symbols.insert_value(
                            QualifiedName::root(inner_name.clone()),
                            MIRExpression {
                                token_range: None,
                                kind: MIRExpressionKind::Variable {
                                    name: inner_name.clone(),
                                    location: SymbolVaFlueOrigin::Local,
                                },
                                _type: variant_ref_type,
                            },
                        );
                        if env.symbols.is_nocopy(variant_type) {
                            env.function
                                .track_binding(inner_name.as_string(), variant_type.is_nodrop());
                        }

                        let body_expr =
                            typecheck_expr(env, namespace, body, None)?.into_expression()?;
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
                            inner_name.clone(),
                            variant_value_expr,
                            Some(SymbolValueOrigin::Local),
                        );
                        let body_expr =
                            typecheck_expr(env, namespace, body, None)?.into_expression()?;
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
                    let body_expr =
                        typecheck_expr(env, namespace, body, None)?.into_expression()?;
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

                result_arms.push((
                    MIRPattern::TaggedUnionVariant {
                        sum_type: expr_type.clone(),
                        variant_index: variant_id,
                        inner_name,
                    },
                    Box::new(body_expr),
                ));
            }

            match_is_exhaustive = matched_variants.len() == variants.len();
            result_arms
        }

        _ => {
            return log_typecheck_error!(
                env,
                Some(condition.token_range()),
                "Match condition must be an integer or tagged union type, found {}",
                expr_type.display_with(&env.symbols)
            );
        }
    };

    // Handle default case
    let default_body = match default {
        Some(default_expr) => {
            let body = typecheck_expr(env, namespace, default_expr, None)?.into_expression()?;
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
