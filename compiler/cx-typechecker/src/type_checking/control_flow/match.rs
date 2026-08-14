use std::collections::HashSet;

use crate::environment::ScopeArrowSink;
use crate::environment::ScopeExitTarget;
use crate::environment::TypeEnvironment;
use crate::symbol::completion::complete_template_input;
use crate::type_checking::coercion::implicit::promotion::std_rval_promotion;
use crate::type_checking::control_flow::append_current_scope_cleanups;
use crate::type_checking::control_flow::expr_may_fall_through;
use crate::type_checking::pattern::tagged_union::{
    TypeConstructor, resolve_type_constructor_pattern,
};
use crate::type_checking::result::TypecheckResult;
use crate::type_checking::typechecker::typecheck_expr;
use crate::type_checking::value::resolve_indirect_base;
use cx_hir::ast::template::HIRTemplateInput;
use cx_hir::ast::{expression::HIRExpression, pattern::HIRPattern};
use cx_log::CXResult;
use cx_thir::EnvironmentNamespace;
use cx_thir::thir::{
    contextual_eq::TypeContextEqual,
    data::{THIRType, THIRTypeKind},
    expression::{SymbolValueOrigin, THIRExpression, THIRExpressionKind, THIRLocalID},
    pattern::THIRPattern,
};
use cx_thir::type_context::THIRTypeContext;
use cx_tokens::TokenRange;
use cx_util::identifier::CXIdent;
use cx_util::namespace::QualifiedName;

pub fn typecheck_match(
    env: &mut TypeEnvironment,
    namespace: &EnvironmentNamespace,
    condition: &HIRExpression,
    arms: &[(HIRPattern, HIRExpression)],
    default: Option<&HIRExpression>,
    expected_type: Option<&THIRType>,
) -> CXResult<TypecheckResult> {
    let expr_value = typecheck_expr(env, namespace, condition, None)
        .and_then(|v| v.standard_ready_coerce(env, condition.token_range()))
        .map(|v| resolve_indirect_base(env, v))?;
    let expr_type = expr_value.source_type.clone();

    env.push_scope(false, false);
    env.function.set_scope_anchor(condition);
    env.function.configure_merge_scope(condition, None);

    let join_scope_idx = env.function.current_scope_index();
    let base_snapshot = env.function.current_snapshot();
    let base_reachable = env.function.is_current_scope_reachable();
    let condition_owned = expr_value.owned;
    let mut arm_flows = Vec::new();

    env.function
        .push_yield_context(join_scope_idx, expected_type.cloned());

    let mut match_condition = expr_value.source.clone();
    let subject = THIRLocalID::fresh();
    let mut match_is_exhaustive = false;

    let match_arms = match &expr_type.kind {
        THIRTypeKind::Integer { .. } => {
            let expr_value = std_rval_promotion(env, expr_value.source.clone())?;
            match_condition = expr_value;
            // Integer matching: each arm has an integer literal pattern
            let mut result_arms = Vec::new();

            for (pattern, body) in arms.iter() {
                let HIRPattern::Integer(pattern_value) = pattern else {
                    return env.log_error(
                        condition.token_range(),
                        "Match pattern must be an integer literal".to_string(),
                    );
                };

                let (body_expr, flow) = typecheck_match_arm_body(env, namespace, body, "arm")?;
                if flow.may_fall_through {
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
                env.function
                    .set_scope_reachable(join_scope_idx, base_reachable);
                arm_flows.push(flow);

                result_arms.push((THIRPattern::Integer(*pattern_value), Box::new(body_expr)));
            }

            result_arms
        }

        THIRTypeKind::TaggedUnion { variants, .. } => {
            let expected_union_name = expr_type.member_lookup_identifier().unwrap();
            let subject_name = CXIdent::from("__internal_match_subject");

            let subject_expr = THIRExpression {
                _type: expr_value.source._type.clone(),
                token_range: TokenRange::internal(),
                kind: THIRExpressionKind::Variable {
                    name: subject_name,
                    local_id: Some(subject),
                    location: SymbolValueOrigin::Local,
                },
            };

            // Tagged union matching: each arm has a type constructor pattern
            let mut result_arms = Vec::new();
            let mut matched_variants = HashSet::new();

            for (pattern, body) in arms.iter() {
                let TypeConstructor {
                    union_name,
                    variant_name,
                    template_input,
                    inner_name,
                } = resolve_type_constructor_pattern(env, namespace, condition, pattern)?;

                if expected_union_name != &union_name {
                    return env.log_error(condition.token_range(), format!("Tagged union variant does not match the type being matched, found '{}', expected '{}'", union_name, expected_union_name));
                }
                validate_variant_template_input(
                    env,
                    namespace,
                    &expr_type,
                    template_input.as_ref(),
                    condition,
                )?;

                let variant_idx = variants.iter().position(|field| {
                    let Some(name) = field.name() else {
                        return false;
                    };

                    name == variant_name.as_str()
                });

                let Some(variant_id) = variant_idx else {
                    return env.log_error(
                        condition.token_range(),
                        format!(
                            "Variant '{}' not found in tagged union '{}'",
                            variant_name, expected_union_name
                        ),
                    );
                };

                let variant_type = env
                    .symbols
                    .resolve_type_id(variants[variant_id].ty())
                    .clone();

                matched_variants.insert(variant_id);

                let variant_get_type = if condition_owned {
                    variant_type.clone()
                } else {
                    env.symbols.mem_ref_to(variant_type.clone())
                };

                // Extract the variant value and bind it
                let variant_value_expr = THIRExpression {
                    _type: variant_get_type,
                    token_range: TokenRange::internal(),
                    kind: THIRExpressionKind::TaggedUnionGet {
                        value: Box::new(subject_expr.clone()),
                        variant_type: variant_type.clone(),
                        variant_index: variant_id,
                    },
                };

                let body_expr = if let Some(inner_name) = &inner_name {
                    let (body_expr, flow) = if condition_owned {
                        let inner_local_id = THIRLocalID::fresh();
                        let variant_ref_type = env.symbols.mem_ref_to(variant_type.clone());
                        let variant_region = THIRExpression {
                            token_range: TokenRange::internal(),
                            _type: variant_ref_type.clone(),
                            kind: THIRExpressionKind::TaggedUnionGet {
                                value: Box::new(subject_expr.clone()),
                                variant_type: variant_type.clone(),
                                variant_index: variant_id,
                            },
                        };
                        let bind_region = THIRExpression {
                            token_range: TokenRange::internal(),
                            _type: variant_ref_type.clone(),
                            kind: THIRExpressionKind::BindRegion {
                                name: inner_name.clone(),
                                local_id: inner_local_id,
                                _type: variant_type.clone(),
                                initial_region: Box::new(variant_region),
                                adopting: true,
                            },
                        };

                        env.push_scope(false, false);
                        env.function.set_scope_anchor(body);
                        env.symbols.insert_local_value(
                            QualifiedName::root(inner_name.clone()),
                            THIRExpression {
                                token_range: TokenRange::internal(),
                                kind: THIRExpressionKind::Variable {
                                    name: inner_name.clone(),
                                    local_id: Some(inner_local_id),
                                    location: SymbolValueOrigin::Local,
                                },
                                _type: variant_ref_type,
                            },
                        );

                        let (body_expr, flow) =
                            typecheck_match_arm_body(env, namespace, body, "arm")?;
                        env.pop_scope()
                            .map_err(|err| env.complete_err(err, body.token_range()))?;

                        (
                            THIRExpression {
                                token_range: TokenRange::internal(),
                                _type: THIRType::unit(),
                                kind: THIRExpressionKind::Block {
                                    statements: vec![bind_region, body_expr],
                                    creates_scope: false,
                                },
                            },
                            flow,
                        )
                    } else {
                        // Typecheck the body with the borrowed variant value bound.
                        env.push_scope(false, false);
                        env.symbols.insert_local_value(
                            QualifiedName::new_raw(inner_name.clone()),
                            variant_value_expr,
                        );
                        let (body_expr, flow) =
                            typecheck_match_arm_body(env, namespace, body, "arm")?;
                        env.pop_scope()
                            .map_err(|err| env.complete_err(err, body.token_range()))?;
                        (body_expr, flow)
                    };
                    if flow.may_fall_through {
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
                    env.function
                        .set_scope_reachable(join_scope_idx, base_reachable);
                    arm_flows.push(flow);
                    body_expr
                } else {
                    let (body_expr, flow) = typecheck_match_arm_body(env, namespace, body, "arm")?;
                    if flow.may_fall_through {
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
                    env.function
                        .set_scope_reachable(join_scope_idx, base_reachable);
                    arm_flows.push(flow);
                    body_expr
                };

                result_arms.push((
                    THIRPattern::TaggedUnionVariant {
                        sum_type: expr_type.clone(),
                        variant_index: variant_id,
                        inner_name,
                        inner_local_id: None,
                    },
                    Box::new(body_expr),
                ));
            }

            match_is_exhaustive = matched_variants.len() == variants.len();
            result_arms
        }

        _ => {
            return env.log_error(
                condition.token_range(),
                format!(
                    "Match condition must be an integer or tagged union type, found {}",
                    expr_type.display_with(&env.symbols)
                ),
            );
        }
    };

    // Handle default case
    let default_body = match default {
        Some(default_expr) => {
            let (body, flow) = typecheck_match_arm_body(env, namespace, default_expr, "default")?;
            if flow.may_fall_through {
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
            env.function
                .set_scope_reachable(join_scope_idx, base_reachable);
            arm_flows.push(flow);
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

    let yield_context = env.function.pop_yield_context();
    let result_type = yield_context.result_type.unwrap_or_else(THIRType::unit);
    if !result_type.is_unit() {
        for flow in &arm_flows {
            if flow.may_fall_through {
                return env.log_error(
                    &flow.range,
                    format!(
                        "Value-producing match {label} may fall through without yielding a value",
                        label = flow.label
                    ),
                );
            }
        }

        if default.is_none() && !match_is_exhaustive {
            return env.log_error(
                condition.token_range(),
                "Value-producing match must be exhaustive or provide a default arm".to_string(),
            );
        }
    }

    env.pop_scope()
        .map_err(|err| env.complete_err(err, condition.token_range()))?;

    // Build the match expression
    Ok(TypecheckResult::new(
        result_type,
        THIRExpressionKind::Match {
            condition: Box::new(match_condition),
            subject,
            arms: match_arms,
            default: default_body,
            exhaustive: match_is_exhaustive || default.is_some(),
        },
    ))
}

struct MatchArmFlow {
    range: TokenRange,
    label: &'static str,
    may_fall_through: bool,
    #[allow(dead_code)]
    yield_count: usize,
}

fn typecheck_match_arm_body(
    env: &mut TypeEnvironment,
    namespace: &EnvironmentNamespace,
    body: &HIRExpression,
    label: &'static str,
) -> CXResult<(THIRExpression, MatchArmFlow)> {
    env.push_child_defer_scope();
    let yield_count_before = env.function.current_yield_count();
    let body_expr = typecheck_expr(env, namespace, body, None)
        .and_then(|v| v.standard_ready_coerce(env, body.token_range()))?;
    let yield_count = env
        .function
        .current_yield_count()
        .saturating_sub(yield_count_before);
    let body_expr = append_current_scope_cleanups(env, body_expr);
    env.pop_defer_scope();

    Ok((
        body_expr.clone(),
        MatchArmFlow {
            range: body.token_range().clone(),
            label,
            may_fall_through: expr_may_fall_through(&body_expr),
            yield_count,
        },
    ))
}

fn validate_variant_template_input(
    env: &mut TypeEnvironment,
    namespace: &EnvironmentNamespace,
    union_type: &THIRType,
    template_input: Option<&HIRTemplateInput>,
    condition: &HIRExpression,
) -> CXResult<()> {
    let Some(template_input) = template_input else {
        return Ok(());
    };
    let completed_input = complete_template_input(env, namespace, template_input)?;
    let Some(template_data) = union_type.get_template_data() else {
        return env.log_error(
            condition.token_range(),
            "Non-templated tagged union pattern may not have template arguments".to_string(),
        );
    };

    if !completed_input.contextual_eq(&template_data.template_input, &env.symbols) {
        return env.log_error(
            condition.token_range(),
            "Tagged union pattern template arguments do not match the matched type".to_string(),
        );
    }

    Ok(())
}
