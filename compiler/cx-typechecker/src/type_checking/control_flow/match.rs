use std::collections::HashSet;

use crate::environment::TypeEnvironment;
use crate::symbol::completion::complete_template_input;
use crate::type_checking::coercion::implicit::promotion::std_rval_promotion;
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
    expression::{THIRExpression, THIRExpressionKind, THIRLocalID},
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
        .and_then(|value| value.standard_ready_coerce(env, condition.token_range()))
        .map(|value| resolve_indirect_base(env, value))?;
    let expr_type = expr_value.source_type.clone();
    let condition_owned = expr_value.owned;

    env.push_yield_scope(expected_type.cloned());

    let mut arm_flows = Vec::new();
    let mut match_condition = expr_value.source.clone();
    let subject = THIRLocalID::fresh();
    let mut match_is_exhaustive = false;

    let match_arms = match &expr_type.kind {
        THIRTypeKind::Integer { .. } => {
            match_condition = std_rval_promotion(env, expr_value.source.clone())?;
            let mut result_arms = Vec::new();

            for (pattern, body) in arms {
                let HIRPattern::Integer(pattern_value) = pattern else {
                    return env.log_error(
                        condition.token_range(),
                        "Match pattern must be an integer literal".to_string(),
                    );
                };

                let (body, flow) = typecheck_arm(env, namespace, body, "arm")?;
                arm_flows.push(flow);
                result_arms.push((THIRPattern::Integer(*pattern_value), Box::new(body)));
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
                    local_id: subject,
                },
            };
            let mut result_arms = Vec::new();
            let mut matched_variants = HashSet::new();

            for (pattern, body) in arms {
                let TypeConstructor {
                    union_name,
                    variant_name,
                    template_input,
                    inner_name,
                } = resolve_type_constructor_pattern(env, namespace, condition, pattern)?;

                if expected_union_name != &union_name {
                    return env.log_error(
                        condition.token_range(),
                        format!(
                            "Tagged union variant does not match the type being matched, found '{}', expected '{}'",
                            union_name, expected_union_name
                        ),
                    );
                }
                validate_variant_template_input(
                    env,
                    namespace,
                    &expr_type,
                    template_input.as_ref(),
                    condition,
                )?;

                let Some(variant_id) = variants.iter().position(|field| {
                    field
                        .name()
                        .is_some_and(|name| name == variant_name.as_str())
                }) else {
                    return env.log_error(
                        condition.token_range(),
                        format!(
                            "Variant '{}' not found in tagged union '{}'",
                            variant_name, expected_union_name
                        ),
                    );
                };

                if !matched_variants.insert(variant_id) {
                    return env.log_error(
                        condition.token_range(),
                        format!(
                            "Variant '{}' already matched in this match expression",
                            variant_name
                        ),
                    );
                }

                let variant_type = env
                    .symbols
                    .resolve_type_id(variants[variant_id].ty())
                    .clone();
                let inner_local_id = inner_name.as_ref().map(|_| THIRLocalID::fresh());

                env.push_scope(false, false, body.token_range().clone());
                let body_expr = if let Some(inner_name) = &inner_name {
                    let local_id = inner_local_id.expect("match binding local id");
                    let variant_ref_type = env.symbols.mem_ref_to(variant_type.clone());
                    env.symbols.insert_local_value(
                        QualifiedName::new_raw(inner_name.clone()),
                        THIRExpression {
                            token_range: TokenRange::internal(),
                            kind: THIRExpressionKind::Variable {
                                name: inner_name.clone(),
                                local_id,
                            },
                            _type: variant_ref_type.clone(),
                        },
                    );

                    let body_expr = typecheck_expr(env, namespace, body, None)?
                        .standard_ready_coerce(env, body.token_range())?;
                    if condition_owned {
                        let variant = THIRExpression {
                            token_range: TokenRange::internal(),
                            _type: variant_ref_type,
                            kind: THIRExpressionKind::TaggedUnionGet {
                                value: Box::new(subject_expr.clone()),
                                variant_type: variant_type.clone(),
                                variant_index: variant_id,
                            },
                        };
                        let binding = THIRExpression {
                            token_range: TokenRange::internal(),
                            _type: env.symbols.mem_ref_to(variant_type.clone()),
                            kind: THIRExpressionKind::CreateLocalVariable {
                                name: inner_name.clone(),
                                local_id,
                                _type: variant_type.clone(),
                                initial_value: Some(Box::new(variant)),
                                adopting: true,
                            },
                        };
                        THIRExpression {
                            token_range: TokenRange::internal(),
                            _type: THIRType::unit(),
                            kind: THIRExpressionKind::Block {
                                statements: vec![binding, body_expr],
                                creates_scope: false,
                                yields: false,
                            },
                        }
                    } else {
                        body_expr
                    }
                } else {
                    if variant_type.is_nodrop() {
                        return env.log_error(
                            condition.token_range(),
                            format!(
                                "Variant '{}' of tagged union '{}' has a non-void type, but no inner name was provided in the pattern",
                                variant_name, expected_union_name
                            ),
                        );
                    }
                    typecheck_expr(env, namespace, body, None)?
                        .standard_ready_coerce(env, body.token_range())?
                };
                env.pop_scope()
                    .map_err(|error| env.complete_err(error, body.token_range()))?;

                arm_flows.push(MatchArmFlow {
                    range: body.token_range().clone(),
                    label: "arm",
                    may_fall_through: expr_may_fall_through(&body_expr),
                });
                result_arms.push((
                    THIRPattern::TaggedUnionVariant {
                        sum_type: expr_type.clone(),
                        variant_index: variant_id,
                        inner_name,
                        inner_local_id,
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

    let default_body = match default {
        Some(default_expr) => {
            let (body, flow) = typecheck_arm(env, namespace, default_expr, "default")?;
            arm_flows.push(flow);
            Some(Box::new(body))
        }
        None => None,
    };

    let effects = env
        .pop_scope()
        .map_err(|error| env.complete_err(error, condition.token_range()))?;
    let result_type = effects.yield_type.clone().unwrap_or_else(THIRType::unit);

    if !result_type.is_void() {
        for flow in &arm_flows {
            if flow.may_fall_through {
                return env.log_error(
                    &flow.range,
                    format!(
                        "Value-producing match {} may fall through without yielding a value",
                        flow.label
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
}

fn typecheck_arm(
    env: &mut TypeEnvironment,
    namespace: &EnvironmentNamespace,
    body: &HIRExpression,
    label: &'static str,
) -> CXResult<(THIRExpression, MatchArmFlow)> {
    env.push_scope(false, false, body.token_range().clone());
    let body_expr = typecheck_expr(env, namespace, body, None)?
        .standard_ready_coerce(env, body.token_range())?;
    env.pop_scope()
        .map_err(|error| env.complete_err(error, body.token_range()))?;
    let flow = MatchArmFlow {
        range: body.token_range().clone(),
        label,
        may_fall_through: expr_may_fall_through(&body_expr),
    };
    Ok((body_expr, flow))
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
