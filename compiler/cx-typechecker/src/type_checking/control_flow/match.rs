use cx_log::catalogue::typecheck as catalogue;
use std::collections::HashSet;

use crate::environment::TypeEnvironment;
use crate::symbol::completion::complete_template_input;
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
use cx_namespace::module::NamespacePath;
use cx_namespace::module::QualifiedName;
use cx_thir::thir::{
    contextual_eq::TypeContextEqual,
    data::{THIRType, THIRTypeKind},
    expression::{THIRExpression, THIRExpressionKind, THIRLocalID},
    pattern::THIRPattern,
};
use cx_thir::type_context::THIRTypeContext;
use cx_tokens::TokenRange;
use cx_util::identifier::CXIdent;

pub fn typecheck_match(
    env: &mut TypeEnvironment,
    namespace: &NamespacePath,
    condition: &HIRExpression,
    arms: &[(HIRPattern, HIRExpression)],
    expected_type: Option<&THIRType>,
) -> CXResult<TypecheckResult> {
    let expr_value = typecheck_expr(env, namespace, condition, None)
        .and_then(|value| value.standard_ready_coerce(env, condition.token_range()))
        .map(|value| resolve_indirect_base(env, value))?;
    let expr_type = expr_value.source_type.clone();
    let condition_owned = expr_value.owned;

    env.push_yield_scope(expected_type.cloned());

    let mut arm_flows = Vec::new();
    let match_condition = expr_value.source.clone();
    let subject = THIRLocalID::fresh();
    let mut has_binding = false;
    for (pattern, body) in arms {
        if has_binding {
            return env.log_error(
                body.token_range(),
                &catalogue::UNREACHABLE_MATCH_ARM_AFTER_A_CATCH_ALL_BINDING,
                (),
            );
        }
        has_binding = matches!(pattern, HIRPattern::Binding(_));
    }

    let match_arms = match &expr_type.kind {
        THIRTypeKind::Integer { .. } => {
            let mut matched_values = HashSet::new();
            let mut result_arms = Vec::new();

            for (pattern, body) in arms {
                if let HIRPattern::Binding(name) = pattern {
                    let (pattern, body, flow) =
                        typecheck_arm(env, namespace, body, Some((name, &expr_type)))?;
                    arm_flows.push(flow);
                    result_arms.push((pattern.expect("binding pattern"), Box::new(body)));
                    continue;
                }
                let HIRPattern::Integer(pattern_value) = pattern else {
                    return env.log_error(
                        condition.token_range(),
                        &catalogue::MATCH_PATTERN_MUST_BE_AN_INTEGER_LITERAL_OR_A_CATCH,
                        (),
                    );
                };

                if !matched_values.insert(*pattern_value) {
                    return env.log_error(
                        body.token_range(),
                        &catalogue::INTEGER_VALUE_ALREADY_MATCHED_IN_THIS_MATCH,
                        format!("{}", pattern_value),
                    );
                }
                let (_, body, flow) = typecheck_arm(env, namespace, body, None)?;
                arm_flows.push(flow);
                result_arms.push((THIRPattern::Integer(*pattern_value), Box::new(body)));
            }

            if !has_binding {
                return env.log_error(
                    condition.token_range(),
                    &catalogue::INTEGER_MATCH_MUST_BE_EXHAUSTIVE_ADD_A_CATCH_ALL_BINDING,
                    (),
                );
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
                if matched_variants.len() == variants.len() {
                    return env.log_error(body.token_range(), &catalogue::UNREACHABLE_MATCH_ARM_ALL_TAGGED_UNION_VARIANTS_ARE_ALREADY_COVERED, ());
                }
                if let HIRPattern::Binding(name) = pattern {
                    let (pattern, body, flow) =
                        typecheck_arm(env, namespace, body, Some((name, &expr_type)))?;
                    arm_flows.push(flow);
                    result_arms.push((pattern.expect("binding pattern"), Box::new(body)));
                    continue;
                }
                let TypeConstructor {
                    union_name,
                    variant_name,
                    template_input,
                    inner_name,
                } = resolve_type_constructor_pattern(env, namespace, condition, pattern)?;

                if expected_union_name != &union_name {
                    return env.log_error(
                        condition.token_range(),
                        &catalogue::TAGGED_UNION_VARIANT_DOES_NOT_MATCH_THE_TYPE_BEING_MATCHED,
                        (
                            format!("{}", union_name),
                            format!("{}", expected_union_name),
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
                        &catalogue::VARIANT_NOT_FOUND_IN_TAGGED_UNION,
                        (
                            format!("{}", variant_name),
                            format!("{}", expected_union_name),
                        ),
                    );
                };

                if !matched_variants.insert(variant_id) {
                    return env.log_error(
                        condition.token_range(),
                        &catalogue::VARIANT_ALREADY_MATCHED_IN_THIS_MATCH_EXPRESSION,
                        format!("{}", variant_name),
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
                            &catalogue::VARIANT_OF_TAGGED_UNION_HAS_A_NON_VOID_TYPE_BUT,
                            (
                                format!("{}", variant_name),
                                format!("{}", expected_union_name),
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

            if !has_binding && matched_variants.len() != variants.len() {
                let missing = variants
                    .iter()
                    .enumerate()
                    .filter(|(index, _)| !matched_variants.contains(index))
                    .filter_map(|(_, variant)| variant.name())
                    .collect::<Vec<_>>()
                    .join(", ");
                return env.log_error(
                    condition.token_range(),
                    &catalogue::MATCH_MUST_BE_EXHAUSTIVE_MISSING_VARIANTS_ADD_THE_MISSING_ARMS,
                    format!("{}", missing),
                );
            }
            result_arms
        }
        _ => {
            return env.log_error(
                condition.token_range(),
                &catalogue::MATCH_CONDITION_MUST_BE_AN_INTEGER_OR_TAGGED_UNION_TYPE,
                format!("{}", expr_type.display_with(&env.symbols)),
            );
        }
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
                    &catalogue::VALUE_PRODUCING_MATCH_ARM_MAY_FALL_THROUGH_WITHOUT_YIELDING_A,
                    (),
                );
            }
        }
    }

    Ok(TypecheckResult::new(
        result_type,
        THIRExpressionKind::Match {
            condition: Box::new(match_condition),
            subject,
            arms: match_arms,
        },
    ))
}

struct MatchArmFlow {
    range: TokenRange,
    may_fall_through: bool,
}

fn typecheck_arm(
    env: &mut TypeEnvironment,
    namespace: &NamespacePath,
    body: &HIRExpression,
    binding: Option<(&CXIdent, &THIRType)>,
) -> CXResult<(Option<THIRPattern>, THIRExpression, MatchArmFlow)> {
    env.push_scope(false, false, body.token_range().clone());
    let pattern = binding.map(|(name, ty)| {
        let local_id = THIRLocalID::fresh();
        let binding_type = env.symbols.mem_ref_to(ty.clone());
        env.symbols.insert_local_value(
            QualifiedName::new_raw(name.clone()),
            THIRExpression {
                token_range: body.token_range().clone(),
                kind: THIRExpressionKind::Variable {
                    name: name.clone(),
                    local_id,
                },
                _type: binding_type,
            },
        );
        THIRPattern::Binding {
            name: name.clone(),
            local_id,
        }
    });
    let body_expr = typecheck_expr(env, namespace, body, None)?
        .standard_ready_coerce(env, body.token_range())?;
    env.pop_scope()
        .map_err(|error| env.complete_err(error, body.token_range()))?;
    let flow = MatchArmFlow {
        range: body.token_range().clone(),
        may_fall_through: expr_may_fall_through(&body_expr),
    };
    Ok((pattern, body_expr, flow))
}

fn validate_variant_template_input(
    env: &mut TypeEnvironment,
    namespace: &NamespacePath,
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
            &catalogue::NON_TEMPLATED_TAGGED_UNION_PATTERN_MAY_NOT_HAVE_TEMPLATE_ARGUMENTS,
            (),
        );
    };

    if !completed_input.contextual_eq(&template_data.template_input, &env.symbols) {
        return env.log_error(
            condition.token_range(),
            &catalogue::TAGGED_UNION_PATTERN_TEMPLATE_ARGUMENTS_DO_NOT_MATCH_THE_MATCHED,
            (),
        );
    }

    Ok(())
}
