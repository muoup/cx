use std::collections::HashSet;

use crate::environment::ScopeArrowSink;
use crate::environment::ScopeExitTarget;
use crate::environment::TypeEnvironment;
use crate::symbol::completion::complete_template_input;
use crate::type_checking::coercion::implicit::promotion::std_rval_promotion;
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
    expr: &HIRExpression,
    condition: &HIRExpression,
    arms: &[(HIRPattern, HIRExpression)],
    default: Option<&HIRExpression>,
    expected_type: Option<&THIRType>,
) -> CXResult<TypecheckResult> {
    env.push_scope(false, false, expr.token_range().clone());

    let expr_value = typecheck_expr(env, namespace, condition, None)
        .and_then(|v| v.standard_ready_coerce(env, condition.token_range()))
        .map(|v| resolve_indirect_base(env, v))?;
    let expr_type = expr_value.source_type.clone();

    env.push_scope(false, false, expr.token_range().clone());

    let condition_owned = expr_value.owned;
    let mut arm_flows = Vec::new();

    let mut match_condition = expr_value.source.clone();
    let subject = THIRLocalID::fresh();
    let mut match_is_exhaustive = false;

    let match_arms = match &expr_type.kind {
        THIRTypeKind::Integer { .. } => {
            let expr_value = std_rval_promotion(env, expr_value.source.clone())?;
            match_condition = expr_value;

            let mut result_arms = Vec::new();

            for (pattern, body) in arms.iter() {
                let HIRPattern::Integer(pattern_value) = pattern else {
                    return env.log_error(
                        condition.token_range(),
                        "Match pattern must be an integer literal".to_string(),
                    );
                };

                env.push_scope(false, false, body.token_range().clone());
                let body_expr = typecheck_expr(env, namespace, body, None)?;
                env.pop_scope()
                    .map_err(|err| env.complete_err(err, body.token_range()))?;

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
                    local_id: subject,
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

                if template_input.is_some() {
                    return env.log_error(
                        condition.token_range(),
                        "Tagged union pattern may not have template arguments".to_string(),
                    );
                }

                if expected_union_name != &union_name {
                    return env.log_error(condition.token_range(), format!("Tagged union variant does not match the type being matched, found '{}', expected '{}'", union_name, expected_union_name));
                }

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

                if matched_variants.contains(&variant_id) {
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

                matched_variants.insert(variant_id);

                let match_local = THIRLocalID::fresh();

                env.push_scope(false, false, body.token_range().clone());

                if let Some(inner_name) = inner_name.as_ref() {
                    env.symbols.insert_local_value(
                        QualifiedName::new_raw(inner_name.clone()),
                        THIRExpression {
                            token_range: TokenRange::internal(),
                            kind: THIRExpressionKind::Variable {
                                name: inner_name.clone(),
                                local_id: match_local,
                            },
                            _type: variant_type.clone(),
                        },
                    );
                } else if variant_type.is_nodrop() {
                    env.log_error(
                        condition.token_range(),
                        format!(
                            "Variant '{}' of tagged union '{}' has a non-void type, but no inner name was provided in the pattern",
                            variant_name, expected_union_name
                        ),
                    )?;
                }

                let body_expr = typecheck_expr(env, namespace, body, None)?;
                env.pop_scope()
                    .map_err(|err| env.complete_err(err, body.token_range()))?;

                result_arms.push((
                    THIRPattern::TaggedUnionVariant {
                        sum_type: expr_type.clone(),
                        variant_index: variant_id,
                        inner_name: inner_name.clone(),
                        inner_local_id: match_local,
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
    let default_body = default
        .map(|default_expr| {
            env.push_scope(false, false, default_expr.token_range().clone());
            let body_expr = typecheck_expr(env, namespace, default_expr, None);
            env.pop_scope()
                .map_err(|err| env.complete_err(err, default_expr.token_range()))?;
            arm_flows.push(body_expr);
            Ok(Box::new(body_expr))
        })
        .transpose()?;

    let result_type = yield_context.result_type.unwrap_or_else(THIRType::unit);
    if !result_type.is_void() {
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
