use crate::environment::TypeEnvironment;
use crate::log_typecheck_error;
use crate::type_checking::structured_initialization::{
    TypeConstructor, deconstruct_type_constructor,
};
use crate::type_checking::typechecker::typecheck_expr;
use crate::type_checking::{accumulation::TypecheckResult, casting::coerce_value};
use cx_parsing_data::ast::{CXExpr, CXExprKind};
use cx_typechecker_data::mir::{
    expression::{MIRExpression, MIRExpressionKind},
    program::MIRBaseMappings,
    types::{CXIntegerType, MIRType, MIRTypeKind},
};
use cx_util::CXResult;
use cx_util::identifier::CXIdent;

pub fn typecheck_switch(
    env: &mut TypeEnvironment,
    base_data: &MIRBaseMappings,
    condition: &CXExpr,
    block: &[CXExpr],
    cases: &[(u64, usize)],
    default_case: Option<&usize>,
) -> CXResult<TypecheckResult> {
    env.push_scope(None, None);

    let condition_value = typecheck_expr(env, base_data, condition, None)
        .and_then(|val| coerce_value(env, condition, val.into_expression()))?;

    env.pop_scope();

    // Build match arms from the cases
    // Each case maps a constant value to a range of expressions in the block
    let mut arms = Vec::new();

    for (case_index, case_value) in cases {
        // Find the expression at this case index
        let case_expr = block
            .get(*case_index as usize)
            .cloned()
            .unwrap_or_else(|| CXExpr::default());

        // Typecheck the case body
        let case_body = typecheck_expr(env, base_data, &case_expr, None)?;

        // Create a pattern expression that matches the constant value
        let pattern_expr = MIRExpression {
            kind: MIRExpressionKind::IntLiteral(*case_value as i64, CXIntegerType::I64, true),
            _type: MIRType::from(MIRTypeKind::Integer {
                signed: true,
                _type: CXIntegerType::I64,
            }),
        };

        arms.push((
            Box::new(pattern_expr),
            Box::new(case_body.into_expression()),
        ));
    }

    // Handle default case
    let default_expr = default_case.and_then(|&idx| block.get(idx).cloned());

    let default_body = match default_expr {
        Some(expr) => {
            let body = typecheck_expr(env, base_data, &expr, None)?;
            Some(Box::new(body.into_expression()))
        }
        None => None,
    };

    // Build the match expression
    Ok(TypecheckResult::standard_expr(
        MIRType::unit(),
        MIRExpressionKind::CSwitch {
            condition: Box::new(condition_value),
            cases: arms,
            default: default_body,
        },
    ))
}

enum MatchCondition<'a> {
    Integer(MIRExpression),
    TaggedUnionTag {
        tag_expr: MIRExpression,
        union_type: MIRType,
        union_name: CXIdent,
        variants: &'a [(String, MIRType)],
    },
}

fn get_match_condition_value<'a>(
    env: &mut TypeEnvironment,
    expr: &CXExpr,
    expr_value: MIRExpression,
    expr_type: &'a MIRType,
) -> CXResult<MatchCondition<'a>> {
    let (is_memory_ref, expr_type) = expr_type
        .mem_ref_inner()
        .map(|t| (true, t))
        .unwrap_or_else(|| (false, expr_type));

    Ok(match (is_memory_ref, &expr_type.kind) {
        (true, MIRTypeKind::Integer { .. }) => {
            let coerced_value = coerce_value(env, expr, expr_value)?;
            MatchCondition::Integer(coerced_value)
        }

        (false, MIRTypeKind::Integer { .. }) => MatchCondition::Integer(expr_value),

        (
            _,
            MIRTypeKind::TaggedUnion {
                name: union_name,
                variants,
            },
        ) => {
            // Extract the tag from the tagged union
            let tag_expr = TypecheckResult::tagged_union_tag(
                TypecheckResult::new(expr_value.clone()),
                expr_type.clone(),
            )
            .into_expression();

            MatchCondition::TaggedUnionTag {
                tag_expr,
                union_type: expr_type.clone(),
                union_name: union_name.clone(),
                variants,
            }
        }

        _ => {
            return log_typecheck_error!(
                env,
                expr,
                "Match condition must be of integer or tagged union type"
            );
        }
    })
}

pub fn typecheck_match(
    env: &mut TypeEnvironment,
    base_data: &MIRBaseMappings,
    expr: &CXExpr,
    condition: &CXExpr,
    arms: &[(CXExpr, CXExpr)],
    default: Option<&Box<CXExpr>>,
) -> CXResult<TypecheckResult> {
    env.push_scope(None, None);

    let expr_value = typecheck_expr(env, base_data, condition, None)?.into_expression();
    let expr_type = expr_value.get_type();

    let condition_tag = get_match_condition_value(env, expr, expr_value.clone(), &expr_type)?;

    let match_arms = match condition_tag {
        MatchCondition::Integer(_) => {
            // Integer matching: each arm has an integer literal pattern
            let mut result_arms = Vec::new();

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
                // For integer matching, we use a comparison expression as the pattern
                let pattern_expr = MIRExpression {
                    kind: MIRExpressionKind::IntLiteral(*pattern_value, CXIntegerType::I64, true),
                    _type: MIRType::from(MIRTypeKind::Integer {
                        signed: true,
                        _type: CXIntegerType::I64,
                    }),
                };

                // Typecheck the body
                env.push_scope(None, None);
                let body_expr = typecheck_expr(env, base_data, body, None)?.into_expression();
                env.pop_scope();

                result_arms.push((Box::new(pattern_expr), Box::new(body_expr)));
            }

            result_arms
        }

        MatchCondition::TaggedUnionTag {
            union_name: expected_union_name,
            variants,
            ..
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
                    kind: MIRExpressionKind::IntLiteral(
                        variant_id as i64,
                        CXIntegerType::I8,
                        false,
                    ),
                    _type: MIRType::from(MIRTypeKind::Integer {
                        signed: false,
                        _type: CXIntegerType::I8,
                    }),
                };

                // Extract the variant value and bind it
                let variant_value_expr = TypecheckResult::tagged_union_get(
                    TypecheckResult::new(expr_value.clone()),
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
                env.push_scope(None, None);
                env.insert_symbol(name.as_string(), variant_value_expr);
                let body_expr = typecheck_expr(env, base_data, body, None)?.into_expression();
                env.pop_scope();

                result_arms.push((Box::new(pattern_expr), Box::new(body_expr)));
            }

            result_arms
        }
    };

    // Handle default case
    let default_body = match default {
        Some(default_expr) => {
            env.push_scope(None, None);
            let body = typecheck_expr(env, base_data, default_expr, None)?.into_expression();
            env.pop_scope();
            Some(Box::new(body))
        }
        None => None,
    };

    env.pop_scope();

    // Build the match expression
    Ok(TypecheckResult::standard_expr(
        MIRType::unit(),
        MIRExpressionKind::Match {
            condition: Box::new(expr_value),
            arms: match_arms,
            default: default_body,
        },
    ))
}
