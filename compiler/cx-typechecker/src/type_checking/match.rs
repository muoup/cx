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

pub fn typecheck_switch(
    env: &mut TypeEnvironment,
    base_data: &MIRBaseMappings,
    condition: &CXExpr,
    block: &[CXExpr],
    cases: &[(u64, usize)],
    default_case: Option<&usize>,
) -> CXResult<TypecheckResult> {
    env.push_scope(false, false);

    let condition_value = typecheck_expr(env, base_data, condition, None)
        .and_then(|val| coerce_value(env, condition, val.into_expression()))?;

    env.pop_scope();

    // Build match arms from the cases
    // Each case maps a constant value to a range of expressions in the block
    let mut arms = Vec::new();

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
        env.push_scope(true, false);
        let case_body = typecheck_expr(env, base_data, case_expr, None)?;
        env.pop_scope();

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
            kind: MIRExpressionKind::IntLiteral(*case_value as i64, *_type, *signed),
            _type: MIRType::from(MIRTypeKind::Integer {
                signed: *signed,
                _type: *_type,
            }),
        };

        arms.push((
            Box::new(pattern_expr),
            Box::new(case_body.into_expression()),
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
            env.push_scope(true, false);
            let body = typecheck_expr(env, base_data, expr, None)?;
            env.pop_scope();

            Some(Box::new(body.into_expression()))
        }
        None => None,
    };

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
    env.push_scope(true, false);

    let mut expr_value = typecheck_expr(env, base_data, condition, None)?.into_expression();
    let mut expr_type = expr_value.get_type();

    if let Some(inner) = expr_type.mem_ref_inner() {
        expr_type = inner.clone();

        if !expr_type.is_memory_resident() {
            expr_value = MIRExpression {
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
                    kind: MIRExpressionKind::IntLiteral(*pattern_value, *_type, *signed),
                    _type: MIRType::from(MIRTypeKind::Integer {
                        signed: *signed,
                        _type: *_type,
                    }),
                };

                // Typecheck the body
                env.push_scope(true, false);
                let body_expr = typecheck_expr(env, base_data, body, None)?.into_expression();
                env.pop_scope();

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
                env.push_scope(false, false);
                env.insert_symbol(name.as_string(), variant_value_expr);
                let body_expr = typecheck_expr(env, base_data, body, None)?.into_expression();
                env.pop_scope();

                result_arms.push((Box::new(pattern_expr), Box::new(body_expr)));
            }

            result_arms
        },
        
        _ => {
            return log_typecheck_error!(
                env,
                condition,
                "Match condition must be an integer or tagged union type, found {}",
                expr_type
            );
        },
    };

    // Handle default case
    let default_body = match default {
        Some(default_expr) => {
            env.push_scope(false, false);
            let body = typecheck_expr(env, base_data, default_expr, None)?.into_expression();
            env.pop_scope();
            Some(Box::new(body))
        }
        None => None,
    };

    env.pop_scope();

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
