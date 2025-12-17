use crate::environment::TypeEnvironment;
use crate::log_typecheck_error;
use crate::type_checking::casting::coerce_value;
use crate::type_checking::typechecker::{typecheck_expr, typecheck_expr_inner};
use cx_parsing_data::ast::{CXExpr, CXExprKind};
use cx_typechecker_data::mir::expression::{MIRInstruction, MIRValue};
use cx_typechecker_data::mir::program::MIRBaseMappings;
use cx_typechecker_data::mir::types::{CXIntegerType, CXType, CXTypeKind};
use cx_util::CXResult;
use cx_util::identifier::CXIdent;

pub fn typecheck_switch(
    env: &mut TypeEnvironment,
    base_data: &MIRBaseMappings,
    condition: &CXExpr,
    block: &[CXExpr],
    cases: &[(u64, usize)],
    default_case: Option<&usize>,
) -> CXResult<MIRValue> {
    env.push_scope(None, None);
    
    let condition_value = typecheck_expr(env, base_data, condition, None)
        .and_then(|val| coerce_value(env, condition, val))?;

    env.pop_scope();

    let (default_block, merge_block) = if default_case.is_some() {
        (
            env.builder.new_named_block_id("default_block"),
            env.builder.new_named_block_id("merge_block"),
        )
    } else {
        let merge_block = env.builder.new_named_block_id("merge_block");
        (merge_block.clone(), merge_block)
    };

    env.push_scope(Some(merge_block.clone()), None);

    let case_blocks = cases
        .iter()
        .map(|_| env.builder.new_named_block_id("case_block"))
        .collect::<Vec<_>>();

    let mut sorted_cases = cases.to_vec();
    sorted_cases.sort_by(|a, b| a.1.cmp(&b.1));

    env.builder.add_instruction(MIRInstruction::JumpTable {
        condition: condition_value,
        targets: sorted_cases
            .iter()
            .enumerate()
            .map(|(i, (case, _))| ((*case), case_blocks[i].clone()))
            .collect(),
        default: default_block.clone(),
    });

    let mut case_iter = sorted_cases.iter().map(|(_, i)| *i);
    let mut case_block_iter = case_blocks.iter();
    let mut next_index = case_iter.next();

    env.push_scope(None, Some(merge_block.clone()));
    
    for (i, expr) in block.iter().enumerate() {
        if let Some(ref mut inner) = next_index
            && *inner == i {
                let case_block = case_block_iter.next().unwrap();
                *inner = case_iter.next().unwrap_or(usize::MAX);
                env.builder.add_and_set_block(case_block.clone());
            }

        if default_case == Some(&i) {
            env.builder.add_and_set_block(default_block.clone());
        }

        typecheck_expr(env, base_data, expr, None)?;
    }
    
    env.pop_scope();

    env.builder.add_jump(merge_block.clone());
    env.builder.add_and_set_block(merge_block);
    env.pop_scope();

    Ok(MIRValue::NULL)
}

enum MatchCondition<'a> {
    Integer(MIRValue),
    TaggedUnionTag {
        tag_value: MIRValue,
        union_name: CXIdent,
        variants: &'a [(String, CXType)],
    },
}

fn get_match_condition_value<'a>(
    env: &mut TypeEnvironment,
    expr: &CXExpr,
    expr_value: MIRValue,
    expr_type: &'a CXType,
) -> CXResult<MatchCondition<'a>> {
    let (is_memory_ref, expr_type) = expr_type
        .mem_ref_inner()
        .map(|t| (true, t))
        .unwrap_or_else(|| (false, expr_type));

    Ok(match (is_memory_ref, &expr_type.kind) {
        (true, CXTypeKind::Integer { .. }) => {
            let coerced_value = coerce_value(env, expr, expr_value)?;
            MatchCondition::Integer(coerced_value)
        }

        (false, CXTypeKind::Integer { .. }) => MatchCondition::Integer(expr_value),

        (
            _,
            CXTypeKind::TaggedUnion {
                name: union_name,
                variants,
            },
        ) => {
            let tag_value = env.builder.new_register();
            env.builder.add_instruction(MIRInstruction::TaggedUnionTag {
                result: tag_value.clone(),
                source: expr_value,
                sum_type: expr_type.clone(),
            });

            MatchCondition::TaggedUnionTag {
                tag_value: MIRValue::Register {
                    register: tag_value,
                    _type: CXType::from(CXTypeKind::Integer {
                        signed: true,
                        _type: CXIntegerType::I8,
                    }),
                },
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
) -> CXResult<MIRValue> {
    env.push_scope(None, None);

    let expr_value = typecheck_expr(env, base_data, condition, None)?;
    let expr_type = expr_value.get_type();

    let condition_tag = get_match_condition_value(env, expr, expr_value.clone(), &expr_type)?;

    let base_block = env.builder.current_block().id.clone();
    let merge_block = env.builder.new_named_block_id("merge_block");
    let default_block = if default.is_some() {
        env.builder.new_named_block_id("default_block")
    } else {
        merge_block.clone()
    };
    let mut targets = Vec::new();

    if let Some(default) = default {
        env.builder
            .add_and_set_block(default_block.clone());
        env.push_scope(None, Some(merge_block.clone()));
        typecheck_expr_inner(env, base_data, default, None)?;
        env.pop_scope();
        env.builder.add_jump(merge_block.clone());
    }

    match condition_tag {
        MatchCondition::Integer(condition_value) => {
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

                let new_block_id = env
                    .builder
                    .new_named_block_id(format!("variant_block_{}", *pattern_value).as_str());
                targets.push((*pattern_value as u64, new_block_id.clone()));

                env.builder.add_and_set_block(new_block_id.clone());
                env.push_scope(None, Some(merge_block.clone()));

                typecheck_expr(env, base_data, body, None)?;

                env.pop_scope();
                env.builder.add_jump(merge_block.clone());
            }

            env.builder.set_block(base_block);
            env.builder.add_instruction(MIRInstruction::JumpTable {
                condition: condition_value,
                targets,
                default: default_block.clone(),
            });
        }

        MatchCondition::TaggedUnionTag {
            tag_value,
            union_name: expected_union_name,
            variants,
        } => {
            for (i, (pattern, _)) in arms.iter().enumerate() {
                let CXExprKind::TypeConstructor {
                    union_name,
                    variant_name,
                    inner,
                } = &pattern.kind
                else {
                    return log_typecheck_error!(
                        env,
                        pattern,
                        "Match pattern must be a tagged union variant constructor"
                    );
                };
                
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

                let new_block_id = env
                    .builder
                    .new_named_block_id(format!("variant_block_{}", variant_name).as_str());
                targets.push((variant_id as u64, new_block_id.clone()));

                env.builder.add_and_set_block(new_block_id);
                env.push_scope(None, Some(merge_block.clone()));

                let variant_value = env.builder.new_register();
                env.builder.add_instruction(MIRInstruction::TaggedUnionGet {
                    result: variant_value.clone(),
                    source: expr_value.clone(),
                    variant_type: variant_type.clone(),
                });
                
                let CXExprKind::Identifier(name) = &inner.kind
                else {
                    return log_typecheck_error!(
                        env,
                        inner,
                        "Tagged union variant pattern must bind to an identifier"
                    );
                };

                env.insert_symbol(
                    name.as_string(),
                    MIRValue::Register {
                        register: variant_value,
                        _type: variant_type.clone().mem_ref_to(),
                    },
                );

                typecheck_expr(env, base_data, &arms[i].1, None)?;

                env.pop_scope();
                env.builder.add_jump(merge_block.clone());
            }

            env.builder.set_block(base_block);
            env.builder.add_instruction(MIRInstruction::JumpTable {
                condition: tag_value,
                targets,
                default: default_block.clone(),
            });
        }
    }

    env.builder.add_and_set_block(merge_block);
    env.pop_scope();

    Ok(MIRValue::NULL)
}
