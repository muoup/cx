//! Control flow lowering (if, while, for, switch, match, return, block)

use cx_bytecode_data::{
    types::{BCIntegerType, BCType},
    BCInstructionKind, BCValue,
};
use cx_typechecker_data::mir::{
    expression::{MIRExpression, MIRExpressionKind},
    types::MIRType,
};
use cx_util::CXResult;

use crate::builder::BCBuilder;
use super::expressions::lower_expression;

/// Lower an if expression
pub fn lower_if(
    builder: &mut BCBuilder,
    condition: &MIRExpression,
    then_branch: &MIRExpression,
    else_branch: Option<&MIRExpression>,
    _result_type: &MIRType,
) -> CXResult<BCValue> {
    builder.push_scope(None, None);
    let bc_condition = lower_expression(builder, condition)?;
    builder.pop_scope();
    
    let then_block_id = builder.create_block(Some("then"));
    let else_block_id = builder.create_block(Some("else"));
    let merge_block_id = if else_branch.is_some() {
        builder.create_block(Some("if_merge"))
    } else {
        else_block_id.clone()
    };

    builder.add_new_instruction(
        BCInstructionKind::Branch {
            condition: bc_condition,
            true_block: then_block_id.clone(),
            false_block: else_block_id.clone(),
        },
        BCType::unit(),
        false,
    )?;

    builder.set_current_block(then_block_id.clone());
    lower_expression(builder, then_branch)?;

    builder.add_new_instruction(
        BCInstructionKind::Jump {
            target: merge_block_id.clone(),
        },
        BCType::unit(),
        false,
    )?;

    builder.set_current_block(else_block_id.clone());
    if let Some(else_expr) = else_branch {
        lower_expression(builder, else_expr)?;
        builder.add_new_instruction(
            BCInstructionKind::Jump {
                target: merge_block_id.clone(),
            },
            BCType::unit(),
            false,
        )?;
    }

    builder.set_current_block(merge_block_id);
    Ok(BCValue::NULL)
}

/// Lower a while loop
pub fn lower_while(
    builder: &mut BCBuilder,
    condition: &MIRExpression,
    body: &MIRExpression,
    pre_eval: bool,
) -> CXResult<BCValue> {
    let condition_block_id = builder.create_block(Some("[while] condition"));
    let body_block_id = builder.create_block(Some("[while] body"));
    let exit_block_id = builder.create_block(Some("[while] exit"));

    builder.push_scope(Some(condition_block_id.clone()), Some(exit_block_id.clone()));

    if pre_eval {
        builder.add_new_instruction(
            BCInstructionKind::Jump {
                target: condition_block_id.clone(),
            },
            BCType::unit(),
            false,
        )?;
    } else {
        builder.add_new_instruction(
            BCInstructionKind::Jump {
                target: body_block_id.clone(),
            },
            BCType::unit(),
            false,
        )?;
    }

    builder.set_current_block(condition_block_id.clone());
    let bc_condition = lower_expression(builder, condition)?;
    builder.add_new_instruction(
        BCInstructionKind::Branch {
            condition: bc_condition,
            true_block: body_block_id.clone(),
            false_block: exit_block_id.clone(),
        },
        BCType::unit(),
        false,
    )?;

    builder.set_current_block(body_block_id.clone());
    lower_expression(builder, body)?;
    builder.add_new_instruction(
        BCInstructionKind::Jump {
            target: condition_block_id.clone(),
        },
        BCType::unit(),
        false,
    )?;

    builder.set_current_block(exit_block_id);
    builder.pop_scope();

    Ok(BCValue::NULL)
}

/// Lower a for loop
pub fn lower_for(
    builder: &mut BCBuilder,
    init: &MIRExpression,
    condition: &MIRExpression,
    increment: &MIRExpression,
    body: &MIRExpression,
) -> CXResult<BCValue> {
    lower_expression(builder, init)?;

    let condition_block_id = builder.create_block(Some("for_condition"));
    let body_block_id = builder.create_block(Some("for_body"));
    let increment_block_id = builder.create_block(Some("for_increment"));
    let merge_block_id = builder.create_block(Some("for_merge"));

    builder.push_scope(Some(increment_block_id.clone()), Some(merge_block_id.clone()));

    builder.add_new_instruction(
        BCInstructionKind::Jump {
            target: condition_block_id.clone(),
        },
        BCType::unit(),
        false,
    )?;

    builder.set_current_block(condition_block_id.clone());

    let bc_condition = lower_expression(builder, condition)?;

    builder.add_new_instruction(
        BCInstructionKind::Branch {
            condition: bc_condition,
            true_block: body_block_id.clone(),
            false_block: merge_block_id.clone(),
        },
        BCType::unit(),
        false,
    )?;

    builder.set_current_block(body_block_id.clone());
    lower_expression(builder, body)?;
    builder.add_new_instruction(
        BCInstructionKind::Jump {
            target: increment_block_id.clone(),
        },
        BCType::unit(),
        false,
    )?;

    builder.set_current_block(increment_block_id.clone());
    lower_expression(builder, increment)?;
    builder.add_new_instruction(
        BCInstructionKind::Jump {
            target: condition_block_id.clone(),
        },
        BCType::unit(),
        false,
    )?;

    builder.set_current_block(merge_block_id);
    builder.pop_scope();

    Ok(BCValue::NULL)
}

/// Lower a C-style switch statement
pub fn lower_cswitch(
    builder: &mut BCBuilder,
    condition: &MIRExpression,
    cases: &[(Box<MIRExpression>, Box<MIRExpression>)],
    default: Option<&MIRExpression>,
) -> CXResult<BCValue> {
    let bc_condition = lower_expression(builder, condition)?;

    let exit_block_id = builder.create_block(Some("switch_exit"));
    let default_block_id = if default.is_some() {
        builder.create_block(Some("switch_default"))
    } else {
        exit_block_id.clone()
    };

    builder.push_scope(None, Some(exit_block_id.clone()));

    let mut targets = Vec::new();
    let mut case_blocks = Vec::new();

    for (case_value, _) in cases {
        let case_block_id = builder.create_block(Some("switch_case"));
        case_blocks.push(case_block_id.clone());

        if let MIRExpressionKind::IntLiteral(value, _, _) = &case_value.kind {
            targets.push((*value as u64, case_block_id));
        } else {
            panic!("CSwitch case must be an integer literal");
        }
    }

    builder.add_new_instruction(
        BCInstructionKind::JumpTable {
            value: bc_condition,
            targets,
            default: default_block_id.clone(),
        },
        BCType::unit(),
        false,
    )?;

    for (i, (_, case_body)) in cases.iter().enumerate() {
        builder.set_current_block(case_blocks[i].clone());
        lower_expression(builder, case_body)?;

        let next_block = if i + 1 < case_blocks.len() {
            case_blocks[i + 1].clone()
        } else if default.is_some() {
            default_block_id.clone()
        } else {
            exit_block_id.clone()
        };

        builder.add_new_instruction(
            BCInstructionKind::Jump { target: next_block },
            BCType::unit(),
            false,
        )?;
    }

    if let Some(default_expr) = default {
        builder.set_current_block(default_block_id);
        lower_expression(builder, default_expr)?;
        builder.add_new_instruction(
            BCInstructionKind::Jump {
                target: exit_block_id.clone(),
            },
            BCType::unit(),
            false,
        )?;
    }

    builder.set_current_block(exit_block_id);
    builder.pop_scope();

    Ok(BCValue::NULL)
}

/// Lower a match expression (non-fallthrough)
pub fn lower_match(
    builder: &mut BCBuilder,
    condition: &MIRExpression,
    arms: &[(Box<MIRExpression>, Box<MIRExpression>)],
    default: Option<&MIRExpression>,
) -> CXResult<BCValue> {
    let bc_condition = lower_expression(builder, condition)?;

    let exit_block_id = builder.create_block(Some("match_exit"));
    let default_block_id = if default.is_some() {
        builder.create_block(Some("match_default"))
    } else {
        exit_block_id.clone()
    };

    builder.push_scope(None, Some(exit_block_id.clone()));

    let mut targets = Vec::new();
    let mut arm_blocks = Vec::new();

    for (pattern, _) in arms {
        let arm_block_id = builder.create_block(Some("match_arm"));
        arm_blocks.push(arm_block_id.clone());

        if let MIRExpressionKind::IntLiteral(value, _, _) = &pattern.kind {
            targets.push((*value as u64, arm_block_id));
        } else {
            panic!("Match pattern must be an integer literal");
        }
    }

    builder.add_new_instruction(
        BCInstructionKind::JumpTable {
            value: bc_condition,
            targets,
            default: default_block_id.clone(),
        },
        BCType::unit(),
        false,
    )?;

    for (i, (_, arm_body)) in arms.iter().enumerate() {
        builder.set_current_block(arm_blocks[i].clone());
        lower_expression(builder, arm_body)?;
        builder.add_new_instruction(
            BCInstructionKind::Jump {
                target: exit_block_id.clone(),
            },
            BCType::unit(),
            false,
        )?;
    }

    if let Some(default_expr) = default {
        builder.set_current_block(default_block_id);
        lower_expression(builder, default_expr)?;
        builder.add_new_instruction(
            BCInstructionKind::Jump {
                target: exit_block_id.clone(),
            },
            BCType::unit(),
            false,
        )?;
    }

    builder.set_current_block(exit_block_id);
    builder.pop_scope();

    Ok(BCValue::NULL)
}

/// Lower a return statement
pub fn lower_return(builder: &mut BCBuilder, value: Option<&MIRExpression>) -> CXResult<BCValue> {
    let has_return_buffer = builder.current_prototype().temp_buffer.is_some();

    let bc_value = match value {
        Some(v) => Some(lower_expression(builder, v)?),
        None => None,
    };

    if has_return_buffer {
        let return_buffer = BCValue::ParameterRef(0);
        let return_type = builder.current_prototype().temp_buffer.clone().unwrap();

        if let Some(src) = bc_value {
            builder.add_new_instruction(
                BCInstructionKind::Memcpy {
                    dest: return_buffer.clone(),
                    src,
                    size: BCValue::IntImmediate {
                        val: return_type.size() as i64,
                        _type: BCIntegerType::I64,
                    },
                    alignment: return_type.alignment(),
                },
                BCType::unit(),
                false,
            )?;
        }

        builder.add_new_instruction(
            BCInstructionKind::Return {
                value: Some(return_buffer),
            },
            BCType::unit(),
            false,
        )
    } else {
        builder.add_new_instruction(
            BCInstructionKind::Return { value: bc_value },
            BCType::unit(),
            false,
        )
    }
}

/// Lower a block expression
pub fn lower_block(builder: &mut BCBuilder, statements: &[MIRExpression]) -> CXResult<BCValue> {
    builder.push_scope(None, None);

    let mut last_value = BCValue::NULL;

    for stmt in statements {
        last_value = lower_expression(builder, stmt)?;
    }

    builder.pop_scope();

    Ok(last_value)
}
