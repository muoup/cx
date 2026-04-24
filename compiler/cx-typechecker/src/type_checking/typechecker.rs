use std::ops::Deref;

use crate::environment::{
    LoopScopeKind, ScopeArrowSink, ScopeExitTarget, TypeEnvironment,
};
use crate::log_typecheck_error;
use crate::type_checking::aggregate::constructors::typecheck_type_constructor_expr;
use crate::type_checking::aggregate::initialization::typecheck_initializer_list;
use crate::type_checking::coercion::implicit::promotion::std_rval_promotion;
use crate::type_checking::coercion::{explicit::explicit_cast, implicit::implicit_cast};
use crate::type_checking::control_flow::r#return::typecheck_return;
use crate::type_checking::control_flow::{
    enqueue_jump_arrow, expr_may_fall_through, process_for_increment_arrows,
    typecheck_fallthrough_scope,
};
use crate::type_checking::op::binop::access::typecheck_access;
use crate::type_checking::op::binop::assign::typecheck_assignment;
use crate::type_checking::op::binop::calls::typecheck_method_call;
use crate::type_checking::op::binop::is::typecheck_is;
use crate::type_checking::op::typecheck_binop;
use crate::type_checking::result::TypecheckResult;
use crate::type_checking::value::{
    identifiers::{typecheck_identifier, typecheck_templated_identifier},
    literals::{
        typecheck_float_literal, typecheck_int_literal, typecheck_string_literal, typecheck_unit,
    },
    locals::typecheck_var_declaration,
    moves::{typecheck_leak, typecheck_move},
    sizeof::{typecheck_sizeof_expr, typecheck_sizeof_type},
    unsafe_ops::typecheck_unsafe,
};
use cx_ast::ast::{CXBinOp, CXExprKind, CXExpression, CXUnOp};
use cx_mir::mir::data::{MIRIntegerType, MIRTypeKind};
use cx_mir::mir::expression::{MIRExpression, MIRExpressionKind, MIRUnOp};
use cx_mir::mir::program::MIRBaseMappings;
use cx_util::CXResult;

use crate::type_checking::control_flow::r#match::typecheck_match;
use crate::type_checking::control_flow::switch::typecheck_switch;
use cx_mir::mir::data::MIRType;

pub fn typecheck_expr(
    env: &mut TypeEnvironment,
    base_data: &MIRBaseMappings,
    expr: &CXExpression,
    expected_type: Option<&MIRType>,
) -> CXResult<TypecheckResult> {
    typecheck_expr_inner(env, base_data, expr, expected_type)
}

pub fn typecheck_expr_inner(
    env: &mut TypeEnvironment,
    base_data: &MIRBaseMappings,
    expr: &CXExpression,
    expected_type: Option<&MIRType>,
) -> CXResult<TypecheckResult> {
    let mut result = match &expr.kind {
        CXExprKind::Block { exprs } => {
            let mut block = Vec::new();

            for statement in exprs {
                block.push(typecheck_expr(env, base_data, statement, None)?.expression);

                if !env.function.is_current_scope_reachable() {
                    break;
                }
            }

            TypecheckResult::from(MIRExpression {
                token_range: None,
                kind: MIRExpressionKind::Block { statements: block },
                _type: MIRType::unit(),
            })
        }

        CXExprKind::IntLiteral { val, bytes } => typecheck_int_literal(*val, *bytes),

        CXExprKind::FloatLiteral { val, bytes } => typecheck_float_literal(*val, *bytes),

        CXExprKind::StringLiteral { val } => typecheck_string_literal(env, val),

        CXExprKind::VarDeclaration {
            _type,
            name,
            initial_value,
        } => typecheck_var_declaration(env, base_data, expr, _type, name, initial_value.as_ref())?,

        CXExprKind::Identifier(name) => typecheck_identifier(env, base_data, expr, name)?,

        CXExprKind::TemplatedIdentifier {
            name,
            template_input,
        } => typecheck_templated_identifier(env, base_data, expr, name, template_input)?,

        CXExprKind::If {
            condition,
            then_branch,
            else_branch,
        } => {
            let condition_result = typecheck_expr(env, base_data, condition, None)
                .and_then(|v| std_rval_promotion(env, v.into_expression()))?;
            env.function.push_scope(false, false);
            env.function
                .configure_merge_scope(expr, "if join", None, false);
            let join_scope_idx = env.function.current_scope_index();

            let then_result = typecheck_fallthrough_scope(
                env,
                base_data,
                then_branch,
                join_scope_idx,
                ScopeArrowSink::Merge,
                "then",
            )?;

            let else_result = if let Some(else_branch) = else_branch {
                Some(typecheck_fallthrough_scope(
                    env,
                    base_data,
                    else_branch,
                    join_scope_idx,
                    ScopeArrowSink::Merge,
                    "else",
                )?)
            } else {
                env.function.enqueue_scope_arrow(
                    &ScopeExitTarget {
                        target_scope: join_scope_idx,
                        sink: ScopeArrowSink::Merge,
                        label: "else".to_string(),
                    },
                    env.function.current_snapshot(),
                );
                None
            };

            env.function
                .pop_scope(env.source.compilation_unit.as_path())?;

            TypecheckResult::from(MIRExpression {
                token_range: None,
                kind: MIRExpressionKind::If {
                    condition: Box::new(condition_result),
                    then_branch: Box::new(then_result),
                    else_branch: else_result.map(|r| Box::new(r)),
                },
                _type: cx_mir::mir::data::MIRType::unit(),
            })
        }

        CXExprKind::While {
            condition,
            body,
            pre_eval,
        } => {
            env.function.push_scope(true, true);
            env.function.set_scope_anchor(expr);
            env.function
                .configure_loop_scope(expr, LoopScopeKind::While);
            let loop_scope_idx = env.function.current_scope_index();
            env.function.enqueue_scope_arrow(
                &ScopeExitTarget {
                    target_scope: loop_scope_idx,
                    sink: ScopeArrowSink::LoopExit,
                    label: "zero iterations".to_string(),
                },
                env.function.current_snapshot(),
            );

            let condition_result = typecheck_expr(env, base_data, condition, None)
                .and_then(|v| std_rval_promotion(env, v.into_expression()))?;
            let body_result = typecheck_fallthrough_scope(
                env,
                base_data,
                body,
                loop_scope_idx,
                ScopeArrowSink::LoopContinue,
                "loop fallthrough",
            )?;
            env.function
                .pop_scope(env.source.compilation_unit.as_path())?;

            TypecheckResult::from(MIRExpression {
                token_range: None,
                kind: MIRExpressionKind::While {
                    condition: Box::new(condition_result),
                    body: Box::new(body_result),
                    pre_eval: *pre_eval,
                },
                _type: cx_mir::mir::data::MIRType::unit(),
            })
        }

        CXExprKind::For {
            init,
            condition,
            increment,
            body,
        } => {
            env.function.push_scope(true, true);
            env.function.set_scope_anchor(expr);
            let init_result = typecheck_expr(env, base_data, init, None)?.into_expression();
            env.function.configure_loop_scope(expr, LoopScopeKind::For);
            let loop_scope_idx = env.function.current_scope_index();
            env.function.enqueue_scope_arrow(
                &ScopeExitTarget {
                    target_scope: loop_scope_idx,
                    sink: ScopeArrowSink::LoopExit,
                    label: "zero iterations".to_string(),
                },
                env.function.current_snapshot(),
            );

            let condition_result = typecheck_expr(env, base_data, condition, None)
                .and_then(|v| std_rval_promotion(env, v.into_expression()))?;
            let body_result = typecheck_fallthrough_scope(
                env,
                base_data,
                body,
                loop_scope_idx,
                ScopeArrowSink::LoopPendingIncrement,
                "loop fallthrough",
            )?;
            process_for_increment_arrows(env, base_data, loop_scope_idx, increment)?;
            let increment_result =
                typecheck_expr(env, base_data, increment, None)?.into_expression();
            env.function
                .restore_snapshot(&env.function.loop_entry_snapshot(loop_scope_idx));
            env.function.set_scope_reachable(loop_scope_idx, true);
            env.function
                .pop_scope(env.source.compilation_unit.as_path())?;

            TypecheckResult::from(MIRExpression {
                token_range: None,
                kind: MIRExpressionKind::For {
                    init: Box::new(init_result),
                    condition: Box::new(condition_result),
                    increment: Box::new(increment_result),
                    body: Box::new(body_result),
                },
                _type: cx_mir::mir::data::MIRType::unit(),
            })
        }

        CXExprKind::Break => {
            let Some(scope_idx) = env.function.nearest_break_scope() else {
                return log_typecheck_error!(
                    env,
                    Some(expr.token_range()),
                    "'break' used outside of a loop or switch context"
                );
            };
            enqueue_jump_arrow(
                env,
                &ScopeExitTarget {
                    target_scope: scope_idx,
                    sink: env.function.break_arrow_sink(scope_idx),
                    label: "break".to_string(),
                },
            );

            TypecheckResult::from(MIRExpression {
                token_range: None,
                kind: MIRExpressionKind::Break {
                    scope_depth: scope_idx.index(),
                },
                _type: MIRType::unit(),
            })
        }

        CXExprKind::Continue => {
            let Some(scope_idx) = env.function.nearest_continue_scope() else {
                return log_typecheck_error!(
                    env,
                    Some(expr.token_range()),
                    "'continue' used outside of a loop context"
                );
            };
            enqueue_jump_arrow(
                env,
                &ScopeExitTarget {
                    target_scope: scope_idx,
                    sink: env.function.continue_arrow_sink(scope_idx),
                    label: "continue".to_string(),
                },
            );

            TypecheckResult::from(MIRExpression {
                token_range: None,
                kind: MIRExpressionKind::Continue {
                    scope_depth: scope_idx.index(),
                },
                _type: MIRType::unit(),
            })
        }

        CXExprKind::Return { value } => {
            let value = value
                .as_ref()
                .map(|v| Ok(typecheck_expr(env, base_data, v, None)?.into_expression()))
                .transpose()?;
            typecheck_return(env, base_data, value)?
        }

        CXExprKind::Unsafe { expr: inner } => {
            typecheck_unsafe(env, base_data, inner, expected_type)?
        }

        CXExprKind::Leak { expr: inner } => typecheck_leak(env, expr, inner)?,

        CXExprKind::UnOp { operator, operand } => {
            match operator {
                CXUnOp::PreIncrement(increment_amount)
                | CXUnOp::PostIncrement(increment_amount) => {
                    let operand_val =
                        typecheck_expr(env, base_data, operand, None)?.into_expression();
                    let operand_type = operand_val.get_type();

                    let Some(inner) = env.symbols.context.mem_ref_inner(&operand_type).cloned()
                    else {
                        return log_typecheck_error!(
                            env,
                            operand_val.token_range.as_ref(),
                            "Cannot apply pre-increment to non-reference type {}",
                            operand_type
                        );
                    };

                    match &inner.kind {
                        MIRTypeKind::PointerTo { .. } | MIRTypeKind::Integer { .. } => {
                            match operator {
                                CXUnOp::PreIncrement(_) => TypecheckResult::new_base(
                                    operand_type.clone(),
                                    MIRExpressionKind::UnaryOperation {
                                        op: MIRUnOp::PreIncrement(*increment_amount),
                                        operand: Box::new(operand_val),
                                    },
                                ),
                                CXUnOp::PostIncrement(_) => TypecheckResult::new_base(
                                    inner.clone(),
                                    MIRExpressionKind::UnaryOperation {
                                        op: MIRUnOp::PostIncrement(*increment_amount),
                                        operand: Box::new(operand_val),
                                    },
                                ),
                                _ => unreachable!(),
                            }
                        }

                        _ => {
                            return log_typecheck_error!(
                                env,
                                operand_val.token_range.as_ref(),
                                "Pre-increment operator requires an integer or pointer type, found {}",
                                inner
                            );
                        }
                    }
                }

                CXUnOp::LNot => {
                    let operand_val = typecheck_expr(env, base_data, operand, None)
                        .and_then(|v| std_rval_promotion(env, v.into_expression()))
                        .and_then(|v| implicit_cast(env, v, &MIRType::bool()))?;

                    TypecheckResult::new_base(
                        MIRTypeKind::Integer {
                            _type: MIRIntegerType::I1,
                            signed: false,
                        }
                        .into(),
                        MIRExpressionKind::UnaryOperation {
                            operand: Box::new(operand_val),
                            op: MIRUnOp::LNOT,
                        },
                    )
                }

                CXUnOp::BNot => {
                    let operand_val = typecheck_expr(env, base_data, operand, None)
                        .and_then(|v| std_rval_promotion(env, v.into_expression()))?;
                    let op_type = operand_val.get_type();

                    if !op_type.is_integer() {
                        return log_typecheck_error!(
                            env,
                            operand_val.token_range.as_ref(),
                            "Bitwise NOT operator requires an integer type, found {}",
                            op_type
                        );
                    }

                    TypecheckResult::new_base(
                        op_type,
                        MIRExpressionKind::UnaryOperation {
                            operand: Box::new(operand_val),
                            op: MIRUnOp::BNOT,
                        },
                    )
                }

                CXUnOp::Negative => {
                    let operand_val = typecheck_expr(env, base_data, operand, None)
                        .and_then(|v| std_rval_promotion(env, v.into_expression()))?;
                    let operand_type = operand_val.get_type();

                    let operator = match &operand_type.kind {
                        MIRTypeKind::Integer { .. } => MIRUnOp::NEG,
                        MIRTypeKind::Float { .. } => MIRUnOp::FNEG,

                        _ => {
                            return log_typecheck_error!(
                                env,
                                operand_val.token_range.as_ref(),
                                "Negation operator requires an integer or float type, found {}",
                                operand_val
                            );
                        }
                    };

                    TypecheckResult::new_base(
                        operand_type,
                        MIRExpressionKind::UnaryOperation {
                            operand: Box::new(operand_val),
                            op: operator,
                        },
                    )
                }

                CXUnOp::AddressOf => {
                    let operand_val = typecheck_expr(env, base_data, operand, None)?;
                    let operand_type = operand_val.get_type();
                    let Some(inner) = env.symbols.context.mem_ref_inner(&operand_type).cloned()
                    else {
                        return log_typecheck_error!(
                            env,
                            Some(operand.token_range()),
                            "Cannot take the address of a non-reference type"
                        );
                    };

                    // AddressOf just returns the operand (which is a reference) as a pointer
                    TypecheckResult::from(MIRExpression {
                        token_range: None,
                        kind: operand_val.into_expression().kind,
                        _type: env.symbols.context.pointer_to(inner.clone()),
                    })
                }

                CXUnOp::Dereference => {
                    // If the operand is a memory reference of a pointer type, we need to load the value first
                    let loaded_operand = typecheck_expr(env, base_data, operand, None)
                        .and_then(|v| std_rval_promotion(env, v.into_expression()))?;
                    let loaded_operand_type = loaded_operand.get_type();

                    let Some(inner) = env.symbols.context.ptr_inner(&loaded_operand_type).cloned()
                    else {
                        return log_typecheck_error!(
                            env,
                            loaded_operand.token_range.as_ref(),
                            "Cannot dereference non-pointer type {}",
                            loaded_operand_type
                        );
                    };

                    // Dereference returns a memory reference to the inner type
                    TypecheckResult::from(MIRExpression {
                        token_range: None,
                        kind: MIRExpressionKind::Typechange(Box::new(loaded_operand)),
                        _type: env.symbols.context.mem_ref_to(inner),
                    })
                }

                CXUnOp::ExplicitCast(to_type) => {
                    let to_type = env.complete_type(base_data, expr, to_type)?;
                    let operand_val = typecheck_expr(env, base_data, operand, Some(&to_type))?;
                    let (operand_expr, implicit_parameters) =
                        operand_val.decompose_function_expr();

                    TypecheckResult::from(explicit_cast(env, operand_expr, &to_type)?)
                        .with_implicit_parameters(implicit_parameters)
                }
            }
        }

        CXExprKind::BinOp {
            op: CXBinOp::Assign(op),
            lhs,
            rhs,
        } => {
            let lhs = typecheck_expr(env, base_data, lhs, None)?.into_expression();
            let rhs = typecheck_expr(env, base_data, rhs, None)?.into_expression();
            
            typecheck_assignment(env, lhs, rhs, op.as_ref().map(Box::deref))?
        },
        
        CXExprKind::BinOp {
            op: CXBinOp::Is,
            lhs,
            rhs,
        } => typecheck_is(env, base_data, lhs, rhs, expr)?,

        CXExprKind::BinOp {
            op: CXBinOp::Access,
            lhs,
            rhs,
        } => {
            let lhs = typecheck_expr(env, base_data, lhs, None)?.into_expression();

            typecheck_access(env, base_data, lhs, rhs, expr)?
        }

        CXExprKind::BinOp {
            op: CXBinOp::MethodCall,
            lhs,
            rhs,
        } => typecheck_method_call(env, base_data, lhs, rhs, expr)?,

        CXExprKind::BinOp { op, lhs, rhs } => {
            let lhs = typecheck_expr(env, base_data, lhs, None)?.into_expression();
            let rhs = typecheck_expr(env, base_data, rhs, None)?.into_expression();

            typecheck_binop(env, op, lhs, rhs)?
        }

        CXExprKind::Move {
            expr: inner_expr, ..
        } => typecheck_move(env, expr, inner_expr)?,

        CXExprKind::InitializerList { indices } => {
            typecheck_initializer_list(env, base_data, expr, indices, expected_type)?
        }

        CXExprKind::TypeConstructor {
            union_name: type_name,
            variant_name: name,
            inner,
        } => typecheck_type_constructor_expr(env, base_data, expr, type_name, name, inner)?,

        CXExprKind::Unit => typecheck_unit(),

        CXExprKind::SizeOfType { _type } => typecheck_sizeof_type(env, base_data, expr, _type)?,

        CXExprKind::SizeOfExpr { expr } => typecheck_sizeof_expr(env, base_data, expr)?,

        CXExprKind::Switch {
            condition,
            block,
            cases,
            default_case,
        } => typecheck_switch(
            env,
            base_data,
            condition,
            block,
            cases,
            default_case.as_ref(),
        )?,

        CXExprKind::Match {
            condition,
            arms,
            default,
        } => typecheck_match(env, base_data, condition, arms, default.as_ref())?,

        CXExprKind::Taken => {
            unreachable!("Taken expressions should not be present in the typechecker")
        }
    };

    if result.expression.token_range.is_none() {
        result.expression.token_range = Some(expr.range.clone());
    }

    Ok(result)
}

pub fn add_implicit_return(
    env: &mut TypeEnvironment,
    base_data: &MIRBaseMappings,
    expr: MIRExpression,
) -> CXResult<MIRExpression> {
    if !expr_may_fall_through(&expr) {
        return Ok(expr);
    }

    let func = env.current_function().clone();

    let implicit_value = if func.name.as_str() == "main" {
        Some(Box::new(MIRExpression {
            token_range: None,
            kind: MIRExpressionKind::IntLiteral(0, MIRIntegerType::I32, true),
            _type: MIRType::from(MIRTypeKind::Integer {
                _type: MIRIntegerType::I32,
                signed: true,
            }),
        }))
    } else if func.return_type.is_unit() {
        None
    } else {
        return log_typecheck_error!(
            env,
            expr.token_range.as_ref(),
            "Function '{}' with non-void return type must have an explicit return statement",
            func.name
        );
    };

    let ret = typecheck_return(env, base_data, implicit_value.map(|v| *v))?.into_expression();

    Ok(MIRExpression {
        token_range: None,
        kind: MIRExpressionKind::Block {
            statements: vec![expr, ret],
        },
        _type: MIRType::unit(),
    })
}
