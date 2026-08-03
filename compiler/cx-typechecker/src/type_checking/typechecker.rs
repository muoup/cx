use std::ops::Deref;

use crate::environment::{LoopScopeKind, ScopeArrowSink, ScopeExitTarget, TypeEnvironment};
use crate::type_checking::aggregate::initialization::typecheck_initializer_list;
use crate::type_checking::coercion::implicit::{implicit_cast, promotion::std_rval_promotion};
use crate::type_checking::control_flow::r#return::typecheck_return;
use crate::type_checking::control_flow::r#yield::typecheck_yield;
use crate::type_checking::control_flow::{
    enqueue_jump_arrow, expr_may_fall_through, process_for_increment_arrows,
    typecheck_fallthrough_scope,
};
use crate::type_checking::op::binop::access::typecheck_access;
use crate::type_checking::op::binop::assign::typecheck_assignment;
use crate::type_checking::op::binop::calls::typecheck_method_call;
use crate::type_checking::op::unop::{typecheck_sizeof_expr, typecheck_sizeof_type};
use crate::type_checking::op::{self, try_typecheck_special_binop, typecheck_binop};
use crate::type_checking::result::TypecheckResult;
use crate::type_checking::value::{
    identifiers::typecheck_identifier,
    literals::{
        typecheck_float_literal, typecheck_int_literal, typecheck_string_literal, typecheck_unit,
    },
    locals::typecheck_var_declaration,
    moves::{typecheck_adopt, typecheck_leak, typecheck_unpack},
    unsafe_ops::typecheck_unsafe,
};
use cx_ast::ast::expression::{CXBinOp, CXExprKind, CXExpression};
use cx_log::CXResult;
use cx_mir::EnvironmentNamespace;
use cx_mir::mir::data::{MIRIntegerType, MIRTypeKind};
use cx_mir::mir::expression::{MIRExpression, MIRExpressionKind};
use cx_tokens::TokenRange;

use crate::type_checking::control_flow::r#match::typecheck_match;
use crate::type_checking::control_flow::switch::typecheck_switch;
use cx_mir::mir::data::MIRType;

pub fn typecheck_expr(
    env: &mut TypeEnvironment,
    namespace: &EnvironmentNamespace,
    expr: &CXExpression,
    expected_type: Option<&MIRType>,
) -> CXResult<TypecheckResult> {
    typecheck_expr_inner(env, namespace, expr, expected_type)
}

fn typecheck_expr_inner(
    env: &mut TypeEnvironment,
    namespace: &EnvironmentNamespace,
    expr: &CXExpression,
    expected_type: Option<&MIRType>,
) -> CXResult<TypecheckResult> {
    let mut result = match &expr.kind {
        CXExprKind::Block {
            exprs,
            creates_scope,
        } => {
            if *creates_scope {
                env.push_defer_scope();
            }

            let mut block = Vec::new();

            for statement in exprs {
                let expr = typecheck_expr(env, namespace, statement, None)?
                    .standard_ready_coerce(env, expr.token_range())?;

                block.push(expr);

                if !env.function.is_current_scope_reachable() {
                    break;
                }
            }

            if *creates_scope {
                if env.function.is_current_scope_reachable() {
                    block.extend(env.function.current_scope_cleanups());
                }
                env.pop_defer_scope();
            }

            TypecheckResult::from(MIRExpression {
                token_range: TokenRange::internal(),
                kind: MIRExpressionKind::Block { statements: block },
                _type: MIRType::unit(),
            })
        }

        CXExprKind::Defer { expr: deferred } => {
            if env.in_defer_context() {
                return env.log_error(
                    expr.token_range(),
                    "nested defer is not supported".to_string(),
                );
            }
            if env.in_comptime_context() && !env.in_runtime_emit_context() {
                return env.log_error(
                    expr.token_range(),
                    "defer cannot execute while evaluating a comptime function".to_string(),
                );
            }

            let deferred = env.in_defer(|env| {
                typecheck_expr(env, namespace, deferred, None)?
                    .standard_ready_coerce(env, deferred.token_range())
            })?;
            if !deferred._type.is_unit() {
                return env.log_error(
                    expr.token_range(),
                    format!(
                        "deferred expression must have type void, found {}",
                        deferred._type.display_with(&env.symbols)
                    ),
                );
            }
            if !expr_may_fall_through(&deferred) {
                return env.log_error(
                    expr.token_range(),
                    "deferred expression must fall through normally".to_string(),
                );
            }

            env.function.register_defer(deferred);
            TypecheckResult::new(MIRType::unit(), MIRExpressionKind::Unit)
        }

        CXExprKind::StagedExpression { .. } => TypecheckResult::staged_expr(MIRExpression {
            token_range: expr.token_range().clone(),
            kind: MIRExpressionKind::Unit,
            _type: expected_type.cloned().unwrap_or_else(MIRType::unit),
        }),

        CXExprKind::Then => {
            return env.log_error(
                expr.token_range(),
                "'then' may only capture the remainder of an enclosing block".to_string(),
            );
        }

        CXExprKind::IntLiteral {
            magnitude,
            base,
            suffix,
        } => typecheck_int_literal(env, expr.token_range(), *magnitude, *base, *suffix)?,

        CXExprKind::FloatLiteral { val, suffix } => {
            typecheck_float_literal(env, expr.token_range(), *val, *suffix)?
        }

        CXExprKind::StringLiteral { val } => typecheck_string_literal(env, val),

        CXExprKind::VarDeclaration {
            _type,
            name,
            initial_value,
        } => typecheck_var_declaration(
            env,
            namespace,
            expr,
            _type,
            name,
            initial_value.as_ref().map(|v| v.as_ref()),
        )?,

        CXExprKind::Identifier {
            name,
            template_input,
        } => typecheck_identifier(env, namespace, expr, name, template_input.as_ref())?,

        CXExprKind::If {
            condition,
            then_branch,
            else_branch,
        } => {
            let condition_result = typecheck_expr(env, namespace, condition, None)
                .and_then(|v| v.standard_ready_coerce(env, expr.token_range()))
                .and_then(|v| std_rval_promotion(env, v))?;
            env.push_scope(false, false);
            env.function.configure_merge_scope(expr, None, false);
            let join_scope_idx = env.function.current_scope_index();

            let then_result = typecheck_fallthrough_scope(
                env,
                namespace,
                then_branch,
                join_scope_idx,
                ScopeArrowSink::Merge,
                "then",
            )?;

            let else_result = if let Some(else_branch) = else_branch {
                Some(typecheck_fallthrough_scope(
                    env,
                    namespace,
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

            env.pop_scope()
                .map_err(|err| env.complete_err(err, expr.token_range()))?;

            TypecheckResult::from(MIRExpression {
                token_range: TokenRange::internal(),
                kind: MIRExpressionKind::If {
                    condition: Box::new(condition_result),
                    then_branch: Box::new(then_result),
                    else_branch: else_result.map(Box::new),
                },
                _type: cx_mir::mir::data::MIRType::unit(),
            })
        }

        CXExprKind::Ternary {
            condition,
            then_branch,
            else_branch,
        } => {
            let condition_result = typecheck_expr(env, namespace, condition, None)
                .and_then(|v| v.standard_ready_coerce(env, expr.token_range()))
                .and_then(|v| std_rval_promotion(env, v))
                .and_then(|v| implicit_cast(env, v, &MIRType::bool()))?;
            let then_result = typecheck_expr(env, namespace, then_branch, expected_type)
                .and_then(|v| v.standard_ready_coerce(env, expr.token_range()))
                .and_then(|v| std_rval_promotion(env, v))?;
            let else_result = typecheck_expr(env, namespace, else_branch, Some(&then_result._type))
                .and_then(|v| v.standard_ready_coerce(env, expr.token_range()))
                .and_then(|v| std_rval_promotion(env, v))
                .and_then(|v| implicit_cast(env, v, &then_result._type))?;

            TypecheckResult::from(MIRExpression {
                token_range: TokenRange::internal(),
                kind: MIRExpressionKind::If {
                    condition: Box::new(condition_result),
                    then_branch: Box::new(then_result.clone()),
                    else_branch: Some(Box::new(else_result)),
                },
                _type: then_result._type,
            })
        }

        CXExprKind::While {
            condition,
            body,
            pre_eval,
        } => {
            env.push_scope(true, true);
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

            let condition_result = typecheck_expr(env, namespace, condition, None)
                .and_then(|v| v.standard_ready_coerce(env, expr.token_range()))
                .and_then(|v| std_rval_promotion(env, v))?;
            let body_result = typecheck_fallthrough_scope(
                env,
                namespace,
                body,
                loop_scope_idx,
                ScopeArrowSink::LoopContinue,
                "loop fallthrough",
            )?;
            env.pop_scope()
                .map_err(|err| env.complete_err(err, expr.token_range()))?;

            TypecheckResult::from(MIRExpression {
                token_range: TokenRange::internal(),
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
            env.push_scope(true, true);
            env.function.set_scope_anchor(expr);
            let init_result = typecheck_expr(env, namespace, init, None)
                .and_then(|v| v.standard_ready_coerce(env, expr.token_range()))?;
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

            let condition_result = typecheck_expr(env, namespace, condition, None)
                .and_then(|v| v.standard_ready_coerce(env, expr.token_range()))
                .and_then(|v| std_rval_promotion(env, v))?;
            let body_result = typecheck_fallthrough_scope(
                env,
                namespace,
                body,
                loop_scope_idx,
                ScopeArrowSink::LoopPendingIncrement,
                "loop fallthrough",
            )?;
            process_for_increment_arrows(env, namespace, loop_scope_idx, increment)?;
            let increment_result = typecheck_expr(env, namespace, increment, None)?
                .standard_ready_coerce(env, expr.token_range())?;
            env.function
                .restore_snapshot(&env.function.loop_entry_snapshot(loop_scope_idx));
            env.function.set_scope_reachable(loop_scope_idx, true);
            env.pop_scope()
                .map_err(|err| env.complete_err(err, expr.token_range()))?;

            TypecheckResult::from(MIRExpression {
                token_range: TokenRange::internal(),
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
            if env.in_defer_context() {
                return env.log_error(
                    expr.token_range(),
                    "break is not allowed inside a deferred expression".to_string(),
                );
            }
            let Some(scope_idx) = env.function.nearest_break_scope() else {
                return env.log_error(
                    expr.token_range(),
                    "'break' used outside of a loop or switch context".to_string(),
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
                token_range: TokenRange::internal(),
                kind: MIRExpressionKind::Break {
                    scope_depth: scope_idx.index(),
                    cleanups: env.function.cleanups_exiting_to(scope_idx, true),
                },
                _type: MIRType::unit(),
            })
        }

        CXExprKind::Continue => {
            if env.in_defer_context() {
                return env.log_error(
                    expr.token_range(),
                    "continue is not allowed inside a deferred expression".to_string(),
                );
            }
            let Some(scope_idx) = env.function.nearest_continue_scope() else {
                return env.log_error(
                    expr.token_range(),
                    "'continue' used outside of a loop context".to_string(),
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
                token_range: TokenRange::internal(),
                kind: MIRExpressionKind::Continue {
                    scope_depth: scope_idx.index(),
                    cleanups: env.function.cleanups_exiting_to(scope_idx, false),
                },
                _type: MIRType::unit(),
            })
        }

        CXExprKind::Return { value } => {
            let return_type = env.current_function().signature().return_type.clone();
            let value = value
                .as_ref()
                .map(|v| {
                    typecheck_expr(env, namespace, v, Some(&return_type))?
                        .standard_ready_coerce(env, expr.token_range())
                })
                .transpose()?;
            typecheck_return(env, namespace, expr.token_range(), value)?
        }

        CXExprKind::Yield { value } => typecheck_yield(
            env,
            namespace,
            expr.token_range(),
            value.as_ref().map(Box::as_ref),
        )?,

        CXExprKind::Emit { expr: inner } => {
            if !env.in_comptime_context() || env.in_runtime_emit_context() {
                return env.log_error(
                    expr.token_range(),
                    "'emit' may only be used directly in a comptime context".to_string(),
                );
            }

            let inner = env.in_runtime_emit(|env| {
                typecheck_expr(env, namespace, inner, expected_type)?
                    .standard_ready_coerce(env, inner.token_range())
            })?;
            TypecheckResult::from(MIRExpression {
                token_range: TokenRange::internal(),
                _type: inner._type.clone(),
                kind: MIRExpressionKind::Emit(Box::new(inner)),
            })
        }

        CXExprKind::Unsafe { expr: inner } => {
            typecheck_unsafe(env, namespace, inner, expected_type)?
        }

        CXExprKind::Leak { expr: inner } => typecheck_leak(env, namespace, expr, inner)?,

        CXExprKind::Adopt { expr: inner } => typecheck_adopt(env, namespace, expr, inner)?,

        CXExprKind::Unpack {
            expr: inner,
            bindings,
        } => typecheck_unpack(env, namespace, expr, inner, bindings)?,

        CXExprKind::UnOp { operator, operand } => {
            op::typecheck_unop(env, namespace, operator, operand)?
        }

        CXExprKind::BinOp {
            op: CXBinOp::Assign(op),
            lhs,
            rhs,
        } => {
            let lhs = typecheck_expr(env, namespace, lhs, None)?;
            let rhs = typecheck_expr(env, namespace, rhs, None)
                .and_then(|v| v.standard_ready_coerce(env, expr.token_range()))?;

            typecheck_assignment(env, lhs, rhs, op.as_ref().map(Box::deref), expr)?
        }

        CXExprKind::BinOp {
            op: CXBinOp::Access,
            lhs,
            rhs,
        } => {
            let lhs = typecheck_expr_inner(env, namespace, lhs, None)?;

            typecheck_access(env, namespace, lhs, rhs, expr)?
        }

        CXExprKind::BinOp {
            op: CXBinOp::MethodCall,
            lhs,
            rhs,
        } => typecheck_method_call(env, namespace, lhs, rhs, expr, expected_type)?,

        CXExprKind::BinOp { op, lhs, rhs } => {
            if let Some(expr) =
                try_typecheck_special_binop(env, namespace, op, expr, lhs, rhs, expected_type)?
            {
                expr
            } else {
                let lhs = typecheck_expr(env, namespace, lhs, None)
                    .and_then(|v| v.standard_ready_coerce(env, expr.token_range()))?;
                let rhs = typecheck_expr(env, namespace, rhs, None)
                    .and_then(|v| v.standard_ready_coerce(env, expr.token_range()))?;

                typecheck_binop(env, op, lhs, rhs)?
            }
        }

        CXExprKind::InitializerList { indices } => {
            typecheck_initializer_list(env, namespace, expr, indices, expected_type)?
        }

        CXExprKind::Unit => typecheck_unit(),

        CXExprKind::SizeOfType { _type } => typecheck_sizeof_type(env, namespace, expr, _type)?,

        CXExprKind::SizeOfExpr { expr } => typecheck_sizeof_expr(env, namespace, expr)?,

        CXExprKind::Switch {
            condition,
            block,
            cases,
            default_case,
        } => typecheck_switch(
            env,
            namespace,
            condition,
            block,
            cases,
            default_case.as_ref(),
        )?,

        CXExprKind::Match {
            condition,
            arms,
            default,
        } => typecheck_match(
            env,
            namespace,
            condition,
            arms,
            default.as_ref().map(Box::as_ref),
            expected_type,
        )?,

        CXExprKind::Taken => unreachable!("Taken expressions should not be typechecked"),
    };

    result.set_token_range_if_missing(expr.range.clone())?;

    Ok(result)
}

pub fn add_implicit_return(
    env: &mut TypeEnvironment,
    namespace: &EnvironmentNamespace,
    expr: MIRExpression,
) -> CXResult<MIRExpression> {
    if !expr_may_fall_through(&expr) {
        return Ok(expr);
    }

    let func = env.current_function().clone();

    let implicit_value = if func.name() == "main" {
        Some(Box::new(MIRExpression {
            token_range: TokenRange::internal(),
            kind: MIRExpressionKind::IntLiteral(0),
            _type: MIRType::from(MIRTypeKind::Integer {
                _type: MIRIntegerType::I32,
                signed: true,
            }),
        }))
    } else if func.signature().return_type.is_unit() {
        None
    } else {
        return env.log_error(
            expr.token_range,
            format!(
                "Function '{}' with non-void return type must have an explicit return statement",
                func.name()
            ),
        );
    };

    let ret = typecheck_return(
        env,
        namespace,
        &expr.token_range,
        implicit_value.map(|v| *v),
    )?
    .internal_ready_assertion();

    Ok(MIRExpression {
        token_range: TokenRange::internal(),
        kind: MIRExpressionKind::Block {
            statements: vec![expr, ret],
        },
        _type: MIRType::unit(),
    })
}
