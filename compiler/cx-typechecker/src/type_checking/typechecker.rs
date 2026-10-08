use cx_log::catalogue::typecheck as catalogue;
use std::ops::Deref;

use crate::environment::{ControlTarget, TypeEnvironment};
use crate::symbol::completion::complete_type;
use crate::type_checking::aggregate::initialization::typecheck_initializer_list;
use crate::type_checking::coercion::implicit::{implicit_cast, promotion::std_rval_promotion};
use crate::type_checking::control_flow::expr_may_fall_through;
use crate::type_checking::control_flow::r#return::typecheck_return;
use crate::type_checking::control_flow::r#yield::typecheck_yield;
use crate::type_checking::op::binop::access::typecheck_access;
use crate::type_checking::op::binop::assign::typecheck_assignment;
use crate::type_checking::op::binop::calls::{typecheck_method_call, typecheck_va_list};
use crate::type_checking::op::unop::{
    typecheck_alignof_expr, typecheck_alignof_type, typecheck_offsetof, typecheck_sizeof_expr,
    typecheck_sizeof_type,
};
use crate::type_checking::op::{self, try_typecheck_special_binop, typecheck_binop};
use crate::type_checking::result::{StagedTC, TypecheckResult, TypecheckedExpr};
use crate::type_checking::staged_expr::{
    into_expression as staged_into_expression, typecheck_staged_expr,
};
use crate::type_checking::value::{
    identifiers::typecheck_identifier,
    literals::{typecheck_float_literal, typecheck_int_literal, typecheck_unit},
    locals::typecheck_var_declaration,
    moves::{typecheck_adopt, typecheck_leak, typecheck_unpack},
    unsafe_ops::typecheck_unsafe,
};
use cx_hir::ast::expression::{HIRBinOp, HIRBlockKind, HIRExprKind, HIRExpression};
use cx_hir::ast::modifiers::HIR_CONST;
use cx_log::CXResult;
use cx_namespace::module::NamespacePath;
use cx_thir::thir::data::THIRTypeKind;
use cx_thir::thir::expression::{THIRBlockKind, THIRExpression, THIRExpressionKind};
use cx_tokens::TokenRange;
use cx_util::identifier::CXIdent;

use crate::type_checking::control_flow::r#match::typecheck_match;
use crate::type_checking::control_flow::switch::{typecheck_case, typecheck_switch};
use crate::type_checking::control_flow::ternary::typecheck_ternary;
use cx_thir::thir::data::THIRType;

pub fn typecheck_expr(
    env: &mut TypeEnvironment,
    namespace: &NamespacePath,
    expr: &HIRExpression,
    expected_type: Option<&THIRType>,
) -> CXResult<TypecheckResult> {
    typecheck_expr_inner(env, namespace, expr, expected_type)
}

fn typecheck_statement(
    env: &mut TypeEnvironment,
    namespace: &NamespacePath,
    statement: &HIRExpression,
) -> CXResult<THIRExpression> {
    typecheck_expr(env, namespace, statement, None)?
        .standard_ready_coerce(env, statement.token_range())
}

/// Typechecks what remains of the current block. `then` calls this from within a statement of
/// that block, which leaves the block's own call with nothing further to check.
pub(crate) fn typecheck_block_statements(
    env: &mut TypeEnvironment,
    namespace: &NamespacePath,
) -> CXResult<Vec<THIRExpression>> {
    let mut checked = Vec::new();
    while let Some((statements, index)) = env.function.flow_mut().next_block_statement() {
        checked.push(typecheck_statement(env, namespace, &statements[index])?);
    }

    Ok(checked)
}

fn typecheck_expr_inner(
    env: &mut TypeEnvironment,
    namespace: &NamespacePath,
    expr: &HIRExpression,
    expected_type: Option<&THIRType>,
) -> CXResult<TypecheckResult> {
    let mut result = match &expr.kind {
        HIRExprKind::Block { exprs, kind } => {
            let creates_scope = !matches!(kind, HIRBlockKind::Sequence);
            let captures_yield = matches!(kind, HIRBlockKind::Expression);
            if captures_yield {
                let expected_yield = expected_type
                    .cloned()
                    .or_else(|| env.function.flow().yield_state().expected_type);
                env.push_yield_scope(expected_yield);
            } else if creates_scope {
                env.push_scope(false, false, expr.token_range().clone());
            }

            let checked = if creates_scope {
                env.function.flow_mut().enter_block(exprs);
                typecheck_block_statements(env, namespace)
            } else {
                exprs
                    .iter()
                    .map(|statement| typecheck_statement(env, namespace, statement))
                    .collect()
            };

            let effects = if creates_scope {
                Some(
                    env.pop_scope()
                        .map_err(|err| env.complete_err(err, expr.token_range()))?,
                )
            } else {
                None
            };
            let statements = checked?;
            let yield_type = captures_yield
                .then(|| effects.and_then(|effects| effects.yield_type))
                .flatten();
            let yields = yield_type.is_some();
            let result_type = yield_type.unwrap_or_else(THIRType::unit);

            // FIXME: There's gotta be a better way to do this, but I can't think of one right now.
            if yields && statements.iter().all(|s| expr_may_fall_through(s)) {
                return env.log_error(
                    expr.token_range(),
                    &catalogue::MISSING_YIELD,
                    "Block".into(),
                );
            }

            let block = THIRExpression {
                token_range: expr.token_range().clone(),
                kind: THIRExpressionKind::Block {
                    statements,
                    kind: match kind {
                        HIRBlockKind::Sequence => THIRBlockKind::Sequence,
                        HIRBlockKind::Statement => THIRBlockKind::Statement,
                        HIRBlockKind::Expression => THIRBlockKind::Expression,
                    },
                    yields,
                },
                ty: result_type,
            };

            TypecheckResult::from(block)
        }

        HIRExprKind::Defer { expr: deferred } => {
            if env.in_defer_context() {
                return env.log_error(
                    expr.token_range(),
                    &catalogue::INVALID_CONTEXT,
                    ("defer statement".into(), "another deferred context".into()),
                );
            }

            if env.in_comptime_context() && !env.in_runtime_emit_context() {
                return env.log_error(
                    expr.token_range(),
                    &catalogue::INVALID_CONTEXT,
                    ("defer statement".into(), "compile-time context".into()),
                );
            }

            let deferred = env.in_defer(|env| {
                typecheck_expr(env, namespace, deferred, None)?
                    .standard_ready_coerce(env, deferred.token_range())
            })?;

            if !deferred.ty.is_void() {
                return env.log_error(
                    expr.token_range(),
                    &catalogue::TYPE_MISMATCH,
                    (
                        "defer statement".into(),
                        "void type".into(),
                        format!("{}", deferred.ty.display_with(&env.symbols)),
                    ),
                );
            }

            if !expr_may_fall_through(&deferred) {
                return env.log_error(expr.token_range(), &catalogue::DEFER_FALLTHROUGH, ());
            }

            TypecheckResult::new(
                THIRType::unit(),
                THIRExpressionKind::Defer {
                    expression: Box::new(deferred),
                },
            )
        }

        HIRExprKind::ParamStagedExpression { params, body } => {
            TypecheckResult::needs_staged_type(params.clone(), body.clone())
        }

        HIRExprKind::Then => {
            if !env.function.flow().in_block() {
                return env.log_error(
                    expr.token_range(),
                    &catalogue::INVALID_CONTEXT,
                    (
                        "then expression".into(),
                        "a scope that is not a block".into(),
                    ),
                );
            }

            TypecheckResult::from(THIRExpression {
                token_range: expr.token_range().clone(),
                kind: THIRExpressionKind::Block {
                    statements: typecheck_block_statements(env, namespace)?,
                    kind: THIRBlockKind::Sequence,
                    yields: false,
                },
                ty: THIRType::unit(),
            })
        }

        HIRExprKind::IntLiteral {
            magnitude,
            base,
            suffix,
        } => typecheck_int_literal(env, expr.token_range(), *magnitude, *base, *suffix)?,

        HIRExprKind::BoolLiteral(value) => TypecheckResult::from(THIRExpression {
            token_range: expr.token_range().clone(),
            kind: THIRExpressionKind::BoolLiteral(*value),
            ty: THIRType::bool(),
        }),

        HIRExprKind::FloatLiteral { val, suffix } => {
            typecheck_float_literal(env, expr.token_range(), *val, *suffix)?
        }

        HIRExprKind::StringLiteral { val } => TypecheckResult::new(
            env.symbols
                .mem_ref_to(THIRType::from(THIRTypeKind::Str).add_specifier(HIR_CONST)),
            THIRExpressionKind::StringLiteral { value: val.clone() },
        ),

        HIRExprKind::VarDeclaration {
            ty,
            name,
            initial_value,
            linkage,
        } => typecheck_var_declaration(
            env,
            namespace,
            expr,
            ty,
            name,
            initial_value.as_ref().map(|v| v.as_ref()),
            *linkage,
        )?,

        HIRExprKind::Identifier {
            name,
            template_input,
        } => typecheck_identifier(env, namespace, expr, name, template_input.as_ref())?,

        HIRExprKind::VaArg { list, ty } => {
            let list = typecheck_va_list(env, namespace, list)?;
            let ty = complete_type(env, namespace, ty)?;
            TypecheckResult::new(
                ty.clone(),
                THIRExpressionKind::VaArg {
                    list: Box::new(list),
                    ty,
                },
            )
        }

        HIRExprKind::If {
            condition,
            then_branch,
            else_branch,
        } => {
            env.push_scope(false, false, condition.token_range().clone());
            let condition_result = typecheck_expr(env, namespace, condition, None)
                .and_then(|v| v.standard_ready_coerce(env, expr.token_range()))
                .and_then(|v| std_rval_promotion(env, v))
                .and_then(|v| implicit_cast(env, v, &THIRType::bool()))?;

            env.push_scope(false, false, then_branch.token_range().clone());
            let then_result = typecheck_expr(env, namespace, then_branch, None)
                .and_then(|v| v.standard_ready_coerce(env, expr.token_range()))?;
            env.pop_scope()
                .map_err(|err| env.complete_err(err, expr.token_range()))?;
            env.pop_scope()
                .map_err(|err| env.complete_err(err, expr.token_range()))?;

            let else_result = else_branch
                .as_ref()
                .map(|e| {
                    env.push_scope(false, false, e.token_range().clone());
                    let result = typecheck_expr(env, namespace, e, None)
                        .and_then(|v| v.standard_ready_coerce(env, expr.token_range()));
                    env.pop_scope()
                        .map_err(|err| env.complete_err(err, expr.token_range()))?;
                    result
                })
                .transpose()?;

            TypecheckResult::from(THIRExpression {
                token_range: TokenRange::internal(),
                kind: THIRExpressionKind::If {
                    condition: Box::new(condition_result),
                    then_branch: Box::new(then_result),
                    else_branch: else_result.map(Box::new),
                },
                ty: THIRType::unit(),
            })
        }

        HIRExprKind::Ternary {
            condition,
            then_branch,
            else_branch,
        } => typecheck_ternary(
            env,
            namespace,
            expr,
            condition,
            then_branch,
            else_branch,
            expected_type,
        )?,

        HIRExprKind::While {
            condition,
            body,
            pre_eval,
        } => {
            let condition_result = typecheck_expr(env, namespace, condition, None)
                .and_then(|v| v.standard_ready_coerce(env, expr.token_range()))
                .and_then(|v| std_rval_promotion(env, v))
                .and_then(|v| implicit_cast(env, v, &THIRType::bool()))?;

            env.push_scope(true, true, expr.range.clone());
            let body_result = typecheck_expr(env, namespace, body, None)
                .and_then(|v| v.standard_ready_coerce(env, expr.token_range()))?;
            env.pop_scope()
                .map_err(|err| env.complete_err(err, expr.token_range()))?;

            TypecheckResult::from(THIRExpression {
                token_range: TokenRange::internal(),
                kind: THIRExpressionKind::While {
                    condition: Box::new(condition_result),
                    body: Box::new(body_result),
                    pre_eval: *pre_eval,
                },
                ty: THIRType::unit(),
            })
        }

        HIRExprKind::For {
            init,
            condition,
            increment,
            body,
        } => {
            env.push_scope(false, false, expr.token_range().clone());
            let init_result = typecheck_expr(env, namespace, init, None)
                .and_then(|v| v.standard_ready_coerce(env, expr.token_range()))?;

            let condition_result = typecheck_expr(env, namespace, condition, None)
                .and_then(|v| v.standard_ready_coerce(env, expr.token_range()))
                .and_then(|v| std_rval_promotion(env, v))
                .and_then(|v| implicit_cast(env, v, &THIRType::bool()))?;

            let increment_result = typecheck_expr(env, namespace, increment, None)?
                .standard_ready_coerce(env, expr.token_range())?;

            env.push_scope(true, true, expr.token_range().clone());
            let body_result = typecheck_expr(env, namespace, body, None)
                .and_then(|v| v.standard_ready_coerce(env, expr.token_range()))?;
            env.pop_scope()
                .map_err(|err| env.complete_err(err, expr.token_range()))?;

            env.pop_scope()
                .map_err(|err| env.complete_err(err, expr.token_range()))?;

            TypecheckResult::from(THIRExpression {
                token_range: TokenRange::internal(),
                kind: THIRExpressionKind::For {
                    init: Box::new(init_result),
                    condition: Box::new(condition_result),
                    increment: Box::new(increment_result),
                    body: Box::new(body_result),
                },
                ty: THIRType::unit(),
            })
        }

        HIRExprKind::Break => {
            if env.in_defer_context() {
                return env.log_error(
                    expr.token_range(),
                    &catalogue::INVALID_CONTEXT,
                    ("break statement".into(), "deferred context".into()),
                );
            }

            let target = env.function.flow().break_target();
            if target == ControlTarget::Invalid {
                return env.log_error(
                    expr.token_range(),
                    &catalogue::INVALID_CONTEXT,
                    ("break statement".into(), "non-loop context".into()),
                );
            }

            TypecheckResult::from(THIRExpression {
                token_range: TokenRange::internal(),
                kind: THIRExpressionKind::Break,
                ty: THIRType::unit(),
            })
        }

        HIRExprKind::Continue => {
            if env.in_defer_context() {
                return env.log_error(
                    expr.token_range(),
                    &catalogue::INVALID_CONTEXT,
                    ("continue statement".into(), "deferred context".into()),
                );
            }

            let target = env.function.flow().continue_target();
            if target == ControlTarget::Invalid {
                return env.log_error(
                    expr.token_range(),
                    &catalogue::INVALID_CONTEXT,
                    ("continue statement".into(), "non-loop context".into()),
                );
            }

            TypecheckResult::from(THIRExpression {
                token_range: TokenRange::internal(),
                kind: THIRExpressionKind::Continue,
                ty: THIRType::unit(),
            })
        }

        HIRExprKind::Goto { name } => {
            if env.in_defer_context() {
                return env.log_error(
                    expr.token_range(),
                    &catalogue::INVALID_CONTEXT,
                    ("goto statement".into(), "deferred context".into()),
                );
            }
            env.function
                .record_label_use(name, expr.token_range().clone());
            TypecheckResult::from(THIRExpression {
                token_range: TokenRange::internal(),
                kind: THIRExpressionKind::Goto { name: name.clone() },
                ty: THIRType::unit(),
            })
        }

        HIRExprKind::IndirectGoto { target } => {
            if env.in_defer_context() {
                return env.log_error(
                    expr.token_range(),
                    &catalogue::INVALID_CONTEXT,
                    ("goto statement".into(), "deferred context".into()),
                );
            }
            let address_type = env
                .symbols
                .pointer_to(THIRType::unit().add_specifier(HIR_CONST));
            let target = typecheck_expr(env, namespace, target, Some(&address_type))
                .and_then(|v| v.standard_ready_coerce(env, target.token_range()))
                .and_then(|v| std_rval_promotion(env, v))
                .and_then(|v| implicit_cast(env, v, &address_type))?;
            TypecheckResult::from(THIRExpression {
                token_range: expr.token_range().clone(),
                kind: THIRExpressionKind::IndirectGoto {
                    target: Box::new(target),
                },
                ty: THIRType::unit(),
            })
        }

        HIRExprKind::LabelAddress { name } => {
            env.function
                .record_label_address(name, expr.token_range().clone());
            TypecheckResult::from(THIRExpression {
                token_range: expr.token_range().clone(),
                kind: THIRExpressionKind::LabelAddress {
                    function: CXIdent::new(env.current_function().symbol_name()),
                    name: name.clone(),
                },
                ty: env.symbols.pointer_to(THIRType::unit()),
            })
        }

        HIRExprKind::Label { name, statement } => {
            if !env.function.declare_label(name, expr.token_range().clone()) {
                return env.log_error(
                    expr.token_range(),
                    &catalogue::DUPLICATE_ITEM,
                    (
                        "label".into(),
                        format!("function {}", env.current_function().symbol_name()),
                    ),
                );
            }
            let statement = typecheck_expr(env, namespace, statement, None)
                .and_then(|v| v.standard_ready_coerce(env, statement.token_range()))?;
            TypecheckResult::from(THIRExpression {
                token_range: TokenRange::internal(),
                kind: THIRExpressionKind::Label {
                    name: name.clone(),
                    statement: Box::new(statement),
                },
                ty: THIRType::unit(),
            })
        }

        HIRExprKind::Return { value } => {
            let return_type = if env.in_staged_context() || env.in_runtime_emit_context() {
                let Some(return_type) = env.staging_context().return_type().cloned() else {
                    return env.log_error(
                        expr.token_range(),
                        &catalogue::INVALID_CONTEXT,
                        ("return statement".into(), "non-function context".into()),
                    );
                };
                return_type
            } else {
                env.current_function().signature().return_type().clone()
            };
            if return_type.is_unreachable() {
                return typecheck_return(env, namespace, expr.token_range(), None);
            }
            let value = value
                .as_ref()
                .map(|v| {
                    let result = typecheck_expr(env, namespace, v, Some(&return_type))?;
                    match result {
                        TypecheckResult::Ready(TypecheckedExpr::Staged(StagedTC::Literal(
                            staged,
                        ))) if env.in_comptime_context() => Ok(staged_into_expression(staged)),
                        TypecheckResult::Ready(TypecheckedExpr::Staged(StagedTC::Binding(
                            staged,
                        ))) if env.in_comptime_context() => {
                            let mut reference = staged.reference;
                            reference.ty = return_type.clone();
                            Ok(reference)
                        }
                        result => result
                            .apply_expected_type(env, namespace, &return_type)?
                            .standard_ready_coerce(env, expr.token_range()),
                    }
                })
                .transpose()?;
            typecheck_return(env, namespace, expr.token_range(), value)?
        }

        HIRExprKind::Yield { value } => typecheck_yield(
            env,
            namespace,
            expr.token_range(),
            value.as_ref().map(Box::as_ref),
        )?,

        HIRExprKind::Emit { expr: inner } => {
            typecheck_staged_expr(env, namespace, inner, expected_type)?
        }

        HIRExprKind::Unsafe { expr: inner } => {
            typecheck_unsafe(env, namespace, inner, expected_type)?
        }

        HIRExprKind::Leak { expr: inner } => typecheck_leak(env, namespace, expr, inner)?,

        HIRExprKind::Adopt { expr: inner } => typecheck_adopt(env, namespace, expr, inner)?,

        HIRExprKind::Unpack {
            expr: inner,
            bindings,
        } => typecheck_unpack(env, namespace, expr, inner, bindings)?,

        HIRExprKind::UnOp { operator, operand } => {
            op::typecheck_unop(env, namespace, expr, operator, operand)?
        }

        HIRExprKind::BinOp {
            op: HIRBinOp::Assign(op),
            lhs,
            rhs,
        } => {
            let lhs = typecheck_expr(env, namespace, lhs, None)?;
            let rhs = typecheck_expr(env, namespace, rhs, None)
                .and_then(|v| v.standard_ready_coerce(env, expr.token_range()))?;

            typecheck_assignment(env, lhs, rhs, op.as_ref().map(Box::deref), expr)?
        }

        HIRExprKind::BinOp {
            op: HIRBinOp::Access,
            lhs,
            rhs,
        } => {
            let lhs = typecheck_expr_inner(env, namespace, lhs, None)?;

            typecheck_access(env, namespace, lhs, rhs, expr)?
        }

        HIRExprKind::BinOp {
            op: HIRBinOp::MethodCall,
            lhs,
            rhs,
        } => env.in_argument_scope(|env| {
            typecheck_method_call(env, namespace, lhs, rhs, expr, expected_type)
        })?,

        HIRExprKind::BinOp { op, lhs, rhs } => {
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

        HIRExprKind::InitializerList { indices } => {
            typecheck_initializer_list(env, namespace, expr, indices, expected_type)?
        }

        HIRExprKind::Void => typecheck_unit(),

        HIRExprKind::SizeOfType { ty } => typecheck_sizeof_type(env, namespace, expr, ty)?,

        HIRExprKind::SizeOfExpr { expr } => typecheck_sizeof_expr(env, namespace, expr)?,

        HIRExprKind::AlignOfType { ty } => typecheck_alignof_type(env, namespace, expr, ty)?,

        HIRExprKind::AlignOfExpr { expr } => typecheck_alignof_expr(env, namespace, expr)?,

        HIRExprKind::OffsetOf { ty, member } => {
            typecheck_offsetof(env, namespace, expr, ty, member)?
        }

        HIRExprKind::Switch { condition, body } => {
            typecheck_switch(env, namespace, expr, condition, body)?
        }

        HIRExprKind::Case { value, statement } => {
            typecheck_case(env, namespace, expr, value.as_deref(), statement)?
        }

        HIRExprKind::Match { condition, arms } => {
            typecheck_match(env, namespace, condition, arms, expected_type)?
        }

        HIRExprKind::Taken => unreachable!("Taken expressions should not be typechecked"),
    };

    result.set_token_range_if_missing(expr.range.clone())?;

    Ok(result)
}
