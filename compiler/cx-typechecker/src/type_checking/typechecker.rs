use crate::environment::{BindingMoveState, TrackedBindingState, TypeEnvironment};
use crate::log_typecheck_error;
use crate::type_checking::accumulation::TypecheckResult;
use crate::type_checking::binary_ops::{
    typecheck_access, typecheck_binop, typecheck_binop_mir_vals, typecheck_is,
    typecheck_method_call,
};
use crate::type_checking::casting::{coerce_condition, coerce_value, explicit_cast, implicit_cast};
use cx_ast::ast::{CXBinOp, CXExpr, CXExprKind, CXGlobalVariable, CXUnOp};
use cx_ast::data::{CX_CONST, CXLinkageMode};
use cx_mir::mir::expression::{MIRExpression, MIRExpressionKind, MIRSourceRange, MIRUnOp};
use cx_mir::mir::program::{MIRBaseMappings, MIRGlobalVarKind, MIRGlobalVariable};
use cx_mir::mir::types::{MIRFloatType, MIRIntegerType, MIRTypeKind};
use cx_util::identifier::CXIdent;
use cx_util::{CXError, CXResult};
use cx_util::scoped_map::ScopedMap;

fn anonymous_name_gen() -> String {
    use std::sync::atomic::{AtomicUsize, Ordering};

    static COUNTER: AtomicUsize = AtomicUsize::new(0);
    let id = COUNTER.fetch_add(1, Ordering::SeqCst);
    format!("__anon_{id}")
}

use crate::type_checking::r#match::{typecheck_match, typecheck_switch};
use crate::type_checking::structured_initialization::typecheck_initializer_list;
use cx_mir::mir::types::{MIRFunctionPrototype, MIRType};

#[derive(Clone)]
pub(crate) struct ControlFlowSnapshot {
    pub(crate) symbol_table: ScopedMap<MIRExpression>,
    pub(crate) tracked_bindings: ScopedMap<TrackedBindingState>,
}

pub(crate) fn control_flow_snapshot(env: &TypeEnvironment) -> ControlFlowSnapshot {
    ControlFlowSnapshot {
        symbol_table: env.symbol_table.clone(),
        tracked_bindings: env.tracked_bindings.clone(),
    }
}

pub(crate) fn restore_control_flow_snapshot(
    env: &mut TypeEnvironment,
    snapshot: &ControlFlowSnapshot,
) {
    env.symbol_table = snapshot.symbol_table.clone();
    env.tracked_bindings = snapshot.tracked_bindings.clone();
}

pub(crate) fn expr_may_fall_through(expr: &MIRExpression) -> bool {
    match &expr.kind {
        MIRExpressionKind::Return { .. }
        | MIRExpressionKind::Break { .. }
        | MIRExpressionKind::Continue { .. } => false,
        MIRExpressionKind::Block { statements } => statements
            .last()
            .map(expr_may_fall_through)
            .unwrap_or(true),
        MIRExpressionKind::If {
            then_branch,
            else_branch,
            ..
        } => {
            expr_may_fall_through(then_branch)
                || else_branch
                    .as_ref()
                    .map(|branch| expr_may_fall_through(branch))
                    .unwrap_or(true)
        }
        MIRExpressionKind::CSwitch { cases, default, .. }
        | MIRExpressionKind::Match {
            arms: cases,
            default,
            ..
        } => {
            cases.iter().any(|(_, branch)| expr_may_fall_through(branch))
                || default
                    .as_ref()
                    .map(|branch| expr_may_fall_through(branch))
                    .unwrap_or(true)
        }
        _ => true,
    }
}

pub(crate) fn join_tracked_bindings(
    env: &mut TypeEnvironment,
    expr: &CXExpr,
    base_snapshot: &ControlFlowSnapshot,
    exit_snapshots: &[ControlFlowSnapshot],
    join_name: &str,
) -> CXResult<()> {
    if exit_snapshots.is_empty() {
        restore_control_flow_snapshot(env, base_snapshot);
        return Ok(());
    }

    restore_control_flow_snapshot(env, base_snapshot);

    let mut inconsistent = Vec::new();
    for (name, base_binding) in base_snapshot.tracked_bindings.iter() {
        let mut merged_state = None;

        for snapshot in exit_snapshots {
            let Some(binding) = snapshot.tracked_bindings.get(name) else {
                continue;
            };

            if let Some(existing) = merged_state {
                if existing != binding.state {
                    inconsistent.push(name.clone());
                    merged_state = Some(BindingMoveState::ConditionallyMoved);
                    break;
                }
            } else {
                merged_state = Some(binding.state);
            }
        }

        let Some(merged_state) = merged_state else {
            continue;
        };

        env.tracked_bindings.insert(
            name.clone(),
            TrackedBindingState {
                state: merged_state,
                ..base_binding.clone()
            },
        );
    }

    if inconsistent.is_empty() {
        return Ok(());
    }

    log_typecheck_error!(
        env,
        expr,
        " nocopy binding(s) have inconsistent move state at {}: {}",
        join_name,
        inconsistent.join(", ")
    )
}

fn ensure_binding_available(
    env: &mut TypeEnvironment,
    expr: &CXExpr,
    name: &CXIdent,
) -> CXResult<()> {
    let Some(binding) = env.tracked_binding(name.as_str()) else {
        return Ok(());
    };

    match binding.state {
        BindingMoveState::Available => Ok(()),
        BindingMoveState::Moved => log_typecheck_error!(
            env,
            expr,
            " Identifier '{}' has been moved",
            name
        ),
        BindingMoveState::ConditionallyMoved => log_typecheck_error!(
            env,
            expr,
            " Identifier '{}' was conditionally moved across a control-flow join",
            name
        ),
    }
}

fn type_is_safe_signature(ty: &MIRType) -> bool {
    match &ty.kind {
        MIRTypeKind::Integer { .. } | MIRTypeKind::Float { .. } | MIRTypeKind::Unit => true,
        MIRTypeKind::Structured { fields, .. } => {
            fields.iter().all(|(_, field_type)| type_is_safe_signature(field_type))
        }
        _ => false,
    }
}

fn prototype_is_safe_callable(prototype: &MIRFunctionPrototype) -> bool {
    prototype.contract.safe
        && !prototype.var_args
        && type_is_safe_signature(&prototype.return_type)
        && prototype
            .params
            .iter()
            .all(|param| type_is_safe_signature(&param._type))
}

fn type_is_safe_expression(ty: &MIRType) -> bool {
    match &ty.kind {
        MIRTypeKind::MemoryReference(inner) => type_is_safe_expression(inner),
        MIRTypeKind::Function { prototype } => prototype_is_safe_callable(prototype),
        _ => type_is_safe_signature(ty),
    }
}

pub(crate) fn validate_safe_function_signature(
    env: &mut TypeEnvironment,
    prototype: &MIRFunctionPrototype,
    expr: &CXExpr,
) -> CXResult<()> {
    if !prototype.contract.safe {
        return Ok(());
    }

    if prototype.var_args {
        return log_typecheck_error!(
            env,
            expr,
            " Safe function '{}' may not use varargs",
            prototype.name
        );
    }

    if !type_is_safe_signature(&prototype.return_type) {
        return log_typecheck_error!(
            env,
            expr,
            " Safe function '{}' has unsupported return type {}",
            prototype.name,
            prototype.return_type
        );
    }

    for param in &prototype.params {
        if !type_is_safe_signature(&param._type) {
            return log_typecheck_error!(
                env,
                expr,
                " Safe function '{}' has unsupported parameter type {}",
                prototype.name,
                param._type
            );
        }
    }

    Ok(())
}

pub(crate) fn assert_scope_nodestruct_discharged(
    env: &mut TypeEnvironment,
    expr: &CXExpr,
) -> CXResult<()> {
    let live = env
        .current_scope_nodestruct_bindings()
        .into_iter()
        .filter(|(_, binding)| binding.state != BindingMoveState::Moved)
        .map(|(name, _)| name)
        .collect::<Vec<_>>();
    if live.is_empty() {
        return Ok(());
    }

    log_typecheck_error!(
        env,
        expr,
        " nodestruct local(s) reach scope end without move or @leak: {}",
        live.join(", ")
    )
}

fn assert_return_nodestruct_discharged(
    env: &mut TypeEnvironment,
    expr: &CXExpr,
) -> CXResult<()> {
    let live = env.nodestruct_bindings_in_nonfinal_state();
    if live.is_empty() {
        return Ok(());
    }

    log_typecheck_error!(
        env,
        expr,
        " nodestruct binding(s) must be moved or @leak'ed before return: {}",
        live.join(", ")
    )
}

fn validate_safe_contract_expression(
    env: &mut TypeEnvironment,
    expr: &CXExpr,
    mir_expr: &MIRExpression,
) -> CXResult<()> {
    if !env.contract_pure_mode {
        return Ok(());
    }

    if !type_is_safe_expression(&mir_expr._type) {
        return log_typecheck_error!(
            env,
            expr,
            " Safe contract expression uses unsupported type {}",
            mir_expr._type
        );
    }

    match &mir_expr.kind {
        MIRExpressionKind::BoolLiteral(_)
        | MIRExpressionKind::IntLiteral(..)
        | MIRExpressionKind::FloatLiteral(..)
        | MIRExpressionKind::Unit
        | MIRExpressionKind::Variable(_)
        | MIRExpressionKind::ContractVariable { .. }
        | MIRExpressionKind::BinaryOperation { .. }
        | MIRExpressionKind::UnaryOperation { .. }
        | MIRExpressionKind::TypeConversion { .. }
        | MIRExpressionKind::Typechange(_) => Ok(()),
        _ => log_typecheck_error!(
            env,
            expr,
            " Safe contract conditions must be pure expressions"
        ),
    }
}

fn validate_safe_expression(
    env: &mut TypeEnvironment,
    expr: &CXExpr,
    mir_expr: &MIRExpression,
) -> CXResult<()> {
    if !env.in_safe_context() {
        return Ok(());
    }

    if !type_is_safe_expression(&mir_expr._type) {
        return log_typecheck_error!(
            env,
            expr,
            " Safe function expression uses unsupported type {}",
            mir_expr._type
        );
    }

    match &mir_expr.kind {
        MIRExpressionKind::BoolLiteral(_)
        | MIRExpressionKind::IntLiteral(..)
        | MIRExpressionKind::FloatLiteral(..)
        | MIRExpressionKind::Unit
        | MIRExpressionKind::Variable(_)
        | MIRExpressionKind::ContractVariable { .. }
        | MIRExpressionKind::BinaryOperation { .. }
        | MIRExpressionKind::UnaryOperation { .. }
        | MIRExpressionKind::MemoryRead { .. }
        | MIRExpressionKind::MemoryWrite { .. }
        | MIRExpressionKind::CreateStackVariable { .. }
        | MIRExpressionKind::Typechange(_)
        | MIRExpressionKind::If { .. }
        | MIRExpressionKind::While { .. }
        | MIRExpressionKind::For { .. }
        | MIRExpressionKind::Return { .. }
        | MIRExpressionKind::Block { .. }
        | MIRExpressionKind::TypeConversion { .. }
        | MIRExpressionKind::LeakLifetime { .. }
        | MIRExpressionKind::Unsafe { .. } => Ok(()),
        MIRExpressionKind::FunctionReference { .. } | MIRExpressionKind::CallFunction { .. } => {
            let MIRTypeKind::Function { prototype } = &mir_expr._type.kind else {
                return log_typecheck_error!(
                    env,
                    expr,
                    " Safe function call target must have a safe function type"
                );
            };

            if prototype_is_safe_callable(prototype) {
                Ok(())
            } else {
                log_typecheck_error!(
                    env,
                    expr,
                    " Safe code may only call other safe functions"
                )
            }
        }
        _ => log_typecheck_error!(
            env,
            expr,
            " Expression kind is not yet allowed in safe functions"
        ),
    }
}

pub fn typecheck_expr(
    env: &mut TypeEnvironment,
    base_data: &MIRBaseMappings,
    expr: &CXExpr,
    expected_type: Option<&MIRType>,
) -> CXResult<TypecheckResult> {
    typecheck_expr_inner(env, base_data, expr, expected_type)
}

pub fn typecheck_expr_inner(
    env: &mut TypeEnvironment,
    base_data: &MIRBaseMappings,
    expr: &CXExpr,
    expected_type: Option<&MIRType>,
) -> CXResult<TypecheckResult> {
    let mut result = match &expr.kind {
        CXExprKind::Block { exprs } => {
            let block = exprs
                .iter()
                .map(|e| typecheck_expr(env, base_data, e, None).map(|res| res.expression))
                .collect::<CXResult<Vec<_>>>()?;

            TypecheckResult::expr2(MIRExpression {
                source_range: None,
                kind: MIRExpressionKind::Block { statements: block },
                _type: MIRType::unit(),
            })
        }

        CXExprKind::IntLiteral { val, bytes } => TypecheckResult::expr2(MIRExpression {
            source_range: None,
            kind: MIRExpressionKind::IntLiteral(
                *val,
                MIRIntegerType::from_bytes(*bytes).unwrap(),
                true,
            ),
            _type: cx_mir::mir::types::MIRType::from(MIRTypeKind::Integer {
                _type: MIRIntegerType::from_bytes(*bytes).unwrap(),
                signed: true,
            }),
        }),

        CXExprKind::FloatLiteral { val, bytes } => TypecheckResult::expr2(MIRExpression {
            source_range: None,
            kind: MIRExpressionKind::FloatLiteral(*val, MIRFloatType::from_bytes(*bytes).unwrap()),
            _type: cx_mir::mir::types::MIRType::from(MIRTypeKind::Float {
                _type: MIRFloatType::from_bytes(*bytes).unwrap(),
            }),
        }),

        CXExprKind::StringLiteral { val } => {
            let anonymous_name = anonymous_name_gen();
            let name_ident = CXIdent::new(anonymous_name.clone());

            env.realized_globals.insert(
                anonymous_name.clone(),
                MIRGlobalVariable {
                    kind: MIRGlobalVarKind::StringLiteral {
                        name: name_ident.clone(),
                        value: val.clone(),
                    },
                    is_mutable: false,
                    linkage: CXLinkageMode::Static,
                },
            );

            let char_type = env
                .get_realized_type("char")
                .unwrap()
                .clone()
                .pointer_to()
                .add_specifier(CX_CONST);

            TypecheckResult::expr2(MIRExpression {
                source_range: None,
                kind: MIRExpressionKind::Variable(name_ident),
                _type: char_type,
            })
        }

        CXExprKind::VarDeclaration {
            _type,
            name,
            initial_value,
        } => {
            let _type = env.complete_type(base_data, _type)?;
            let mem_type = _type.clone().mem_ref_to();

            // Typecheck initial value if present
            let mir_initial_value = match initial_value {
                Some(init_expr) => {
                    let init_tc = typecheck_expr(env, base_data, init_expr, Some(&_type))?;
                    let init_tc = implicit_cast(env, expr, init_tc.into_expression(), &_type)?;
                    Some(Box::new(init_tc))
                }
                None => None,
            };

            let allocation = MIRExpression {
                source_range: None,
                kind: MIRExpressionKind::CreateStackVariable {
                    name: Some(name.clone()),
                    _type: _type.clone(),
                    initial_value: mir_initial_value,
                },
                _type: mem_type.clone(),
            };

            env.insert_symbol(
                name.as_string(),
                MIRExpression {
                    source_range: None,
                    kind: MIRExpressionKind::Variable(name.clone()),
                    _type: mem_type,
                },
            );

            env.track_binding(name.as_string(), &_type);

            TypecheckResult::expr2(allocation)
        }

        CXExprKind::Identifier(name) => {
            if let Some(symbol_val) = env.symbol_value(name.as_str()) {
                let symbol_val = symbol_val.clone();
                ensure_binding_available(env, expr, name)?;
                TypecheckResult::expr2(symbol_val)
            } else if let Ok(function_type) = env.get_standard_function(base_data, expr, name, None)
            {
                TypecheckResult::expr2(MIRExpression {
                    source_range: None,
                    kind: MIRExpressionKind::FunctionReference {
                        implicit_variables: vec![],
                    },
                    _type: MIRType::from(MIRTypeKind::Function {
                        prototype: Box::new(function_type),
                    }),
                })
            } else if env.in_safe_context()
                && base_data.global_variables.contains_key(name.as_str())
            {
                return log_typecheck_error!(
                    env,
                    expr,
                    " Safe functions may not access global variables"
                );
            } else if let Ok(global) = global_expr(env, base_data, name.as_str()) {
                TypecheckResult::expr2(global)
            } else {
                return log_typecheck_error!(env, expr, "Identifier '{}' not found", name);
            }
        }

        CXExprKind::TemplatedIdentifier {
            name,
            template_input,
        } => {
            // These [for now], are only for functions, as templated type identifiers can only appear
            // in CXType contexts.

            let function =
                env.get_standard_function(base_data, expr, name, Some(template_input))?;

            TypecheckResult::expr2(MIRExpression {
                source_range: None,
                kind: MIRExpressionKind::FunctionReference {
                    implicit_variables: vec![],
                },
                _type: MIRType::from(MIRTypeKind::Function {
                    prototype: Box::new(function),
                }),
            })
        }

        CXExprKind::If {
            condition,
            then_branch,
            else_branch,
        } => {
            let condition_result = typecheck_expr(env, base_data, condition, None)
                .and_then(|c| coerce_condition(env, expr, c.into_expression()))?;
            let base_snapshot = control_flow_snapshot(env);

            let then_result = typecheck_expr(env, base_data, then_branch, None)?.into_expression();
            let then_snapshot = control_flow_snapshot(env);
            let then_reaches_end = expr_may_fall_through(&then_result);

            restore_control_flow_snapshot(env, &base_snapshot);

            let else_result = if let Some(else_branch) = else_branch {
                let else_result = typecheck_expr(env, base_data, else_branch, None)?.into_expression();
                let else_snapshot = control_flow_snapshot(env);
                let else_reaches_end = expr_may_fall_through(&else_result);

                let mut exit_snapshots = Vec::new();
                if then_reaches_end {
                    exit_snapshots.push(then_snapshot.clone());
                }
                if else_reaches_end {
                    exit_snapshots.push(else_snapshot);
                }

                join_tracked_bindings(env, expr, &base_snapshot, &exit_snapshots, "if join")?;
                Some(else_result)
            } else {
                let mut exit_snapshots = vec![base_snapshot.clone()];
                if then_reaches_end {
                    exit_snapshots.push(then_snapshot);
                }

                join_tracked_bindings(env, expr, &base_snapshot, &exit_snapshots, "if join")?;
                None
            };

            TypecheckResult::expr2(MIRExpression {
                source_range: None,
                kind: MIRExpressionKind::If {
                    condition: Box::new(condition_result),
                    then_branch: Box::new(then_result),
                    else_branch: else_result.map(|r| Box::new(r)),
                },
                _type: cx_mir::mir::types::MIRType::unit(),
            })
        }

        CXExprKind::While {
            condition,
            body,
            pre_eval,
        } => {
            let outer_snapshot = control_flow_snapshot(env);
            env.push_scope(true, true);

            let condition_result = typecheck_expr(env, base_data, condition, None)
                .and_then(|c| coerce_condition(env, expr, c.into_expression()))?;
            let loop_snapshot = control_flow_snapshot(env);
            let body_result = typecheck_expr(env, base_data, body, None)?;
            let body_snapshot = control_flow_snapshot(env);
            let mut exit_snapshots = vec![loop_snapshot.clone()];

            if expr_may_fall_through(&body_result.expression) {
                exit_snapshots.push(body_snapshot);
            }

            join_tracked_bindings(
                env,
                expr,
                &loop_snapshot,
                &exit_snapshots,
                "loop join",
            )?;
            let merged_loop_snapshot = control_flow_snapshot(env);
            env.pop_scope();
            restore_control_flow_snapshot(env, &outer_snapshot);

            for (name, _) in outer_snapshot.tracked_bindings.iter() {
                if let Some(binding) = merged_loop_snapshot.tracked_bindings.get(name) {
                    env.tracked_bindings.insert(name.clone(), binding.clone());
                }
            }

            TypecheckResult::expr2(MIRExpression {
                source_range: None,
                kind: MIRExpressionKind::While {
                    condition: Box::new(condition_result),
                    body: Box::new(body_result.expression),
                    pre_eval: *pre_eval,
                },
                _type: cx_mir::mir::types::MIRType::unit(),
            })
        }

        CXExprKind::For {
            init,
            condition,
            increment,
            body,
        } => {
            let outer_snapshot = control_flow_snapshot(env);
            env.push_scope(true, true);

            let init_result = typecheck_expr(env, base_data, init, None)?.into_expression();
            let condition_result = typecheck_expr(env, base_data, condition, None)
                .and_then(|c| coerce_condition(env, expr, c.into_expression()))?;
            let loop_snapshot = control_flow_snapshot(env);
            let body_result = typecheck_expr(env, base_data, body, None)?.into_expression();
            let increment_result =
                typecheck_expr(env, base_data, increment, None)?.into_expression();
            let loop_exit_snapshot = control_flow_snapshot(env);
            let mut exit_snapshots = vec![loop_snapshot.clone()];

            if expr_may_fall_through(&body_result) && expr_may_fall_through(&increment_result) {
                exit_snapshots.push(loop_exit_snapshot);
            }

            join_tracked_bindings(
                env,
                expr,
                &loop_snapshot,
                &exit_snapshots,
                "loop join",
            )?;
            let merged_loop_snapshot = control_flow_snapshot(env);
            env.pop_scope();
            restore_control_flow_snapshot(env, &outer_snapshot);

            for (name, _) in outer_snapshot.tracked_bindings.iter() {
                if let Some(binding) = merged_loop_snapshot.tracked_bindings.get(name) {
                    env.tracked_bindings.insert(name.clone(), binding.clone());
                }
            }

            TypecheckResult::expr2(MIRExpression {
                source_range: None,
                kind: MIRExpressionKind::For {
                    init: Box::new(init_result),
                    condition: Box::new(condition_result),
                    increment: Box::new(increment_result),
                    body: Box::new(body_result),
                },
                _type: cx_mir::mir::types::MIRType::unit(),
            })
        }

        CXExprKind::Break => {
            let Some(scope_idx) = env
                .scope_stack
                .iter()
                .rposition(|inner| inner.has_break_merge)
            else {
                return log_typecheck_error!(
                    env,
                    expr,
                    " 'break' used outside of a loop or switch context"
                );
            };

            TypecheckResult::expr2(MIRExpression {
                source_range: None,
                kind: MIRExpressionKind::Break {
                    scope_depth: scope_idx,
                },
                _type: MIRType::unit(),
            })
        }

        CXExprKind::Continue => {
            let Some(scope_idx) = env
                .scope_stack
                .iter()
                .rposition(|inner| inner.has_continue_merge)
            else {
                return log_typecheck_error!(
                    env,
                    expr,
                    " 'continue' used outside of a loop context"
                );
            };

            TypecheckResult::expr2(MIRExpression {
                source_range: None,
                kind: MIRExpressionKind::Continue {
                    scope_depth: scope_idx,
                },
                _type: MIRType::unit(),
            })
        }

        CXExprKind::Return { value } => {
            let return_type = env.current_function().return_type.clone();

            let value_tc = value
                .as_ref()
                .map(|v| typecheck_expr(env, base_data, v, Some(&return_type)))
                .transpose()?;

            let value = match (value_tc, &return_type) {
                (Some(mut some_value), return_type) if !return_type.is_unit() => {
                    let mut _ty = some_value.expression._type.clone();

                    // If we are returning a copyable struct T, and we are given a &T, we can inline a bit
                    // of the implicit cast behavior here so instead of creating a temporary buffer to copy
                    // into, and then memcpy from that buffer, we can just "unsafely" coerce the &T to a T
                    // so we will induce in effect just a direct memcpy from the source T to the return buffer.
                    if let Some(inner) = _ty.mem_ref_inner()
                        && env.is_copyable(inner)
                        && return_type.is_memory_resident()
                    {
                        some_value = TypecheckResult::expr(
                            inner.clone(),
                            MIRExpressionKind::Typechange(Box::new(some_value.into_expression())),
                        );
                    }

                    Some(Box::new(implicit_cast(
                        env,
                        expr,
                        some_value.into_expression(),
                        return_type,
                    )?))
                }

                (None, _) if return_type.is_unit() => None,

                (Some(_), _) => {
                    return log_typecheck_error!(
                        env,
                        expr,
                        " Cannot return from function {} with a void return type",
                        env.current_function()
                    );
                }

                (None, _) => {
                    return log_typecheck_error!(
                        env,
                        expr,
                        " Function {} expects a return value, but none was provided",
                        env.current_function()
                    );
                }
            };

            assert_return_nodestruct_discharged(env, expr)?;
            TypecheckResult::expr(MIRType::unit(), MIRExpressionKind::Return { value })
        }

        CXExprKind::Defer { expr: _ } => {
            todo!()
        }

        CXExprKind::Unsafe { expr: inner } => {
            env.unsafe_depth += 1;
            let inner_result = typecheck_expr(env, base_data, inner, expected_type)?;
            env.unsafe_depth -= 1;

            TypecheckResult::expr2(MIRExpression {
                source_range: None,
                _type: inner_result.get_type(),
                kind: MIRExpressionKind::Unsafe {
                    expression: Box::new(inner_result.into_expression()),
                },
            })
        }

        CXExprKind::Leak { expr: inner } => {
            let CXExprKind::Identifier(ident) = &inner.kind else {
                return log_typecheck_error!(
                    env,
                    expr,
                    " @leak currently requires a local identifier"
                );
            };

            let Some(value) = env.symbol_value(ident.as_str()) else {
                return log_typecheck_error!(env, expr, " Identifier '{}' not found", ident);
            };
            let value = value.clone();

            let Some(inner_type) = value._type.mem_ref_inner() else {
                return log_typecheck_error!(
                    env,
                    expr,
                    " @leak requires a stack local value"
                );
            };

            if !env.is_nodestruct(inner_type) {
                return log_typecheck_error!(
                    env,
                    expr,
                    " @leak is only valid for nodestruct locals"
                );
            }

            ensure_binding_available(env, inner, ident)?;
            let leaked = value.clone();
            env.set_tracked_binding_state(ident.as_str(), BindingMoveState::Moved);

            TypecheckResult::expr(MIRType::unit(), MIRExpressionKind::LeakLifetime {
                expression: Box::new(leaked),
            })
        }

        CXExprKind::UnOp { operator, operand } => {
            match operator {
                CXUnOp::PreIncrement(increment_amount)
                | CXUnOp::PostIncrement(increment_amount) => {
                    let operand_val =
                        typecheck_expr(env, base_data, operand, None)?.into_expression();
                    let operand_type = operand_val.get_type();

                    let Some(inner) = operand_type.mem_ref_inner() else {
                        return log_typecheck_error!(
                            env,
                            operand,
                            " Cannot apply pre-increment to a non-reference {}",
                            operand_type
                        );
                    };

                    match &inner.kind {
                        MIRTypeKind::PointerTo { .. } | MIRTypeKind::Integer { .. } => {
                            match operator {
                                CXUnOp::PreIncrement(_) => TypecheckResult::expr(
                                    operand_type.clone(),
                                    MIRExpressionKind::UnaryOperation {
                                        op: MIRUnOp::PreIncrement(*increment_amount),
                                        operand: Box::new(operand_val),
                                    },
                                ),
                                CXUnOp::PostIncrement(_) => TypecheckResult::expr(
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
                                operand,
                                " Pre-increment operator requires an integer or pointer type, found {}",
                                inner
                            );
                        }
                    }
                }

                CXUnOp::LNot => {
                    let operand_val = typecheck_expr(env, base_data, operand, None)?;
                    let loaded_operand = coerce_value(env, expr, operand_val.into_expression())?;
                    let loaded_operand_type = loaded_operand.get_type();

                    if !loaded_operand_type.is_integer() {
                        return log_typecheck_error!(
                            env,
                            operand,
                            " Logical NOT operator requires an integer type, found {}",
                            loaded_operand_type
                        );
                    }

                    TypecheckResult::unary_op(
                        TypecheckResult::expr2(loaded_operand),
                        MIRUnOp::LNOT,
                        MIRTypeKind::Integer {
                            _type: MIRIntegerType::I1,
                            signed: false,
                        }
                        .into(),
                    )
                }

                CXUnOp::BNot => {
                    let operand_val = typecheck_expr(env, base_data, operand, None)?;
                    let mut loaded_op_val = coerce_value(env, expr, operand_val.into_expression())?;
                    let loaded_op_type = loaded_op_val.get_type();

                    if !loaded_op_type.is_integer() {
                        return log_typecheck_error!(
                            env,
                            operand,
                            " Bitwise NOT operator requires an integer type, found {}",
                            loaded_op_type
                        );
                    }

                    // If the type is I1 (boolean), we must promote to I32 first
                    if let MIRTypeKind::Integer {
                        _type: MIRIntegerType::I1,
                        ..
                    } = loaded_op_type.kind
                    {
                        loaded_op_val = implicit_cast(
                            env,
                            expr,
                            loaded_op_val.clone(),
                            &MIRType::from(MIRTypeKind::Integer {
                                _type: MIRIntegerType::I32,
                                signed: true,
                            }),
                        )?;
                    }

                    let result_type = loaded_op_val.get_type();

                    TypecheckResult::unary_op(
                        TypecheckResult::expr2(loaded_op_val),
                        MIRUnOp::BNOT,
                        result_type,
                    )
                }

                CXUnOp::Negative => {
                    let operand_val = typecheck_expr(env, base_data, operand, None)?;
                    let loaded_op_val = coerce_value(env, expr, operand_val.into_expression())?;
                    let loaded_op_type = loaded_op_val.get_type();

                    let operator = match &loaded_op_type.kind {
                        MIRTypeKind::Integer { .. } => MIRUnOp::NEG,
                        MIRTypeKind::Float { .. } => MIRUnOp::FNEG,

                        _ => {
                            return log_typecheck_error!(
                                env,
                                operand,
                                " Negation operator requires an integer or float type, found {}",
                                loaded_op_type
                            );
                        }
                    };

                    TypecheckResult::unary_op(
                        TypecheckResult::expr2(loaded_op_val),
                        operator,
                        loaded_op_type,
                    )
                }

                CXUnOp::AddressOf => {
                    let operand_val = typecheck_expr(env, base_data, operand, None)?;
                    let operand_type = operand_val.get_type();
                    let Some(inner) = operand_type.mem_ref_inner() else {
                        return log_typecheck_error!(
                            env,
                            operand,
                            " Cannot take address of a non-reference type"
                        );
                    };

                    // AddressOf just returns the operand (which is a reference) as a pointer
                    TypecheckResult::expr2(MIRExpression {
                        source_range: None,
                        kind: operand_val.into_expression().kind,
                        _type: inner.clone().pointer_to(),
                    })
                }

                CXUnOp::Dereference => {
                    // If the operand is a memory reference of a pointer type, we need to load the value first
                    let loaded_operand = typecheck_expr(env, base_data, operand, None)
                        .and_then(|v| coerce_value(env, expr, v.into_expression()))?;
                    let loaded_operand_type = loaded_operand.get_type();

                    let Some(inner) = loaded_operand_type.ptr_inner().cloned() else {
                        return log_typecheck_error!(
                            env,
                            operand,
                            " Cannot dereference a non-pointer type {}",
                            loaded_operand_type
                        );
                    };

                    // Dereference returns a memory reference to the inner type
                    TypecheckResult::expr2(MIRExpression {
                        source_range: None,
                        kind: MIRExpressionKind::Typechange(Box::new(loaded_operand)),
                        _type: inner.mem_ref_to(),
                    })
                }

                CXUnOp::ExplicitCast(to_type) => {
                    let to_type = env.complete_type(base_data, to_type)?;
                    let operand_val = typecheck_expr(env, base_data, operand, Some(&to_type))?;

                    TypecheckResult {
                        expression: explicit_cast(
                            env,
                            expr,
                            operand_val.into_expression(),
                            &to_type,
                        )?,
                    }
                }
            }
        }

        CXExprKind::BinOp {
            op: CXBinOp::Assign(op),
            lhs,
            rhs,
        } => {
            let lhs_val = if op.is_none() {
                if let CXExprKind::Identifier(name) = &lhs.kind {
                    if let Some(symbol_val) = env.symbol_value(name.as_str()) {
                        symbol_val.clone()
                    } else {
                        typecheck_expr(env, base_data, lhs, None)?.into_expression()
                    }
                } else {
                    typecheck_expr(env, base_data, lhs, None)?.into_expression()
                }
            } else {
                typecheck_expr(env, base_data, lhs, None)?.into_expression()
            };
            let lhs_type = lhs_val.get_type();

            let Some(inner) = lhs_type.mem_ref_inner() else {
                return log_typecheck_error!(
                    env,
                    expr,
                    " Cannot assign to non-reference type {}",
                    lhs_type
                );
            };

            let mut rhs_val = typecheck_expr(env, base_data, rhs, Some(inner))?;

            if let Some(op) = op {
                let loaded_lhs = coerce_value(env, expr, lhs_val.clone())?;
                let loaded_rhs = coerce_value(env, expr, rhs_val.into_expression())?;

                rhs_val = typecheck_binop_mir_vals(env, *op.clone(), loaded_lhs, loaded_rhs, expr)?;
            }

            if inner.get_specifier(CX_CONST) {
                return log_typecheck_error!(env, expr, " Cannot assign to a const type");
            }

            let coerced_rhs_val = implicit_cast(env, expr, rhs_val.into_expression(), inner)?;

            if op.is_none()
                && let CXExprKind::Identifier(name) = &lhs.kind
            {
                env.set_tracked_binding_state(name.as_str(), BindingMoveState::Available);
            }

            TypecheckResult::expr(
                lhs_val.get_type(),
                MIRExpressionKind::MemoryWrite {
                    target: Box::new(lhs_val),
                    value: Box::new(coerced_rhs_val),
                },
            )
        }

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
            typecheck_binop(env, base_data, op.clone(), lhs, rhs, expr)?
        }

        CXExprKind::Move {
            expr: inner_expr, ..
        } => {
            let CXExprKind::Identifier(ident) = &inner_expr.kind else {
                return log_typecheck_error!(
                    env,
                    expr,
                    "Move expressions can currently only be applied to stack variable indentifiers"
                );
            };

            let Some(inner_val) = env.symbol_table.get(ident.as_str()) else {
                return log_typecheck_error!(env, expr, " Identifier '{}' not found", ident);
            };
            let inner_val = inner_val.clone();

            if !matches!(inner_val.kind, MIRExpressionKind::Variable(_)) {
                return log_typecheck_error!(
                    env,
                    expr,
                    "Move expressions can currently only be applied to stack variable indentifiers"
                );
            }

            let Some(inner_type) = inner_val._type.mem_ref_inner().cloned() else {
                unreachable!()
            };

            if env.is_nocopy(&inner_type) {
                ensure_binding_available(env, inner_expr, ident)?;
                env.set_tracked_binding_state(ident.as_str(), BindingMoveState::Moved);
            }

            TypecheckResult::expr(
                inner_type,
                MIRExpressionKind::Move {
                    source: Box::new(inner_val),
                },
            )
        }

        CXExprKind::New { _type } => {
            todo!(
                "Intrinsic strong pointers are no longer supported, new semantics for 'new' tbd."
            );
        }

        CXExprKind::InitializerList { indices } => {
            typecheck_initializer_list(env, base_data, expr, indices, expected_type)?
        }

        CXExprKind::TypeConstructor {
            union_name: type_name,
            variant_name: name,
            inner,
        } => {
            let union_type = env.get_type(base_data, type_name.as_str())?;
            let MIRTypeKind::TaggedUnion { variants, .. } = &union_type.kind else {
                return log_typecheck_error!(env, expr, " Unknown type: {}", type_name);
            };

            let Some((i, variant_type)) = variants
                .iter()
                .enumerate()
                .find(|(_, (variant_name, _))| variant_name == name.as_str())
                .map(|(i, (_, variant_type))| (i, variant_type.clone()))
            else {
                return log_typecheck_error!(
                    env,
                    expr,
                    " Variant '{}' not found in tagged union type {}",
                    name,
                    type_name
                );
            };

            let inner = typecheck_expr(env, base_data, inner, Some(&variant_type))
                .and_then(|v| implicit_cast(env, expr, v.into_expression(), &variant_type))?;

            let allocation = TypecheckResult::expr(
                union_type.clone(),
                MIRExpressionKind::CreateStackVariable {
                    name: None,
                    _type: union_type.clone(),
                    initial_value: None,
                },
            );

            TypecheckResult::expr(
                union_type.clone().mem_ref_to(),
                MIRExpressionKind::TaggedUnionSet {
                    target: Box::new(allocation.into_expression()),
                    variant_index: i,
                    inner_value: Box::new(inner),
                    sum_type: union_type.clone(),
                },
            )
        }

        CXExprKind::Unit => TypecheckResult::expr2(MIRExpression {
            source_range: None,
            kind: MIRExpressionKind::Unit,
            _type: cx_mir::mir::types::MIRType::unit(),
        }),

        CXExprKind::SizeOf { expr } => {
            let tc_expr = typecheck_expr(env, base_data, expr, None)?;
            let tc_type = tc_expr.get_type();

            TypecheckResult::expr2(MIRExpression {
                source_range: None,
                kind: MIRExpressionKind::IntLiteral(
                    tc_type.type_size() as i64,
                    MIRIntegerType::I64,
                    false,
                ),
                _type: cx_mir::mir::types::MIRType::from(MIRTypeKind::Integer {
                    _type: MIRIntegerType::I64,
                    signed: false,
                }),
            })
        }

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
        } => typecheck_match(env, base_data, expr, condition, arms, default.as_ref())?,

        CXExprKind::Taken => {
            unreachable!("Taken expressions should not be present in the typechecker")
        }
    };

    if result.expression.source_range.is_none() {
        result.expression.source_range = Some(MIRSourceRange {
            start_token: expr.start_index,
            end_token: expr.end_index,
        });
    }

    validate_safe_contract_expression(env, expr, &result.expression)?;
    validate_safe_expression(env, expr, &result.expression)?;

    Ok(result)
}

pub(crate) fn global_expr(
    env: &mut TypeEnvironment,
    base_data: &MIRBaseMappings,
    ident: &str,
) -> CXResult<MIRExpression> {
    if let Some(global) = env.realized_globals.get(ident) {
        return tcglobal_expr(global);
    }

    let Some(module_res) = base_data.global_variables.get(ident) else {
        return CXError::create_result(format!("Global variable '{}' not found", ident));
    };

    let module_res = match env.in_external_templated_function {
        true => module_res.clone().transfer(""),
        false => module_res.clone(),
    };

    match &module_res.resource {
        CXGlobalVariable::EnumConstant(val) => Ok(MIRExpression {
            source_range: None,
            kind: MIRExpressionKind::IntLiteral(
                *val as i64,
                MIRIntegerType::from_bytes(8).unwrap(),
                true,
            ),
            _type: MIRType::from(MIRTypeKind::Integer {
                _type: MIRIntegerType::from_bytes(8).unwrap(),
                signed: true,
            }),
        }),

        CXGlobalVariable::Standard {
            _type,
            initializer,
            is_mutable,
        } => {
            let _type = env.complete_type(base_data, _type)?;
            let _initializer = match initializer.as_ref() {
                Some(init_expr) => {
                    let CXExprKind::IntLiteral { val, .. } = &init_expr.kind else {
                        return log_typecheck_error!(
                            env,
                            init_expr,
                            " CX currently only supports integer initializers for global variable initialization"
                        );
                    };

                    Some(*val)
                }

                None => None,
            };

            env.realized_globals.insert(
                ident.to_string(),
                MIRGlobalVariable {
                    kind: MIRGlobalVarKind::Variable {
                        name: CXIdent::new(ident.to_string()),
                        initializer: _initializer,
                        _type,
                    },
                    is_mutable: *is_mutable,
                    linkage: module_res.linkage,
                },
            );

            tcglobal_expr(env.realized_globals.get(ident).unwrap())
        }
    }
}

fn tcglobal_expr(global: &MIRGlobalVariable) -> CXResult<MIRExpression> {
    match &global.kind {
        MIRGlobalVarKind::Variable { name, _type, .. } => Ok(MIRExpression {
            source_range: None,
            kind: MIRExpressionKind::Variable(name.clone()),
            _type: _type.clone().mem_ref_to(),
        }),

        MIRGlobalVarKind::StringLiteral { .. } => {
            unreachable!("String literals cannot be referenced via an identifier")
        }
    }
}

pub fn add_implicit_return(
    env: &mut TypeEnvironment,
    expr: MIRExpression,
) -> CXResult<MIRExpression> {
    if let MIRExpressionKind::Block { statements } = &expr.kind {
        if let Some(MIRExpression {
            kind: MIRExpressionKind::Return { .. },
            ..
        }) = statements.last()
        {
            return Ok(expr);
        }
    }

    let func = env.current_function().clone();

    let implicit_value = if func.name.as_str() == "main" {
        Some(Box::new(MIRExpression {
            source_range: None,
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
            &CXExpr::default(),
            "Function '{}' with non-void return type must have an explicit return statement",
            func.name
        );
    };

    Ok(MIRExpression {
        source_range: None,
        kind: MIRExpressionKind::Block {
            statements: vec![
                expr,
                MIRExpression {
                    source_range: None,
                    kind: MIRExpressionKind::Return {
                        value: implicit_value,
                    },
                    _type: MIRType::unit(),
                },
            ],
        },
        _type: MIRType::unit(),
    })
}
