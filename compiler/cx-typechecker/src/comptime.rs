use cx_ast::ast::expression::{CXExprKind, CXExpression};
use cx_log::CXResult;
use cx_mir::mir::{data::MIRComptimeValueType, expression::MIRExpression};
use cx_util::namespace::QualifiedName;

use crate::{
    comptime::{
        engine::ComptimeEngine,
        evaluation::evaluate_expression,
        value::{ComptimeKind, ComptimeValue},
    },
    environment::TypeEnvironment,
    type_checking::{
        coercion::implicit::{implicit_cast, promotion::std_rval_promotion},
        result::{ComptimeFunctionValue, StagedFunctionValue, TypecheckResult},
        typechecker::typecheck_expr,
    },
};

pub(crate) mod engine;
pub(crate) mod evaluation;
pub(crate) mod value;

pub(crate) enum ComptimeCallArg<'a> {
    Mir(MIRExpression),
    Source {
        namespace: &'a cx_mir::EnvironmentNamespace,
        expr: &'a CXExpression,
    },
}

pub fn evaluate_comptime_expression(
    env: &mut TypeEnvironment,
    expr: MIRExpression,
) -> CXResult<ComptimeValue> {
    evaluate_expression(&mut ComptimeEngine::new(env), expr)
}

pub(crate) fn evaluate_comptime_call(
    env: &mut TypeEnvironment,
    call_range: &cx_tokens::TokenRange,
    function: ComptimeFunctionValue,
    args: Vec<ComptimeCallArg>,
) -> CXResult<TypecheckResult> {
    if args.len() != function.prototype.params().len() {
        return env.log_error(
            call_range,
            format!(
                "Comptime call expects {} arguments, found {}",
                function.prototype.params().len(),
                args.len()
            ),
        );
    }

    env.symbols.push_local_scope();
    let result = (|| {
        for (name, type_id) in function.template_bindings.iter() {
            env.symbols
                .insert_local_type_id(name.as_string(), *type_id)
                .map_err(|err| env.complete_err(err, call_range))?;
        }

        for (param, arg) in function.prototype.params().iter().zip(args) {
            let Some(name) = param.name.clone() else {
                continue;
            };

            if !param.value_type.expr {
                return env.log_error(
                    call_range,
                    "Non-expr comptime parameters are not implemented yet".to_string(),
                );
            }

            match arg {
                ComptimeCallArg::Mir(arg) => {
                    if !param.value_type.params.is_empty() {
                        return env.log_error(
                            call_range,
                            "Parameterized staged expressions require |parameters| syntax"
                                .to_string(),
                        );
                    }
                    let staged = coerce_staged_argument(
                        env,
                        call_range,
                        TypecheckResult::from(arg),
                        &param.value_type._type,
                    )?;
                    env.symbols
                        .insert_local_value(QualifiedName::new_raw(name), staged);
                }
                ComptimeCallArg::Source { namespace, expr } => {
                    check_staged_source_argument(
                        env,
                        namespace,
                        call_range,
                        expr,
                        &param.value_type,
                    )?;
                    if param.value_type.params.is_empty() {
                        let staged_id = env.next_staged_expression_id();
                        env.symbols.insert_local_staged_expression(
                            staged_id,
                            QualifiedName::new_raw(name),
                            namespace.clone(),
                            expr.clone(),
                            param.value_type._type.clone(),
                        );
                    } else {
                        let CXExprKind::StagedExpression {
                            params: source_params,
                            body,
                        } = &expr.kind
                        else {
                            unreachable!("staged expression shape was checked above")
                        };
                        let params = source_params
                            .iter()
                            .cloned()
                            .zip(param.value_type.params.iter().cloned())
                            .collect();
                        env.symbols.insert_local_staged_expression_function(
                            QualifiedName::new_raw(name),
                            namespace.clone(),
                            params,
                            body.as_ref().clone(),
                            param.value_type._type.clone(),
                        );
                    }
                }
            }
        }

        env.enter_comptime_context();
        let body = typecheck_comptime_body(
            env,
            &function.namespace,
            &function.body,
            &function.prototype.return_type()._type,
        );
        env.exit_comptime_context();

        let body = body?;
        let value = evaluate_comptime_expression(env, body)?;

        if function.prototype.return_type().expr {
            let ComptimeKind::Emit(expr) = value.kind else {
                return env.log_error(
                    call_range,
                    "Comptime expr function must return an emitted expression".to_string(),
                );
            };

            Ok(TypecheckResult::from(expr))
        } else {
            Ok(TypecheckResult::comptime_value(value))
        }
    })();
    env.symbols.pop_local_scope();

    result
}

fn check_staged_source_argument(
    env: &mut TypeEnvironment,
    namespace: &cx_mir::EnvironmentNamespace,
    call_range: &cx_tokens::TokenRange,
    expr: &CXExpression,
    target: &MIRComptimeValueType,
) -> CXResult<()> {
    if !target.params.is_empty() {
        let CXExprKind::StagedExpression { params, .. } = &expr.kind else {
            return env.log_error(
                call_range,
                "Expected a parameterized staged expression written as |parameters| expression"
                    .to_string(),
            );
        };
        if params.len() != target.params.len() {
            return env.log_error(
                call_range,
                format!(
                    "Staged expression expects {} parameters, found {}",
                    target.params.len(),
                    params.len()
                ),
            );
        }
        return Ok(());
    }

    let scope = env.function.current_scope_index();
    let reachable = env.function.is_current_scope_reachable();
    let snapshot = env.function.current_snapshot();

    let result = (|| {
        let arg = typecheck_expr(env, namespace, expr, Some(&target._type))?;
        coerce_staged_argument(env, call_range, arg, &target._type).map(|_| ())
    })();

    env.function.restore_snapshot(&snapshot);
    env.function.set_scope_reachable(scope, reachable);

    result
}

pub(crate) fn evaluate_staged_expression_call(
    env: &mut TypeEnvironment,
    call_range: &cx_tokens::TokenRange,
    function: StagedFunctionValue,
    argument_namespace: &cx_mir::EnvironmentNamespace,
    args: Vec<&CXExpression>,
) -> CXResult<TypecheckResult> {
    if args.len() != function.params.len() {
        return env.log_error(
            call_range,
            format!(
                "Staged expression expects {} arguments, found {}",
                function.params.len(),
                args.len()
            ),
        );
    }

    env.symbols.push_local_scope();
    let result = (|| {
        for ((name, target_type), arg) in function.params.iter().zip(args) {
            let arg = typecheck_expr(env, argument_namespace, arg, Some(target_type))?;
            let arg = coerce_staged_argument(env, call_range, arg, target_type)?;
            env.symbols
                .insert_local_value(QualifiedName::new_raw(name.clone()), arg);
        }

        let body = typecheck_expr(
            env,
            &function.namespace,
            &function.body,
            Some(&function.return_type),
        )?;
        let body = coerce_staged_argument(env, call_range, body, &function.return_type)?;
        Ok(TypecheckResult::from(body))
    })();
    env.symbols.pop_local_scope();
    result
}

fn coerce_staged_argument(
    env: &mut TypeEnvironment,
    call_range: &cx_tokens::TokenRange,
    arg: TypecheckResult,
    target_type: &cx_mir::mir::data::MIRType,
) -> CXResult<MIRExpression> {
    let arg = arg.standard_ready_coerce(env, call_range)?;

    if env.type_eq(&arg._type, target_type) {
        return Ok(arg);
    }

    let arg = std_rval_promotion(env, arg)?;
    implicit_cast(env, arg, target_type)
}

fn typecheck_comptime_body(
    env: &mut TypeEnvironment,
    namespace: &cx_mir::EnvironmentNamespace,
    body: &CXExpression,
    expected_type: &cx_mir::mir::data::MIRType,
) -> CXResult<MIRExpression> {
    let expr = match &body.kind {
        CXExprKind::Block { exprs, .. } if exprs.len() == 1 => &exprs[0],
        CXExprKind::Block { .. } => {
            return env.log_error(
                body.token_range(),
                "Only single-expression comptime function bodies are implemented".to_string(),
            );
        }
        _ => body,
    };

    let expr = match &expr.kind {
        CXExprKind::Return { value: Some(value) } => value.as_ref(),
        CXExprKind::Return { value: None } => {
            return env.log_error(
                expr.token_range(),
                "Comptime function return requires a value".to_string(),
            );
        }
        _ => expr,
    };

    typecheck_expr(env, namespace, expr, Some(expected_type))?
        .standard_ready_coerce(env, expr.token_range())
}
