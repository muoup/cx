use cx_ast::ast::expression::{CXExprKind, CXExpression};
use cx_log::CXResult;
use cx_mir::mir::expression::MIRExpression;
use cx_mir::type_context::MIRTypeContext;
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
        result::{ComptimeFunctionValue, TypecheckResult},
        typechecker::typecheck_expr,
    },
};

pub(crate) mod engine;
pub(crate) mod evaluation;
pub(crate) mod value;

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
    args: Vec<TypecheckResult>,
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

        for (param, arg) in function.prototype.params().iter().zip(args.into_iter()) {
            let Some(name) = param.name.clone() else {
                continue;
            };

            if !param.value_type.expr {
                return env.log_error(
                    call_range,
                    format!("Non-expr comptime parameters are not implemented yet"),
                );
            }

            let staged = coerce_staged_argument(env, call_range, arg, &param.value_type._type)?;
            env.symbols
                .insert_local_value(QualifiedName::new_raw(name), staged);
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
                    format!("Comptime expr function must return an emitted expression"),
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

fn coerce_staged_argument(
    env: &mut TypeEnvironment,
    call_range: &cx_tokens::TokenRange,
    arg: TypecheckResult,
    target_type: &cx_mir::mir::data::MIRType,
) -> CXResult<MIRExpression> {
    let arg = arg.standard_ready_coerce(env, call_range)?;

    if env
        .symbols
        .mem_ref_inner(&arg._type)
        .is_some_and(|inner| env.type_eq(inner, target_type))
        || env.type_eq(&arg._type, target_type)
    {
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
        CXExprKind::Block { exprs } if exprs.len() == 1 => &exprs[0],
        CXExprKind::Block { .. } => {
            return env.log_error(
                body.token_range(),
                format!("Only single-expression comptime function bodies are implemented"),
            );
        }
        _ => body,
    };

    let expr = match &expr.kind {
        CXExprKind::Return { value: Some(value) } => value.as_ref(),
        CXExprKind::Return { value: None } => {
            return env.log_error(
                expr.token_range(),
                format!("Comptime function return requires a value"),
            );
        }
        _ => expr,
    };

    typecheck_expr(env, namespace, expr, Some(expected_type))?
        .standard_ready_coerce(env, expr.token_range())
}
