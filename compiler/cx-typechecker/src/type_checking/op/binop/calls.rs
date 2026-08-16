use crate::comptime::{ComptimeCallArg, evaluate_comptime_call, evaluate_staged_expression_call};
use crate::environment::TypeEnvironment;
use crate::symbol::deduction::complete_templated_callee_maybe;
use crate::type_checking::coercion::implicit::implicit_cast;
use crate::type_checking::coercion::implicit::promotion::lvalue;
use crate::type_checking::coercion::implicit::promotion::std_rval_promotion;
use crate::type_checking::contracts::typecheck_contract;
use crate::type_checking::result::{ComptimeTypecheckValue, TypecheckExtract, TypecheckResult};
use crate::type_checking::typechecker::typecheck_expr;
use cx_hir::ast::expression::{HIRBinOp, HIRExprKind, HIRExpression};
use cx_log::CXResult;
use cx_thir::EnvironmentNamespace;
use cx_thir::thir::data::{THIRFloatType, THIRFnSignature, THIRType, THIRTypeKind};
use cx_thir::thir::expression::{THIRExpression, THIRExpressionKind};
use cx_thir::type_context::THIRTypeContext;

pub(crate) fn typecheck_method_call(
    env: &mut TypeEnvironment,
    namespace: &EnvironmentNamespace,
    lhs: &HIRExpression,
    rhs: &HIRExpression,
    expr: &HIRExpression,
    expected_type: Option<&THIRType>,
) -> CXResult<TypecheckResult> {
    let function = typecheck_expr(env, namespace, lhs, None)?;

    typecheck_callee_method_call(env, namespace, function, vec![], rhs, expr, expected_type)
}

pub(crate) fn typecheck_callee_method_call(
    env: &mut TypeEnvironment,
    namespace: &EnvironmentNamespace,
    callee: TypecheckResult,
    implicit_args: Vec<THIRExpression>,
    rhs: &HIRExpression,
    expr: &HIRExpression,
    expected_type: Option<&THIRType>,
) -> CXResult<TypecheckResult> {
    let raw_args = comma_separated_exprs(rhs);

    let scope = env.function.current_scope_index();
    let reachable = env.function.is_current_scope_reachable();
    let snapshot = env.function.current_snapshot();

    let tc_args = typecheck_args(env, namespace, raw_args.as_slice())?;

    env.function.restore_snapshot(&snapshot);
    env.function.set_scope_reachable(scope, reachable);

    let callee = complete_callee(
        env,
        namespace,
        expr,
        callee,
        &implicit_args,
        &tc_args,
        expected_type,
    )?;

    let callee = match callee {
        CompletedCallee::Staged(function) => {
            return evaluate_staged_expression_call(
                env,
                expr.token_range(),
                function,
                namespace,
                raw_args,
            );
        }
        callee => callee,
    };

    let CompletedCallee::Runtime(callee) = callee else {
        let CompletedCallee::Comptime(function) = callee else {
            unreachable!()
        };
        let all_args = implicit_args
            .into_iter()
            .map(ComptimeCallArg::Mir)
            .chain(raw_args.into_iter().map(|arg| ComptimeCallArg::Source {
                namespace,
                expr: arg,
            }))
            .collect::<Vec<_>>();
        return evaluate_comptime_call(env, expr.token_range(), function, all_args);
    };

    let (loaded_function, signature) = load_callable(env, expr, callee)?;

    check_argument_count(env, expr, &signature, implicit_args.len() + raw_args.len())?;

    let explicit_args = typecheck_args(env, namespace, &raw_args)?;
    let all_args = implicit_args
        .into_iter()
        .map(TypecheckResult::from)
        .chain(explicit_args.into_iter().map(|(_, arg)| arg))
        .collect::<Vec<_>>();

    let argument_results = complete_call_arguments(env, namespace, &signature, all_args)?;
    let arguments = complete_call_argument_expressions(env, expr, &signature, argument_results)?;
    let contract = typecheck_contract(env, namespace, &signature)?;

    Ok(TypecheckResult::new(
        signature.return_type,
        THIRExpressionKind::CallFunction {
            function: Box::new(loaded_function),
            arguments,
            contract,
        },
    ))
}

fn typecheck_args<'a>(
    env: &mut TypeEnvironment,
    namespace: &EnvironmentNamespace,
    args: &[&'a HIRExpression],
) -> CXResult<Vec<(&'a HIRExpression, TypecheckResult)>> {
    args.iter()
        .map(|arg| typecheck_expr(env, namespace, arg, None).map(|result| (*arg, result)))
        .collect()
}

fn load_callable(
    env: &mut TypeEnvironment,
    expr: &HIRExpression,
    function: THIRExpression,
) -> CXResult<(THIRExpression, THIRFnSignature)> {
    let loaded_function =
        lvalue::try_conversion(env, function)?.catch_unapplied(|expr, _| Ok(expr))?;
    let function_type = loaded_function.get_type();
    let Some(callable_type) = env
        .symbols
        .intern_signature(loaded_function.get_type_ref())
        .cloned()
    else {
        return env.log_error(
            expr.token_range(),
            format!(
                "Attempted to call value of non-function type {}",
                function_type.display_with(&env.symbols)
            ),
        );
    };

    Ok((loaded_function, callable_type))
}

fn check_argument_count(
    env: &TypeEnvironment,
    expr: &HIRExpression,
    signature: &THIRFnSignature,
    arg_count: usize,
) -> CXResult<()> {
    if arg_count != signature.params.len() && !signature.var_args {
        return env.log_error(
            expr.token_range(),
            format!(
                "Call to {} expects {} arguments, found {}",
                signature.display_with(&env.symbols),
                signature.params.len(),
                arg_count
            ),
        );
    }

    if arg_count < signature.params.len() {
        return env.log_error(
            expr.token_range(),
            format!(
                "Call to {} expects at least {} arguments, found {}",
                signature.display_with(&env.symbols),
                signature.params.len(),
                arg_count
            ),
        );
    }

    Ok(())
}

fn complete_fixed_argument(
    env: &mut TypeEnvironment,
    namespace: &EnvironmentNamespace,
    val: TypecheckResult,
    target_type: &THIRType,
) -> CXResult<TypecheckResult> {
    val.apply_expected_type(env, namespace, target_type)
}

fn coerce_fixed_argument(
    env: &mut TypeEnvironment,
    expr: &HIRExpression,
    val: TypecheckResult,
    target_type: &THIRType,
) -> CXResult<THIRExpression> {
    let val = val.standard_ready_coerce(env, expr.token_range())?;
    let val = if target_type.is_memory_reference() {
        val
    } else {
        std_rval_promotion(env, val)?
    };

    implicit_cast(env, val, target_type)
}

fn complete_vararg_argument(
    env: &mut TypeEnvironment,
    expr: &HIRExpression,
    val: TypecheckResult,
) -> CXResult<THIRExpression> {
    let mut val = val.standard_ready_coerce(env, expr.token_range())?;

    val = std_rval_promotion(env, val)?;
    let arg_type = val._type.clone();

    match &arg_type.kind {
        THIRTypeKind::PointerTo { .. } => {}
        THIRTypeKind::Integer { .. } => {}
        THIRTypeKind::Float {
            _type: THIRFloatType::F32,
        } => {
            val = implicit_cast(
                env,
                val,
                &THIRTypeKind::Float {
                    _type: THIRFloatType::F64,
                }
                .into(),
            )?;
        }
        THIRTypeKind::Float {
            _type: THIRFloatType::F64,
        } => {}
        _ => {
            return env.log_error(
                expr.token_range(),
                format!(
                    "Cannot pass {} to varargs: expected an intrinsic type or pointer",
                    arg_type.display_with(&env.symbols)
                ),
            );
        }
    }

    Ok(val)
}

fn complete_call_arguments(
    env: &mut TypeEnvironment,
    namespace: &EnvironmentNamespace,
    signature: &THIRFnSignature,
    args: Vec<TypecheckResult>,
) -> CXResult<Vec<TypecheckResult>> {
    args.into_iter()
        .enumerate()
        .map(|(i, val)| {
            if let Some(param) = signature.params.get(i) {
                complete_fixed_argument(env, namespace, val, &param._type)
            } else {
                Ok(val)
            }
        })
        .collect()
}

fn coerce_call_arguments(
    env: &mut TypeEnvironment,
    expr: &HIRExpression,
    signature: &THIRFnSignature,
    args: Vec<TypecheckResult>,
) -> CXResult<Vec<THIRExpression>> {
    let mut coerced_args = Vec::with_capacity(args.len());

    for (i, val) in args.into_iter().enumerate() {
        let val = if let Some(param) = signature.params.get(i) {
            coerce_fixed_argument(env, expr, val, &param._type)?
        } else {
            complete_vararg_argument(env, expr, val)?
        };

        coerced_args.push(val);
    }

    Ok(coerced_args)
}

fn deduction_arg_types(
    implicit_args: &[THIRExpression],
    args: &[(&HIRExpression, TypecheckResult)],
) -> Vec<THIRType> {
    implicit_args
        .iter()
        .map(THIRExpression::get_type)
        .chain(args.iter().filter_map(|(_, arg)| arg.ready_type().cloned()))
        .collect()
}

fn complete_call_argument_expressions(
    env: &mut TypeEnvironment,
    expr: &HIRExpression,
    signature: &THIRFnSignature,
    args: Vec<TypecheckResult>,
) -> CXResult<Vec<THIRExpression>> {
    coerce_call_arguments(env, expr, signature, args)
}

fn complete_callee(
    env: &mut TypeEnvironment,
    namespace: &EnvironmentNamespace,
    expr: &HIRExpression,
    function: TypecheckResult,
    implicit_args: &[THIRExpression],
    args: &[(&HIRExpression, TypecheckResult)],
    expected_type: Option<&THIRType>,
) -> CXResult<CompletedCallee> {
    match function.try_into_expression() {
        TypecheckExtract::Succ(callee) => Ok(CompletedCallee::Runtime(callee)),

        TypecheckExtract::Fail(function) => {
            let function = match function.try_into_comptime_value() {
                TypecheckExtract::Succ(ComptimeTypecheckValue::Function(function)) => {
                    return Ok(CompletedCallee::Comptime(function));
                }
                TypecheckExtract::Succ(ComptimeTypecheckValue::StagedFunction(function)) => {
                    return Ok(CompletedCallee::Staged(function));
                }
                TypecheckExtract::Succ(_) => {
                    return env.log_error(
                        expr.token_range(),
                        "Comptime value is not callable".to_string(),
                    );
                }
                TypecheckExtract::Fail(function) => function,
            };

            let Some(parts) = function.into_incomplete_callee_parts() else {
                return env.log_error(expr.token_range(), "Could not deduce callee".to_string());
            };

            let deduction_arg_types = deduction_arg_types(implicit_args, args);

            let symbol = match complete_templated_callee_maybe(
                env,
                namespace,
                &parts.name,
                parts.template_input.as_ref(),
                &deduction_arg_types,
                expected_type,
            ) {
                Ok(symbol) => symbol,
                Err(err) => {
                    return Err(env.complete_maybe_err(err, expr.token_range()));
                }
            };

            match TypecheckResult::from_symbol(symbol, parts.name, parts.template_input) {
                Ok(result) => match result.try_into_comptime_value() {
                    TypecheckExtract::Succ(ComptimeTypecheckValue::Function(function)) => {
                        Ok(CompletedCallee::Comptime(function))
                    }
                    TypecheckExtract::Succ(ComptimeTypecheckValue::StagedFunction(function)) => {
                        Ok(CompletedCallee::Staged(function))
                    }
                    TypecheckExtract::Succ(_) => env.log_error(
                        expr.token_range(),
                        "Comptime value is not callable".to_string(),
                    ),
                    TypecheckExtract::Fail(result) => match result.try_into_expression() {
                        TypecheckExtract::Succ(function) => Ok(CompletedCallee::Runtime(function)),
                        TypecheckExtract::Fail(_) => {
                            env.log_error(expr.token_range(), "Could not deduce callee".to_string())
                        }
                    },
                },
                Err(err) => env.log_error(expr.token_range(), err.message().to_string()),
            }
        }
    }
}

enum CompletedCallee {
    Runtime(THIRExpression),
    Comptime(crate::type_checking::result::ComptimeFunctionValue),
    Staged(crate::type_checking::result::StagedFunctionValue),
}

pub(crate) fn comma_separated_exprs(expr: &HIRExpression) -> Vec<&HIRExpression> {
    let mut expr_iter = expr;
    let mut exprs = Vec::new();

    if matches!(expr.kind, HIRExprKind::Void) {
        return exprs;
    }

    while let HIRExprKind::BinOp {
        lhs,
        rhs,
        op: HIRBinOp::Comma,
    } = &expr_iter.kind
    {
        exprs.push(rhs.as_ref());
        expr_iter = lhs;
    }

    exprs.push(expr_iter);
    exprs.reverse();

    exprs
}
