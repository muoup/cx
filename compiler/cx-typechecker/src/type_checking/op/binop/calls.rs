use crate::comptime::evaluate_comptime_call;
use crate::environment::TypeEnvironment;
use crate::symbol::deduction::complete_templated_callee_maybe;
use crate::type_checking::coercion::implicit::implicit_cast;
use crate::type_checking::coercion::implicit::promotion::lvalue;
use crate::type_checking::coercion::implicit::promotion::std_rval_promotion;
use crate::type_checking::contracts::typecheck_contract;
use crate::type_checking::result::{ComptimeTypecheckValue, TypecheckExtract, TypecheckResult};
use crate::type_checking::typechecker::typecheck_expr;
use cx_ast::ast::expression::{CXBinOp, CXExprKind, CXExpression};
use cx_log::CXResult;
use cx_mir::EnvironmentNamespace;
use cx_mir::mir::data::{MIRFloatType, MIRFunctionSignature, MIRType, MIRTypeKind};
use cx_mir::mir::expression::{MIRExpression, MIRExpressionKind};
use cx_mir::type_context::MIRTypeContext;

pub(crate) fn typecheck_method_call(
    env: &mut TypeEnvironment,
    namespace: &EnvironmentNamespace,
    lhs: &CXExpression,
    rhs: &CXExpression,
    expr: &CXExpression,
    expected_type: Option<&MIRType>,
) -> CXResult<TypecheckResult> {
    let function = typecheck_expr(env, namespace, lhs, None)?;

    typecheck_callee_method_call(env, namespace, function, vec![], rhs, expr, expected_type)
}

pub(crate) fn typecheck_callee_method_call(
    env: &mut TypeEnvironment,
    namespace: &EnvironmentNamespace,
    callee: TypecheckResult,
    implicit_args: Vec<MIRExpression>,
    rhs: &CXExpression,
    expr: &CXExpression,
    expected_type: Option<&MIRType>,
) -> CXResult<TypecheckResult> {
    let tc_args = comma_separated(env, namespace, rhs)?;
    let callee = complete_callee(
        env,
        namespace,
        expr,
        callee,
        &implicit_args,
        &tc_args,
        expected_type,
    )?;
    let all_args = implicit_args
        .into_iter()
        .map(TypecheckResult::from)
        .chain(tc_args.into_iter().map(|(_, arg)| arg))
        .collect::<Vec<_>>();

    let CompletedCallee::Runtime(callee) = callee else {
        let CompletedCallee::Comptime(function) = callee else {
            unreachable!()
        };
        return evaluate_comptime_call(env, expr.token_range(), function, all_args);
    };

    let (loaded_function, signature) = load_callable(env, expr, callee)?;

    check_argument_count(env, expr, &signature, all_args.len())?;

    let argument_results = complete_call_arguments(env, namespace, &signature, all_args)?;
    let arguments = complete_call_argument_expressions(env, expr, &signature, argument_results)?;
    let contract = typecheck_contract(env, namespace, &signature)?;

    Ok(TypecheckResult::new(
        signature.return_type,
        MIRExpressionKind::CallFunction {
            function: Box::new(loaded_function),
            arguments,
            contract,
        },
    ))
}

fn load_callable(
    env: &mut TypeEnvironment,
    expr: &CXExpression,
    function: MIRExpression,
) -> CXResult<(MIRExpression, MIRFunctionSignature)> {
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
    expr: &CXExpression,
    signature: &MIRFunctionSignature,
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
    target_type: &MIRType,
) -> CXResult<TypecheckResult> {
    val.apply_expected_type(env, namespace, target_type)
}

fn coerce_fixed_argument(
    env: &mut TypeEnvironment,
    expr: &CXExpression,
    val: TypecheckResult,
    target_type: &MIRType,
) -> CXResult<MIRExpression> {
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
    expr: &CXExpression,
    val: TypecheckResult,
) -> CXResult<MIRExpression> {
    let mut val = val.standard_ready_coerce(env, expr.token_range())?;

    val = std_rval_promotion(env, val)?;
    let arg_type = val._type.clone();

    match &arg_type.kind {
        MIRTypeKind::PointerTo { .. } => {}
        MIRTypeKind::Integer { .. } => {}
        MIRTypeKind::Float {
            _type: MIRFloatType::F32,
        } => {
            val = implicit_cast(
                env,
                val,
                &MIRTypeKind::Float {
                    _type: MIRFloatType::F64,
                }
                .into(),
            )?;
        }
        MIRTypeKind::Float {
            _type: MIRFloatType::F64,
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
    signature: &MIRFunctionSignature,
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
    expr: &CXExpression,
    signature: &MIRFunctionSignature,
    args: Vec<TypecheckResult>,
) -> CXResult<Vec<MIRExpression>> {
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
    implicit_args: &[MIRExpression],
    args: &[(&CXExpression, TypecheckResult)],
) -> Vec<MIRType> {
    implicit_args
        .iter()
        .map(MIRExpression::get_type)
        .chain(args.iter().filter_map(|(_, arg)| arg.ready_type().cloned()))
        .collect()
}

fn complete_call_argument_expressions(
    env: &mut TypeEnvironment,
    expr: &CXExpression,
    signature: &MIRFunctionSignature,
    args: Vec<TypecheckResult>,
) -> CXResult<Vec<MIRExpression>> {
    coerce_call_arguments(env, expr, signature, args)
}

fn complete_callee(
    env: &mut TypeEnvironment,
    namespace: &EnvironmentNamespace,
    expr: &CXExpression,
    function: TypecheckResult,
    implicit_args: &[MIRExpression],
    args: &[(&CXExpression, TypecheckResult)],
    expected_type: Option<&MIRType>,
) -> CXResult<CompletedCallee> {
    match function.try_into_expression() {
        TypecheckExtract::Succ(callee) => Ok(CompletedCallee::Runtime(callee)),

        TypecheckExtract::Fail(function) => {
            let function = match function.try_into_comptime_value() {
                TypecheckExtract::Succ(ComptimeTypecheckValue::Function(function)) => {
                    return Ok(CompletedCallee::Comptime(function));
                }
                TypecheckExtract::Succ(_) => {
                    return env.log_error(
                        expr.token_range(),
                        format!("Comptime value is not callable"),
                    );
                }
                TypecheckExtract::Fail(function) => function,
            };

            let Some(parts) = function.into_incomplete_callee_parts() else {
                return env.log_error(expr.token_range(), format!("Could not deduce callee"));
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
                    TypecheckExtract::Succ(_) => env.log_error(
                        expr.token_range(),
                        format!("Comptime value is not callable"),
                    ),
                    TypecheckExtract::Fail(result) => match result.try_into_expression() {
                        TypecheckExtract::Succ(function) => Ok(CompletedCallee::Runtime(function)),
                        TypecheckExtract::Fail(_) => {
                            env.log_error(expr.token_range(), format!("Could not deduce callee"))
                        }
                    },
                },
                Err(err) => env.log_error(expr.token_range(), format!("{}", err.message())),
            }
        }
    }
}

enum CompletedCallee {
    Runtime(MIRExpression),
    Comptime(crate::type_checking::result::ComptimeFunctionValue),
}

pub(crate) fn comma_separated<'a>(
    env: &mut TypeEnvironment,
    namespace: &EnvironmentNamespace,
    expr: &'a CXExpression,
) -> CXResult<Vec<(&'a CXExpression, TypecheckResult)>> {
    let mut expr_iter = expr;
    let mut exprs = Vec::new();

    if matches!(expr.kind, CXExprKind::Unit) {
        return Ok(exprs);
    }

    while let CXExprKind::BinOp {
        lhs,
        rhs,
        op: CXBinOp::Comma,
    } = &expr_iter.kind
    {
        let tc_result = typecheck_expr(env, namespace, rhs, None)?;
        exprs.push((rhs, tc_result));
        expr_iter = lhs;
    }

    let tc_result = typecheck_expr(env, namespace, expr_iter, None)?;
    exprs.push((expr_iter, tc_result));
    exprs.reverse();

    Ok(exprs)
}
