use crate::environment::{THIRFunctionGenRequest, TypeEnvironment};
use crate::symbol::deduction::complete_templated_callee_maybe;
use crate::symbol::name_mangling::base_mangle_templated_name;
use crate::type_checking::coercion::implicit::conversion::compatible;
use crate::type_checking::coercion::implicit::implicit_cast;
use crate::type_checking::coercion::implicit::promotion::lvalue;
use crate::type_checking::coercion::implicit::promotion::std_rval_promotion;
use crate::type_checking::contracts::typecheck_contract;
use crate::type_checking::result::{TypecheckExtract, TypecheckResult, TypecheckState};
use crate::type_checking::staged_expr::typecheck_staged_expr;
use crate::type_checking::typechecker::typecheck_expr;
use cx_hir::ast::expression::{HIRBinOp, HIRExprKind, HIRExpression};
use cx_log::CXResult;
use cx_thir::EnvironmentNamespace;
use cx_thir::thir::comptime::THIRStagedExpr;
use cx_thir::thir::data::{
    THIRComptimeFnPrototype, THIRComptimeValueType, THIRFloatType, THIRFnSignature,
    THIRTemplateInput, THIRType, THIRTypeKind,
};
use cx_thir::thir::expression::{THIRExpression, THIRExpressionKind, THIRFnContract, THIRLocalID};
use cx_thir::thir::r#type::THIRTypeID;
use cx_thir::type_context::THIRTypeContext;
use cx_tokens::TokenRange;
use cx_util::{identifier::CXIdent, namespace::QualifiedName};

enum CompletedCallee {
    Runtime(THIRExpression),
    Staged(THIRStagedExpr),
    Comptime {
        prototype: THIRComptimeFnPrototype,
        symbol_name: CXIdent,
    },
}

pub const BUILTIN_FNS: &[&str] = &[
    "__builtin_va_start",
    "__builtin_va_end",
    "__builtin_va_copy",
    "va_start",
    "va_end",
    "va_copy",
];

pub(crate) fn typecheck_method_call(
    env: &mut TypeEnvironment,
    namespace: &EnvironmentNamespace,
    lhs: &HIRExpression,
    rhs: &HIRExpression,
    expr: &HIRExpression,
    expected_type: Option<&THIRType>,
) -> CXResult<TypecheckResult> {
    if let HIRExprKind::Identifier {
        name,
        template_input: None,
    } = &lhs.kind
        && let Some(name) = name.root_name_ref().map(|name| name.as_str())
        && BUILTIN_FNS.contains(&name)
    {
        // TODO: This should be handled via real functions implemented with @intrinsics, but for now we just special-case them here.
        return typecheck_internal_method_call(env, namespace, name, rhs, expr);
    }

    let function = typecheck_expr(env, namespace, lhs, None)?;
    typecheck_callee_call(env, namespace, function, &vec![], rhs, expr, expected_type)
}

fn typecheck_internal_method_call(
    env: &mut TypeEnvironment,
    namespace: &EnvironmentNamespace,
    name: &str,
    rhs: &HIRExpression,
    expr: &HIRExpression,
) -> CXResult<TypecheckResult> {
    let args = comma_separated_exprs(rhs);
    let is_start = matches!(name, "va_start" | "__builtin_va_start");
    let expected = if is_start { 2 } else { 1 };
    if args.len() != expected {
        return env.log_error(
            expr.token_range(),
            format!("{name} expects {expected} arguments, found {}", args.len()),
        );
    }

    let list = typecheck_va_list(env, namespace, args[0])?;
    if is_start {
        let last = typecheck_expr(env, namespace, args[1], None)?
            .standard_ready_assure(env, args[1].token_range())?
            .internal_ready_assertion();
        Ok(TypecheckResult::new(
            THIRType::unit(),
            THIRExpressionKind::VaStart {
                list: Box::new(list),
                last: Box::new(last),
            },
        ))
    } else {
        Ok(TypecheckResult::new(
            THIRType::unit(),
            THIRExpressionKind::VaEnd {
                list: Box::new(list),
            },
        ))
    }
}

pub(crate) fn typecheck_va_list(
    env: &mut TypeEnvironment,
    namespace: &EnvironmentNamespace,
    expr: &HIRExpression,
) -> CXResult<THIRExpression> {
    let list = typecheck_expr(env, namespace, expr, None)?
        .standard_ready_assure(env, expr.token_range())?
        .internal_ready_assertion();
    let actual = env
        .symbols
        .mem_ref_inner(&list._type)
        .unwrap_or(&list._type);
    let expected = env.get_intrinsic_type("__builtin_va_list");
    if !compatible::compatible_types(env, actual, &expected)? {
        return env.log_error(
            expr.token_range(),
            format!(
                "expected va_list, found {}",
                list._type.display_with(&env.symbols)
            ),
        );
    }
    Ok(list)
}

pub(crate) fn typecheck_callee_call(
    env: &mut TypeEnvironment,
    namespace: &EnvironmentNamespace,
    callee: TypecheckResult,
    implicit_args: &[HIRExpression],
    rhs: &HIRExpression,
    expr: &HIRExpression,
    expected_type: Option<&THIRType>,
) -> CXResult<TypecheckResult> {
    let args = comma_separated_exprs(rhs);

    let scope = env.function.current_scope_index();
    let reachable = env.function.is_current_scope_reachable();
    let snapshot = env.function.current_snapshot();

    let tc_args = typecheck_args(env, namespace, args.as_slice())?;

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
        CompletedCallee::Staged(staged_expr) => {
            return complete_staged_call(env, namespace, staged_expr, args.iter());
        }

        CompletedCallee::Comptime {
            prototype,
            symbol_name,
        } => {
            return complete_comptime_call(
                env,
                namespace,
                expr,
                prototype,
                symbol_name,
                implicit_args.iter().chain(args.iter()),
            );
        }

        CompletedCallee::Runtime(callee) => callee,
    };

    let (loaded_function, signature) = load_callable(env, expr, callee)?;

    check_argument_count(env, expr, &signature, implicit_args.len() + args.len())?;

    let explicit_args = typecheck_args(env, namespace, &args)?;
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
        lvalue::try_conversion(env, function, false)?.catch_unapplied(|expr, _| Ok(expr))?;
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
    implicit_args: &[HIRExpression],
    args: &[(&HIRExpression, TypecheckResult)],
    expected_type: Option<&THIRType>,
) -> CXResult<CompletedCallee> {
    match function.try_into_expression() {
        TypecheckExtract::Succ(callee) => Ok(CompletedCallee::Runtime(callee)),
        TypecheckExtract::Fail(function) => {
            let function = match function.try_into_staged() {
                TypecheckExtract::Succ(staged) => return Ok(CompletedCallee::Staged(staged)),
                TypecheckExtract::Fail(function) => function,
            };

            match function.expression_state() {
                TypecheckState::ComptimeFunction {
                    prototype,
                    template_bindings,
                    ..
                } => {
                    return Ok(prepare_comptime_callee(env, prototype, template_bindings));
                }

                _ => (),
            }

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
                Ok(result) => {
                    if let TypecheckState::ComptimeFunction {
                        prototype,
                        template_bindings,
                        ..
                    } = &result.expression_state()
                    {
                        return Ok(prepare_comptime_callee(env, prototype, template_bindings));
                    }

                    match result.try_into_expression() {
                        TypecheckExtract::Succ(function) => Ok(CompletedCallee::Runtime(function)),
                        TypecheckExtract::Fail(_) => {
                            env.log_error(expr.token_range(), "Could not deduce callee".to_string())
                        }
                    }
                }
                Err(err) => env.log_error(expr.token_range(), err.message().to_string()),
            }
        }
    }
}

fn prepare_comptime_callee(
    env: &mut TypeEnvironment,
    prototype: &THIRComptimeFnPrototype,
    template_bindings: &[(CXIdent, THIRTypeID)],
) -> CompletedCallee {
    let symbol_name = comptime_instance_name(env, prototype, template_bindings);
    let runtime_return_type = env.materialization_return_type();

    let prototype = prototype
        .clone()
        .with_runtime_return_type(runtime_return_type);

    if !template_bindings.is_empty() {
        let input = THIRTemplateInput {
            args: template_bindings.iter().map(|(_, ty)| *ty).collect(),
        };

        env.items.push_request(THIRFunctionGenRequest::Comptime {
            name: prototype.lookup_identifier().clone(),
            prototype: prototype.clone(),
            input,
        });
    }

    CompletedCallee::Comptime {
        prototype,
        symbol_name,
    }
}

fn comptime_instance_name(
    env: &TypeEnvironment,
    prototype: &THIRComptimeFnPrototype,
    template_bindings: &[(CXIdent, THIRTypeID)],
) -> CXIdent {
    if template_bindings.is_empty() {
        return CXIdent::new(prototype.symbol_name());
    }
    CXIdent::new(base_mangle_templated_name(
        &env.symbols,
        prototype.symbol_name(),
        template_bindings
            .iter()
            .map(|(_, ty)| env.symbols.resolve_type_id(*ty)),
    ))
}

fn complete_comptime_call<'a>(
    env: &mut TypeEnvironment,
    namespace: &EnvironmentNamespace,
    expr: &HIRExpression,
    prototype: THIRComptimeFnPrototype,
    symbol_name: CXIdent,
    args: impl ExactSizeIterator<Item = &'a HIRExpression>,
) -> CXResult<TypecheckResult> {
    if args.len() != prototype.params().len() {
        return env.log_error(
            expr.token_range(),
            format!(
                "Call to comptime function {} expects {} arguments, found {}",
                prototype.pretty_name(),
                prototype.params().len(),
                args.len()
            ),
        );
    }

    let args = args
        .zip(prototype.params())
        .map(|(arg, param)| {
            if param.value_type.expr {
                Ok(typecheck_staged_expr(env, namespace, arg)?
                    .map_expr(|expr| implicit_cast(env, expr, &param.value_type._type))?
                    .into_expr())
            } else {
                typecheck_expr(env, namespace, arg, Some(&param.value_type._type))
                    .and_then(|v| v.apply_expected_type(env, namespace, &param.value_type._type))
                    .and_then(|v| v.standard_ready_coerce(env, arg.token_range()))
                    .and_then(|v| implicit_cast(env, v, &param.value_type._type))
            }
        })
        .collect::<CXResult<Vec<_>>>()?;

    Ok(TypecheckResult::from(THIRExpression {
        token_range: expr.token_range().clone(),
        _type: prototype.return_type()._type.clone(),
        kind: THIRExpressionKind::CallFunction {
            function: Box::new(THIRExpression {
                token_range: TokenRange::internal(),
                // The callee's type is undefined; lowering dispatches on the
                // symbol name against the unit's comptime function list.
                _type: THIRTypeKind::Undefined.into(),
                kind: THIRExpressionKind::FunctionReference {
                    name: symbol_name,
                    debug_name: prototype.debug_name().cloned(),
                },
            }),
            arguments: args,
            contract: THIRFnContract::default(),
        },
    }))
}

fn complete_staged_call(
    env: &mut TypeEnvironment,
    namespace: &EnvironmentNamespace,
    staged_expr: THIRStagedExpr,
    args: impl ExactSizeIterator<Item = &'_ HIRExpression>,
) -> CXResult<TypecheckResult> {
    if args.len() != params.len() {
        return env.log_error(
            expr.token_range(),
            format!(
                "Staged expression expects {} arguments, found {}",
                params.len(),
                args.len()
            ),
        );
    }

    let args = args
        .zip(params.iter())
        .map(|(arg, param)| {
            typecheck_staged_expr(env, namespace, arg)
                .and_then(|v| v.apply_expected_type(env, namespace, &param._type))
                .and_then(|v| v.standard_ready_assert(env, token_range))
                .and_then(|v| implicit_cast(env, v, &param._type))
        })
        .collect::<CXResult<Vec<_>>>()?;

    Ok(
        TypecheckResult::new(
            staged_expr.expr()._type.clone(),
            THIRExpressionKind::MaterializeStagedExpression { 
                expr: Box::new(staged_expr.into_expr()),
                with_params: args,
            }
        )
    )
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
