use crate::environment::TypeEnvironment;
use crate::log_typecheck_error;
use crate::symbol::deduction::complete_templated_callee;
use crate::type_checking::coercion::implicit::implicit_cast;
use crate::type_checking::coercion::implicit::promotion::lvalue;
use crate::type_checking::coercion::implicit::promotion::std_rval_promotion;
use crate::type_checking::contracts::typecheck_contract;
use crate::type_checking::result::{
    CalleeExtraction, PendingReceiver, TypecheckExtract, TypecheckResult,
};
use crate::type_checking::typechecker::typecheck_expr;
use crate::type_checking::value::moves::typecheck_move;
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
) -> CXResult<TypecheckResult> {
    let function = typecheck_expr(env, namespace, lhs, None)?;

    typecheck_callee_method_call(env, namespace, function, rhs, expr)
}

pub(crate) fn typecheck_callee_method_call(
    env: &mut TypeEnvironment,
    namespace: &EnvironmentNamespace,
    callee: TypecheckResult,
    rhs: &CXExpression,
    expr: &CXExpression,
) -> CXResult<TypecheckResult> {
    let tc_args = comma_separated(env, namespace, rhs)?;
    let callee = complete_callee(env, namespace, expr, callee, &tc_args)?;
    let arg_count = call_arg_count(&callee, &tc_args);
    let CalleeExtraction {
        function,
        implicit_args,
    } = callee;
    let (loaded_function, signature) = load_callable(env, expr, function)?;

    check_argument_count(env, expr, &signature, arg_count)?;

    let argument_results =
        complete_call_arguments(env, namespace, &signature, implicit_args, tc_args)?;
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
    let loaded_function = lvalue::try_conversion(env, function)?.expr();
    let function_type = loaded_function.get_type();
    let callable_type = env
        .symbols
        .ptr_inner(&function_type)
        .cloned()
        .unwrap_or(function_type);

    let MIRTypeKind::Function { signature } = &callable_type.kind else {
        return log_typecheck_error!(
            env,
            expr.token_range(),
            "Attempted to call value of non-function type {}",
            callable_type.display_with(&env.symbols)
        );
    };

    Ok((loaded_function, signature.as_ref().clone()))
}

fn check_argument_count(
    env: &TypeEnvironment,
    expr: &CXExpression,
    signature: &MIRFunctionSignature,
    arg_count: usize,
) -> CXResult<()> {
    if arg_count != signature.params.len() && !signature.var_args {
        return log_typecheck_error!(
            env,
            expr.token_range(),
            "Call to {} expects {} arguments, found {}",
            signature.display_with(&env.symbols),
            signature.params.len(),
            arg_count
        );
    }

    if arg_count < signature.params.len() {
        return log_typecheck_error!(
            env,
            expr.token_range(),
            "Call to {} expects at least {} arguments, found {}",
            signature.display_with(&env.symbols),
            signature.params.len(),
            arg_count
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
            return log_typecheck_error!(
                env,
                Some(expr.token_range()),
                "Cannot pass {} to varargs: expected an intrinsic type or pointer",
                arg_type.display_with(&env.symbols)
            );
        }
    }

    Ok(val)
}

fn complete_call_arguments(
    env: &mut TypeEnvironment,
    namespace: &EnvironmentNamespace,
    signature: &MIRFunctionSignature,
    implicit_args: Vec<MIRExpression>,
    explicit_args: Vec<(&CXExpression, TypecheckResult)>,
) -> CXResult<Vec<TypecheckResult>> {
    let tc_args = implicit_args
        .into_iter()
        .map(TypecheckResult::from)
        .chain(explicit_args.into_iter().map(|(_, val)| val));

    tc_args
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
    source_base_type: Option<MIRType>,
    args: &[(&CXExpression, TypecheckResult)],
) -> Vec<MIRType> {
    source_base_type
        .into_iter()
        .chain(args.iter().filter_map(|(_, arg)| arg.ready_type().cloned()))
        .collect()
}

fn call_arg_count(
    callee: &CalleeExtraction,
    explicit_args: &[(&CXExpression, TypecheckResult)],
) -> usize {
    callee.implicit_args.len() + explicit_args.len()
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
    args: &[(&CXExpression, TypecheckResult)],
) -> CXResult<CalleeExtraction> {
    match function.try_into_callee() {
        TypecheckExtract::Succ(callee) => {
            complete_pending_receiver(env, namespace, expr, callee, None)
        }
        TypecheckExtract::Fail(function) => {
            let Some(parts) = function.into_incomplete_callee_parts() else {
                return log_typecheck_error!(env, expr.token_range(), "Could not deduce callee");
            };

            let deduction_arg_types = deduction_arg_types(parts.source_base_type, args);

            let symbol = match complete_templated_callee(
                env,
                namespace,
                &parts.name,
                parts.template_input.as_ref(),
                &deduction_arg_types,
            ) {
                Ok(symbol) => symbol,
                Err(err) => {
                    return log_typecheck_error!(
                        env,
                        expr.token_range(),
                        "{}",
                        err.error_content()
                    );
                }
            };

            let function = match symbol.as_expression() {
                Ok(function) => function,
                Err(err) => {
                    return log_typecheck_error!(
                        env,
                        expr.token_range(),
                        "{}",
                        err.error_content()
                    );
                }
            };

            complete_pending_receiver(
                env,
                namespace,
                expr,
                CalleeExtraction {
                    function,
                    implicit_args: parts.implicit_args,
                },
                parts.pending_receiver,
            )
        }
    }
}

fn complete_pending_receiver(
    env: &mut TypeEnvironment,
    namespace: &EnvironmentNamespace,
    expr: &CXExpression,
    mut callee: CalleeExtraction,
    pending_receiver: Option<PendingReceiver>,
) -> CXResult<CalleeExtraction> {
    let Some(PendingReceiver { source, binding }) = pending_receiver else {
        return Ok(callee);
    };

    let MIRTypeKind::Function { signature } = &callee.function._type.kind else {
        unreachable!("function references must have function type")
    };

    let needs_move = signature
        .params
        .first()
        .map(|param| !param._type.is_memory_reference())
        .unwrap_or(false);

    let receiver = if needs_move {
        let mut receiver = TypecheckResult::from(source);
        if let Some(binding) = binding {
            receiver = receiver.with_binding(binding);
        }

        typecheck_move(env, namespace, receiver, expr)
            .and_then(|v| v.standard_ready_coerce(env, expr.token_range()))?
    } else {
        source
    };

    callee.implicit_args.insert(0, receiver);
    Ok(callee)
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
