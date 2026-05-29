use crate::environment::TypeEnvironment;
use crate::environment::functions::query::query_function;
use crate::log_typecheck_error;
use crate::type_checking::coercion::implicit::conversion::try_argument_conversion;
use crate::type_checking::coercion::implicit::implicit_cast;
use crate::type_checking::coercion::implicit::promotion::lvalue;
use crate::type_checking::coercion::implicit::promotion::std_rval_promotion;
use crate::type_checking::contracts::typecheck_contract;
use crate::type_checking::result::{CalleeExtraction, TypecheckExtract, TypecheckResult};
use crate::type_checking::typechecker::typecheck_expr;
use cx_ast::ast::{CXBinOp, CXExprKind, CXExpression};
use cx_mir::mir::data::{MIRFloatType, MIRFunctionPrototype, MIRType, MIRTypeKind};
use cx_mir::mir::expression::{MIRExpression, MIRExpressionKind};
use cx_mir::mir::program::EnvironmentNamespace;
use cx_util::CXResult;

pub(crate) fn build_function_reference(prototype: &MIRFunctionPrototype) -> MIRExpression {
    MIRExpression {
        token_range: None,
        _type: MIRTypeKind::Function {
            signature: Box::new(prototype.signature()),
        }
        .into(),
        kind: MIRExpressionKind::FunctionReference {
            name: prototype.name.clone(),
        },
    }
}

pub(crate) fn finish_function_call<'a>(
    env: &mut TypeEnvironment,
    namespace: &EnvironmentNamespace,
    expr: &'a CXExpression,
    callee: CalleeExtraction,
    mut tc_args: Vec<(&'a CXExpression, TypecheckResult)>,
) -> CXResult<TypecheckResult> {
    tc_args = callee
        .implicit_args
        .iter()
        .map(|val| (expr, TypecheckResult::from(val.clone())))
        .chain(tc_args)
        .collect();

    let loaded_function = lvalue::try_conversion(env, callee.function)?.expr();
    let loaded_function_type = loaded_function.get_type();
    let loaded_function_type = env
        .symbols
        .context
        .ptr_inner(&loaded_function_type)
        .cloned()
        .unwrap_or(loaded_function_type);

    let MIRTypeKind::Function { signature } = &loaded_function_type.kind else {
        return log_typecheck_error!(
            env,
            expr.token_range(),
            "Attempted to call value of non-function type {}",
            loaded_function_type.display_with(&env.symbols.context)
        );
    };

    if tc_args.len() != signature.params.len() && !signature.var_args {
        return log_typecheck_error!(
            env,
            expr.token_range(),
            "Call to {} expects {} arguments, found {}",
            signature.display_with(&env.symbols.context),
            signature.params.len(),
            tc_args.len()
        );
    }

    if tc_args.len() < signature.params.len() {
        return log_typecheck_error!(
            env,
            expr.token_range(),
            "Call to {} expects at least {} arguments, found {}",
            signature.display_with(&env.symbols.context),
            signature.params.len(),
            tc_args.len()
        );
    }

    let mut args = Vec::with_capacity(tc_args.len());

    for (i, (_arg_expr, val)) in tc_args.into_iter().enumerate() {
        let mut val = if let Some(param) = signature.params.get(i) {
            let val = val.into_expression_with_expected(env, namespace, &param._type)?;
            try_argument_conversion(env, val, &param._type)?
        } else {
            val.into_expression()?
        };

        if i < signature.params.len() {
            args.push(val);
            continue;
        }

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
                    arg_type.display_with(&env.symbols.context)
                );
            }
        }

        args.push(val);
    }

    let contract = typecheck_contract(env, namespace, signature)?;

    Ok(TypecheckResult::new_base(
        signature.return_type.clone(),
        MIRExpressionKind::CallFunction {
            function: Box::new(loaded_function),
            arguments: args,
            contract,
        },
    ))
}

fn complete_callee(
    env: &mut TypeEnvironment,
    namespace: &EnvironmentNamespace,
    expr: &CXExpression,
    function: TypecheckResult,
    arg_types: &[MIRType],
) -> CXResult<CalleeExtraction> {
    match function.try_into_callee() {
        TypecheckExtract::Succ(callee) => Ok(callee),
        TypecheckExtract::Fail(function) => {
            let Some((name, template_input, deduction_arg_prefix, implicit_parameters)) =
                function.into_incomplete_callee_parts()
            else {
                return log_typecheck_error!(
                    env,
                    Some(expr.token_range()),
                    "Expected a callable expression"
                );
            };

            let all_arg_types = deduction_arg_prefix
                .iter()
                .cloned()
                .chain(arg_types.iter().cloned())
                .collect::<Vec<_>>();

            let Some(prototype) = query_function(
                env,
                namespace,
                expr,
                &name,
                template_input.as_ref(),
                &all_arg_types,
            )?
            else {
                return log_typecheck_error!(
                    env,
                    Some(expr.token_range()),
                    "Function '{}' not found",
                    name
                );
            };

            Ok(CalleeExtraction::new(
                build_function_reference(&prototype),
                implicit_parameters,
                Vec::new(),
            ))
        }
    }
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

pub(crate) fn ready_arg_types(
    args: &[(&CXExpression, TypecheckResult)],
) -> CXResult<Option<Vec<MIRType>>> {
    let mut arg_types = Vec::with_capacity(args.len());

    for (_, arg) in args {
        let Some(arg_type) = arg.get_type_if_ready()? else {
            return Ok(None);
        };
        arg_types.push(arg_type);
    }

    Ok(Some(arg_types))
}

pub(crate) fn typecheck_method_call(
    env: &mut TypeEnvironment,
    namespace: &EnvironmentNamespace,
    lhs: &CXExpression,
    rhs: &CXExpression,
    expr: &CXExpression,
) -> CXResult<TypecheckResult> {
    let tc_args = comma_separated(env, namespace, rhs)?;
    let arg_types = ready_arg_types(&tc_args)?.unwrap_or_default();

    let function = typecheck_expr(env, namespace, lhs, None)?;
    let function = complete_callee(env, namespace, expr, function, &arg_types)?;

    finish_function_call(env, namespace, expr, function, tc_args)
}
