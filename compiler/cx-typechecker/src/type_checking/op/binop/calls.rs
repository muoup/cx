use crate::environment::TypeEnvironment;
use crate::environment::functions::query::{
    query_deduced_member_function, query_deduced_standard_function,
};
use crate::environment::symbols::{ResolvedValueSymbol, SymbolValueOrigin};
use crate::log_typecheck_error;
use crate::type_checking::aggregate::fields::struct_field;
use crate::type_checking::coercion::implicit::implicit_cast;
use crate::type_checking::coercion::implicit::promotion::lvalue;
use crate::type_checking::coercion::implicit::promotion::std_rval_promotion;
use crate::type_checking::contracts::typecheck_contract;
use crate::type_checking::op::binop::access::{
    build_member_receiver_argument, resolve_access_base,
};
use crate::type_checking::op::binop::scoped_calls::typecheck_scoped_call;
use crate::type_checking::result::TypecheckResult;
use crate::type_checking::typechecker::typecheck_expr;
use cx_ast::ast::{CXBinOp, CXExprKind, CXExpression};
use cx_mir::mir::data::{MIRFloatType, MIRFunctionPrototype, MIRType, MIRTypeKind};
use cx_mir::mir::expression::{MIRExpression, MIRExpressionKind};
use cx_mir::mir::program::MIRBaseMappings;
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
    base_data: &MIRBaseMappings,
    expr: &'a CXExpression,
    function: TypecheckResult,
    mut tc_args: Vec<(&'a CXExpression, MIRExpression)>,
) -> CXResult<TypecheckResult> {
    let (function, implicit_parameters) = function.decompose_function_expr();
    let implicit_arg_count = implicit_parameters.len();
    tc_args = implicit_parameters
        .iter()
        .map(|val| (expr, val.clone()))
        .chain(tc_args)
        .collect();

    let loaded_function = lvalue::try_conversion(env, function)?.expr();
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

    let canon_params = signature.params.len();

    for (_, val) in tc_args.iter_mut().skip(implicit_arg_count) {
        *val = std_rval_promotion(env, std::mem::take(val))?;
    }

    for ((_arg_expr, val), param) in tc_args.iter_mut().zip(signature.params.iter()) {
        *val = implicit_cast(env, std::mem::take(val), &param._type)?;
    }

    for (_arg_expr, val) in tc_args.iter_mut().skip(canon_params) {
        let arg_type = val._type.clone();

        match &arg_type.kind {
            MIRTypeKind::PointerTo { .. } => {}
            MIRTypeKind::Integer { .. } => {}
            MIRTypeKind::Float {
                _type: MIRFloatType::F32,
            } => {
                *val = implicit_cast(
                    env,
                    std::mem::take(val),
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
    }

    let args = tc_args.into_iter().map(|(_, val)| val).collect::<Vec<_>>();
    let contract = typecheck_contract(env, base_data, signature)?;

    Ok(TypecheckResult::new_base(
        signature.return_type.clone(),
        MIRExpressionKind::CallFunction {
            function: Box::new(loaded_function),
            arguments: args,
            contract,
        },
    ))
}

pub(crate) fn comma_separated<'a>(
    env: &mut TypeEnvironment,
    base_data: &MIRBaseMappings,
    expr: &'a CXExpression,
) -> CXResult<Vec<(&'a CXExpression, MIRExpression)>> {
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
        let tc_result = typecheck_expr(env, base_data, rhs, None)?;
        exprs.push((rhs, tc_result.into_expression()));
        expr_iter = lhs;
    }

    let tc_result = typecheck_expr(env, base_data, expr_iter, None)?;
    exprs.push((expr_iter, tc_result.into_expression()));
    exprs.reverse();

    Ok(exprs)
}

pub(crate) fn deduced_callee(
    env: &mut TypeEnvironment,
    base_data: &MIRBaseMappings,
    lhs: &CXExpression,
    expr: &CXExpression,
    arg_types: &[MIRType],
) -> CXResult<Option<TypecheckResult>> {
    match &lhs.kind {
        CXExprKind::Identifier(name) => {
            if matches!(
                env.symbols.resolve_value_symbol(name.as_str()),
                Some(ResolvedValueSymbol::Value {
                    origin: Some(SymbolValueOrigin::Local | SymbolValueOrigin::Contract),
                    ..
                })
            ) {
                return Ok(None);
            }

            let Some(prototype) =
                query_deduced_standard_function(env, base_data, expr, name, arg_types)?
            else {
                return Ok(None);
            };

            Ok(Some(TypecheckResult::from(build_function_reference(
                &prototype,
            ))))
        }

        CXExprKind::BinOp {
            op: CXBinOp::Access,
            lhs: receiver_expr,
            rhs: member_expr,
        } => {
            let CXExprKind::Identifier(name) = &member_expr.kind else {
                return Ok(None);
            };

            let receiver_result = typecheck_expr(env, base_data, receiver_expr, None)?;
            let receiver_binding = receiver_result.binding.clone();
            let receiver_source = receiver_result.into_expression();
            let (receiver_root, receiver_value, receiver_type, _) =
                resolve_access_base(env, expr, receiver_source)?;

            if struct_field(&receiver_type, &env.symbols.context, name.as_str()).is_some() {
                return Ok(None);
            }

            if matches!(receiver_type.kind, MIRTypeKind::Union { .. })
                && receiver_type
                    .aggregate_fields(&env.symbols.context)
                    .map(|fields| {
                        fields
                            .iter()
                            .any(|(field_name, _)| field_name == name.as_str())
                    })
                    .unwrap_or(false)
            {
                return Ok(None);
            }

            let Some(prototype) = query_deduced_member_function(
                env,
                base_data,
                expr,
                &receiver_type,
                name,
                arg_types,
            )?
            else {
                return Ok(None);
            };

            let receiver = build_member_receiver_argument(
                env,
                expr,
                &receiver_root,
                receiver_binding.as_ref(),
                receiver_value,
                &receiver_type,
                &prototype,
            )?;

            Ok(Some(
                TypecheckResult::from(build_function_reference(&prototype))
                    .with_implicit_parameters(vec![receiver]),
            ))
        }

        _ => Ok(None),
    }
}

pub(crate) fn typecheck_method_call(
    env: &mut TypeEnvironment,
    base_data: &MIRBaseMappings,
    lhs: &CXExpression,
    rhs: &CXExpression,
    expr: &CXExpression,
) -> CXResult<TypecheckResult> {
    if let CXExprKind::BinOp {
        op: CXBinOp::ScopeRes,
        lhs: type_expr,
        rhs: method_expr,
    } = &lhs.kind
    {
        return typecheck_scoped_call(env, base_data, type_expr, method_expr, rhs, expr);
    }

    let tc_args = comma_separated(env, base_data, rhs)?;
    let arg_types = &tc_args
        .iter()
        .map(|(_, val)| val.get_type())
        .collect::<Vec<_>>();

    let function = match deduced_callee(env, base_data, lhs, expr, arg_types)? {
        Some(function) => function,
        None => typecheck_expr(env, base_data, lhs, None)?,
    };

    finish_function_call(env, base_data, expr, function, tc_args)
}
