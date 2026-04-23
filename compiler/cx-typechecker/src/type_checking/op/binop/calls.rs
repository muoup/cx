use crate::environment::TypeEnvironment;
use crate::environment::functions::query::{
    query_deduced_member_function, query_deduced_standard_function,
};
use crate::log_typecheck_error;
use crate::type_checking::aggregate::fields::struct_field;
use crate::type_checking::coercion::implicit::implicit_cast;
use crate::type_checking::coercion::implicit::promotion::lvalue;
use crate::type_checking::coercion::implicit::promotion::std_rval_promotion;
use crate::type_checking::op::binop::access::{
    build_member_receiver_argument, resolve_access_base,
};
use crate::type_checking::op::binop::scoped_calls::typecheck_scoped_call;
use crate::type_checking::result::TypecheckResult;
use crate::type_checking::typechecker::typecheck_expr;
use cx_ast::ast::{CXBinOp, CXExpr, CXExprKind};
use cx_mir::mir::data::{MIRFloatType, MIRFunctionPrototype, MIRType, MIRTypeKind};
use cx_mir::mir::expression::{MIRExpression, MIRExpressionKind};
use cx_mir::mir::program::MIRBaseMappings;
use cx_util::CXResult;

pub(crate) fn build_function_reference(prototype: &MIRFunctionPrototype) -> MIRExpression {
    MIRExpression {
        token_range: None,
        kind: MIRExpressionKind::FunctionReference {
            name: prototype.name.clone(),
        },
        _type: MIRTypeKind::Function {
            signature: Box::new(prototype.signature()),
        }
        .into(),
    }
}

pub(crate) fn finish_function_call<'a>(
    env: &mut TypeEnvironment,
    _callee_expr: &'a CXExpr,
    expr: &'a CXExpr,
    function: TypecheckResult,
    mut tc_args: Vec<(&'a CXExpr, MIRExpression)>,
) -> CXResult<TypecheckResult> {
    let (function, implicit_parameters) = function.into_parts();
    tc_args = implicit_parameters
        .iter()
        .map(|val| (expr, val.clone()))
        .chain(tc_args)
        .collect();

    let loaded_function =
        lvalue::try_conversion(env, function)?.catch_unapplied(|expr, _cause| {
            log_typecheck_error!(
                env,
                expr.token_range.as_ref(),
                "Attempted to call value that cannot be loaded as a function"
            )
        })?;
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
            loaded_function_type
        );
    };

    if tc_args.len() != signature.params.len() && !signature.var_args {
        return log_typecheck_error!(
            env,
            expr.token_range(),
            "Call to {} expects {} arguments, found {}",
            signature,
            signature.params.len(),
            tc_args.len()
        );
    }

    if tc_args.len() < signature.params.len() {
        return log_typecheck_error!(
            env,
            expr.token_range(),
            "Call to {} expects at least {} arguments, found {}",
            signature,
            signature.params.len(),
            tc_args.len()
        );
    }

    let canon_params = signature.params.len();

    for ((_arg_expr, val), param) in tc_args.iter_mut().zip(signature.params.iter()) {
        *val = implicit_cast(env, std::mem::take(val), &param._type)?;
    }

    for (_arg_expr, val) in tc_args.iter_mut().skip(canon_params) {
        *val = std_rval_promotion(env, std::mem::take(val))?;

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
                    arg_type
                );
            }
        }
    }

    let args = tc_args.into_iter().map(|(_, val)| val).collect::<Vec<_>>();

    Ok(TypecheckResult::new_base(
        signature.return_type.clone(),
        MIRExpressionKind::CallFunction {
            function: Box::new(loaded_function),
            arguments: args,
        },
    ))
}

pub(crate) fn comma_separated<'a>(
    env: &mut TypeEnvironment,
    base_data: &MIRBaseMappings,
    expr: &'a CXExpr,
) -> CXResult<Vec<(&'a CXExpr, MIRExpression)>> {
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
    lhs: &CXExpr,
    expr: &CXExpr,
    arg_types: &[MIRType],
) -> CXResult<Option<TypecheckResult>> {
    match &lhs.kind {
        CXExprKind::Identifier(name) => {
            if env.function.symbol_value(name.as_str()).is_some() {
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

            let receiver_source =
                typecheck_expr(env, base_data, receiver_expr, None)?.into_expression();
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
    lhs: &CXExpr,
    rhs: &CXExpr,
    expr: &CXExpr,
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

    let function = match typecheck_expr(env, base_data, lhs, None) {
        Ok(function) => function,
        Err(err) => match deduced_callee(env, base_data, lhs, expr, &arg_types)? {
            Some(function) => function,
            None => return Err(err),
        },
    };

    finish_function_call(env, lhs, expr, function, tc_args)
}
