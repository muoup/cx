use crate::environment::TypeEnvironment;
use crate::type_checking::coercion::implicit::implicit_cast;
use crate::type_checking::coercion::implicit::promotion::std_rval_promotion;
use crate::type_checking::typechecker::typecheck_expr;
use cx_log::CXResult;
use cx_thir::EnvironmentNamespace;
use cx_thir::thir::data::{THIRFnPrototype, THIRFnSignature, THIRType};
use cx_thir::thir::expression::{
    THIRExpression, THIRExpressionKind, THIRFnContract, THIRPostcondition,
};
use cx_thir::symbol::MIRSymbol;
use cx_tokens::TokenRange;
use cx_util::identifier::CXIdent;
use cx_util::namespace::{NamespacePath, QualifiedName};

pub(crate) fn typecheck_contract(
    env: &mut TypeEnvironment,
    namespace: &EnvironmentNamespace,
    prototype: &THIRFnSignature,
) -> CXResult<THIRFnContract> {
    let naive_contract = &prototype.contract;
    let previous_mode = env.push_contract_mode(naive_contract.safe);
    let assertion_prototype =
        if naive_contract.precondition.is_some() || naive_contract.postcondition.is_some() {
            Some(Box::new(resolve_assertion_prototype(env, namespace)?))
        } else {
            None
        };

    env.push_scope(false, false);

    for param in prototype.params.iter() {
        if let Some(name) = &param.name {
            let _ty = env.symbols.mem_ref_to(param._type.clone());

            env.symbols.insert_local_value(
                QualifiedName::new_raw(name.clone()),
                THIRExpression {
                    token_range: TokenRange::internal(),
                    kind: THIRExpressionKind::ContractVariable {
                        name: name.clone(),
                        force_param: false,
                    },
                    _type: param._type.clone(),
                },
            );
        }
    }

    let precondition = naive_contract
        .precondition
        .as_ref()
        .map(|pre_expr| {
            let tc_pre = typecheck_expr(env, namespace, pre_expr, Some(&THIRType::bool()))
                .and_then(|v| v.standard_ready_coerce(env, pre_expr.token_range()))
                .and_then(|v| std_rval_promotion(env, v))
                .and_then(|v| implicit_cast(env, v, &THIRType::bool()))?;
            Ok(Box::new(tc_pre))
        })
        .transpose()?;

    let postcondition = if let Some((ret_name, post_expr)) = &naive_contract.postcondition {
        if let Some(ret_name) = ret_name {
            env.symbols.insert_local_value(
                QualifiedName::new_raw(ret_name.clone()),
                THIRExpression {
                    token_range: TokenRange::internal(),
                    kind: THIRExpressionKind::ContractVariable {
                        name: ret_name.clone(),
                        force_param: false,
                    },
                    _type: prototype.return_type.clone(),
                },
            );
        }

        let tc_post = typecheck_expr(env, namespace, post_expr, Some(&THIRType::bool()))
            .and_then(|v| v.standard_ready_coerce(env, post_expr.token_range()))
            .and_then(|v| std_rval_promotion(env, v))
            .and_then(|v| implicit_cast(env, v, &THIRType::bool()))?;
        Some(THIRPostcondition {
            binding: ret_name.clone(),
            condition: Box::new(tc_post),
            assertion_prototype: assertion_prototype
                .clone()
                .expect("postcondition requires assertion prototype"),
        })
    } else {
        None
    };

    env.pop_scope()
        .map_err(|err| env.complete_err(err, &TokenRange::internal()))?;
    env.restore_function_mode(previous_mode);

    Ok(THIRFnContract {
        safe: naive_contract.safe,
        assertion_prototype,
        precondition,
        postcondition,
    })
}

pub(crate) fn resolve_assertion_prototype(
    env: &mut TypeEnvironment,
    namespace: &EnvironmentNamespace,
) -> CXResult<THIRFnPrototype> {
    let name = QualifiedName::new(
        NamespacePath::from_scoped_path("std::intrinsic::assertion"),
        CXIdent::new("__compiler_assert"),
    );

    let Some(symbol) = env.get_symbol(namespace, &name)? else {
        return crate::log::internal_type_error(
            "Function contract used but std::intrinsic::assertion::__compiler_assert was not found",
        );
    };

    let MIRSymbol::FunctionReference(prototype) = symbol else {
        return crate::log::internal_type_error(
            "std::intrinsic::assertion::__compiler_assert is not a function",
        );
    };

    Ok(prototype)
}
