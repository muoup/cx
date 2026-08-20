use crate::environment::TypeEnvironment;
use crate::type_checking::coercion::implicit::implicit_cast;
use crate::type_checking::coercion::implicit::promotion::std_rval_promotion;
use crate::type_checking::typechecker::typecheck_expr;
use cx_log::CXResult;
use cx_thir::EnvironmentNamespace;
use cx_thir::thir::data::{THIRFnSignature, THIRType};
use cx_thir::thir::expression::{
    THIRExpression, THIRExpressionKind, THIRFnContract, THIRPostcondition,
};
use cx_tokens::TokenRange;
use cx_util::namespace::QualifiedName;

pub(crate) fn typecheck_contract(
    env: &mut TypeEnvironment,
    namespace: &EnvironmentNamespace,
    prototype: &THIRFnSignature,
) -> CXResult<THIRFnContract> {
    let naive_contract = &prototype.contract;
    let previous_mode = env.push_contract_mode(naive_contract.safe);

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
            let condition = typecheck_expr(env, namespace, pre_expr, Some(&THIRType::bool()))
                .and_then(|value| value.standard_ready_coerce(env, pre_expr.token_range()))
                .and_then(|value| std_rval_promotion(env, value))
                .and_then(|value| implicit_cast(env, value, &THIRType::bool()))?;
            Ok(Box::new(THIRExpression {
                token_range: condition.token_range.clone(),
                kind: THIRExpressionKind::Assert {
                    condition: Box::new(condition),
                    message: "Precondition failed".to_string(),
                },
                _type: THIRType::unit(),
            }))
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
            .and_then(|value| value.standard_ready_coerce(env, post_expr.token_range()))
            .and_then(|value| std_rval_promotion(env, value))
            .and_then(|value| implicit_cast(env, value, &THIRType::bool()))?;
        Some(THIRPostcondition {
            binding: ret_name.clone(),
            condition: Box::new(tc_post),
        })
    } else {
        None
    };

    env.pop_scope()
        .map_err(|err| env.complete_err(err, &TokenRange::internal()))?;
    env.restore_function_mode(previous_mode);

    Ok(THIRFnContract {
        safe: naive_contract.safe,
        noreturn: naive_contract.noreturn,
        precondition,
        postcondition,
    })
}
