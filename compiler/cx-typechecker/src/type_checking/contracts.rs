use crate::environment::TypeEnvironment;
use crate::type_checking::coercion::implicit::implicit_cast;
use crate::type_checking::coercion::implicit::promotion::std_rval_promotion;
use crate::type_checking::typechecker::typecheck_expr;
use cx_mir::mir::data::{MIRFunctionSignature, MIRType};
use cx_mir::mir::expression::{MIRExpression, MIRExpressionKind, MIRFunctionContract};
use cx_mir::mir::program::MIRBaseMappings;
use cx_util::CXResult;

pub(crate) fn typecheck_contract(
    env: &mut TypeEnvironment,
    base_data: &MIRBaseMappings,
    prototype: &MIRFunctionSignature,
) -> CXResult<MIRFunctionContract> {
    let naive_contract = &prototype.contract;
    let previous_mode = env.push_contract_mode(naive_contract.safe);

    env.function.push_scope(false, false);

    for param in prototype.params.iter() {
        if let Some(name) = &param.name {
            let _ty = env.symbols.context.mem_ref_to(param._type.clone());

            env.function.insert_symbol(
                name.to_string(),
                MIRExpression {
                    token_range: None,
                    kind: MIRExpressionKind::ContractVariable { 
                        name: name.clone(),
                        force_param: false
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
            let tc_pre = typecheck_expr(env, base_data, pre_expr, Some(&MIRType::bool()))
                .and_then(|v| std_rval_promotion(env, v.into_expression()))
                .and_then(|v| implicit_cast(env, v, &MIRType::bool()))?;
            Ok(Box::new(tc_pre))
        })
        .transpose()?;

    let postcondition = if let Some((ret_name, post_expr)) = &naive_contract.postcondition {
        if let Some(ret_name) = ret_name {
            env.function.insert_symbol(
                ret_name.to_string(),
                MIRExpression {
                    token_range: None,
                    kind: MIRExpressionKind::ContractVariable { 
                        name: ret_name.clone(),
                        force_param: false 
                    },
                    _type: prototype.return_type.clone(),
                },
            );
        }

        let tc_post = typecheck_expr(env, base_data, post_expr, Some(&MIRType::bool()))
            .and_then(|v| std_rval_promotion(env, v.into_expression()))
            .and_then(|v| implicit_cast(env, v, &MIRType::bool()))?;
        Some((ret_name.clone(), Box::new(tc_post)))
    } else {
        None
    };

    env.function
        .pop_scope(env.source.compilation_unit.as_path())?;
    env.restore_function_mode(previous_mode);

    Ok(MIRFunctionContract {
        safe: naive_contract.safe,
        precondition,
        postcondition,
    })
}
