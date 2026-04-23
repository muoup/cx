use crate::environment::TypeEnvironment;
use crate::type_checking::coercion::implicit::implicit_cast;
use crate::type_checking::coercion::implicit::promotion::std_rval_promotion;
use crate::type_checking::typechecker::typecheck_expr;
use cx_ast::ast::CXExpression;
use cx_ast::data::CXFunctionPrototype;
use cx_mir::mir::data::MIRType;
use cx_mir::mir::expression::{MIRExpression, MIRExpressionKind, MIRFunctionContract};
use cx_mir::mir::program::MIRBaseMappings;
use cx_util::{CXResult, identifier::CXIdent};

pub(crate) fn typecheck_contract(
    env: &mut TypeEnvironment,
    base_data: &MIRBaseMappings,
    function_name: &CXIdent,
    prototype: &CXFunctionPrototype,
) -> CXResult<MIRFunctionContract> {
    let naive_contract = &prototype.contract;
    let previous_mode = env.push_contract_mode(naive_contract.safe);

    env.function.push_scope(false, false);

    for param in prototype.params.iter() {
        if let Some(name) = &param.name {
            let mir_type = env.complete_type(base_data, &CXExpression::default(), &param._type)?;
            env.function.insert_symbol(
                name.to_string(),
                MIRExpression {
                    token_range: None,
                    kind: MIRExpressionKind::ContractVariable {
                        name: name.clone(),
                        parent_function: function_name.clone(),
                    },
                    _type: mir_type,
                },
            );
        }
    }

    let precondition = if let Some(pre_expr) = &naive_contract.precondition {
        let tc_pre = typecheck_expr(env, base_data, pre_expr, Some(&MIRType::bool()))
            .and_then(|v| std_rval_promotion(env, v.into_expression()))
            .and_then(|v| implicit_cast(env, v, &MIRType::bool()))?;
        Some(Box::new(tc_pre))
    } else {
        None
    };

    let postcondition = if let Some((ret_name, post_expr)) = &naive_contract.postcondition {
        if let Some(ret_name) = ret_name {
            let mir_type =
                env.complete_type(base_data, &CXExpression::default(), &prototype.return_type)?;
            env.function.insert_symbol(
                ret_name.to_string(),
                MIRExpression {
                    token_range: None,
                    kind: MIRExpressionKind::Variable(ret_name.clone()),
                    _type: mir_type,
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
