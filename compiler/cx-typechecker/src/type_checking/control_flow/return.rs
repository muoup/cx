use cx_mir::mir::{
    expression::{MIRExpression, MIRExpressionKind},
    program::EnvironmentNamespace,
    r#type::MIRType,
};
use cx_tokens::TokenRange;
use cx_util::CXResult;

use crate::{
    environment::{
        ScopeArrowSink, ScopeExitTarget, ScopeId, TypeEnvironment, symbols::SymbolValueOrigin,
    },
    log_typecheck_error,
    type_checking::{
        coercion::implicit::{implicit_cast, promotion::std_rval_promotion},
        control_flow::enqueue_jump_arrow,
        result::TypecheckResult,
        typechecker::typecheck_expr,
    },
};

fn typechange_can_forward_region(return_type: &MIRType) -> bool {
    return_type.is_structure()
        || return_type.is_union()
        || return_type.is_array()
        || return_type.is_opaque()
}

pub fn typecheck_return(
    env: &mut TypeEnvironment,
    namespace: &EnvironmentNamespace,
    value: Option<MIRExpression>,
) -> CXResult<TypecheckResult> {
    let return_type = env.current_function().return_type.clone();

    let return_value = match (value, &return_type) {
        (Some(mut some_value), return_type) if !return_type.is_unit() => {
            let mut _ty = some_value._type.clone();

            // If we are returning a copyable struct T, and we are given a &T, we can inline a bit
            // of the implicit cast behavior here so instead of creating a temporary buffer to copy
            // into, and then memcpy from that buffer, we can just "unsafely" coerce the &T to a T
            // so we will induce in effect just a direct memcpy from the source T to the return buffer.
            if let Some(inner) = env.symbols.mem_ref_inner(&_ty).cloned()
                && env.symbols.is_copyable(&inner)
                && typechange_can_forward_region(&inner)
            {
                some_value = MIRExpression {
                    _type: inner,
                    token_range: some_value.token_range.clone(),
                    kind: MIRExpressionKind::Typechange(Box::new(some_value)),
                };
            } else if env.symbols.mem_ref_inner(return_type).is_none() {
                some_value = std_rval_promotion(env, some_value)?;
            }

            Some(Box::new(implicit_cast(env, some_value, return_type)?))
        }

        (None, _) if return_type.is_unit() => None,

        (Some(value), _) => {
            return log_typecheck_error!(
                env,
                value.token_range.as_ref(),
                "Cannot return from function {} with a void return type",
                env.current_function().display_with(&env.symbols)
            );
        }

        (None, _) => {
            return log_typecheck_error!(
                env,
                Option::<TokenRange>::None,
                "Function {} expects a return value, but none was provided",
                env.current_function().display_with(&env.symbols)
            );
        }
    };

    enqueue_jump_arrow(
        env,
        &ScopeExitTarget {
            target_scope: ScopeId::new(0),
            sink: ScopeArrowSink::Merge,
            label: "return".to_string(),
        },
    );

    if let Some((ret_name, ret_contract)) = env.current_function().contract.postcondition.clone() {
        if ret_name.is_some() && return_type.is_unit() {
            return log_typecheck_error!(
                env,
                Option::<TokenRange>::None,
                "Cannot have a named return variable in a function with void return type"
            );
        }

        env.push_scope(false, false);

        for param in env.current_function().params.clone() {
            let Some(name) = param.name else {
                continue;
            };

            env.symbols.insert_value(
                name.clone(),
                MIRExpression {
                    kind: MIRExpressionKind::ContractVariable {
                        name: name.clone(),
                        force_param: true,
                    },
                    token_range: None,
                    _type: param._type.clone(),
                },
                Some(SymbolValueOrigin::Contract),
            );
        }

        if let Some(ret_name) = ret_name.as_ref() {
            env.symbols.insert_value(
                ret_name.clone(),
                MIRExpression {
                    kind: MIRExpressionKind::ContractVariable {
                        name: ret_name.clone(),
                        force_param: false,
                    },
                    token_range: None,
                    _type: return_type.clone(),
                },
                Some(SymbolValueOrigin::Contract),
            );
        }

        let postcondition = typecheck_expr(env, namespace, &ret_contract, None)
            .and_then(|v| implicit_cast(env, v.into_expression()?, &MIRType::bool()))?;

        env.pop_scope()?;

        Ok(TypecheckResult::new_base(
            MIRType::unit(),
            MIRExpressionKind::Return {
                value: return_value,
                postcondition: Some((ret_name.clone(), Box::new(postcondition))),
            },
        ))
    } else {
        Ok(TypecheckResult::new_base(
            MIRType::unit(),
            MIRExpressionKind::Return {
                value: return_value,
                postcondition: None,
            },
        ))
    }
}
