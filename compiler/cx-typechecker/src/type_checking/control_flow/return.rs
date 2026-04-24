use cx_mir::mir::{
    expression::{MIRExpression, MIRExpressionKind},
    program::MIRBaseMappings,
    r#type::MIRType,
};
use cx_tokens::TokenRange;
use cx_util::CXResult;

use crate::{
    environment::{ScopeArrowSink, ScopeExitTarget, ScopeId, TypeEnvironment},
    log_typecheck_error,
    type_checking::{
        coercion::implicit::implicit_cast, control_flow::enqueue_jump_arrow,
        result::TypecheckResult, typechecker::typecheck_expr,
    },
};

pub fn typecheck_return(
    env: &mut TypeEnvironment,
    base_data: &MIRBaseMappings,
    value: Option<MIRExpression>,
) -> CXResult<TypecheckResult> {
    let return_type = env.current_function().return_type.clone();

    let value = match (value, &return_type) {
        (Some(mut some_value), return_type) if !return_type.is_unit() => {
            let mut _ty = some_value._type.clone();

            // If we are returning a copyable struct T, and we are given a &T, we can inline a bit
            // of the implicit cast behavior here so instead of creating a temporary buffer to copy
            // into, and then memcpy from that buffer, we can just "unsafely" coerce the &T to a T
            // so we will induce in effect just a direct memcpy from the source T to the return buffer.
            if let Some(inner) = env.symbols.context.mem_ref_inner(&_ty).cloned()
                && env.symbols.is_copyable(&inner)
                && return_type.is_memory_resident()
            {
                some_value = MIRExpression {
                    _type: inner,
                    token_range: some_value.token_range.clone(),
                    kind: MIRExpressionKind::Typechange(Box::new(some_value)),
                };
            }

            Some(Box::new(implicit_cast(env, some_value, return_type)?))
        }

        (None, _) if return_type.is_unit() => None,

        (Some(value), _) => {
            return log_typecheck_error!(
                env,
                value.token_range.as_ref(),
                "Cannot return from function {} with a void return type",
                env.current_function()
                    .display_with(&env.symbols.context)
            );
        }

        (None, _) => {
            return log_typecheck_error!(
                env,
                Option::<TokenRange>::None,
                "Function {} expects a return value, but none was provided",
                env.current_function()
                    .display_with(&env.symbols.context)
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

    if let Some((Some(ret_name), ret_contract)) =
        env.current_function().contract.postcondition.clone()
    {
        env.function.push_scope(false, false);

        let ret_val = MIRExpression {
            _type: env.symbols.context.mem_ref_to(return_type.clone()),
            token_range: None,
            kind: MIRExpressionKind::RegionCreate {
                name: Some(ret_name.clone()),
                _type: return_type.clone(),
                initial_value: value,
            },
        };

        for param in env.current_function().params.clone() {
            let Some(name) = param.name else {
                continue;
            };

            env.function.insert_symbol(
                name.as_string(),
                MIRExpression {
                    kind: MIRExpressionKind::ContractVariable(name.clone()),
                    token_range: None,
                    _type: param._type,
                },
            );
        }

        env.function.insert_symbol(
            ret_name.as_string(),
            MIRExpression {
                kind: MIRExpressionKind::Variable(ret_name.clone()),
                token_range: None,
                _type: ret_val._type.clone(),
            },
        );

        let postcondition = typecheck_expr(env, base_data, &ret_contract, None)?.into_expression();

        env.function
            .pop_scope(env.source.compilation_unit.as_path())?;

        Ok(TypecheckResult::new_base(
            MIRType::unit(),
            MIRExpressionKind::Return {
                value: Some(Box::new(ret_val)),
                postcondition: Some(Box::new(postcondition)),
            },
        ))
    } else {
        Ok(TypecheckResult::new_base(
            MIRType::unit(),
            MIRExpressionKind::Return {
                value,
                postcondition: None,
            },
        ))
    }
}
