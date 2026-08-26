use cx_log::CXResult;
use cx_thir::{
    EnvironmentNamespace,
    thir::{
        expression::{THIRExpression, THIRExpressionKind},
        r#type::THIRType,
    },
    type_context::THIRTypeContext,
};
use cx_tokens::TokenRange;
use cx_util::namespace::QualifiedName;

use crate::{
    environment::{ScopeArrowSink, ScopeExitTarget, ScopeId, TypeEnvironment},
    type_checking::{
        coercion::implicit::{implicit_cast, promotion::std_rval_promotion},
        control_flow::enqueue_jump_arrow,
        result::TypecheckResult,
        typechecker::typecheck_expr,
    },
};

fn typechange_can_forward_region(return_type: &THIRType) -> bool {
    return_type.is_structure()
        || return_type.is_union()
        || return_type.is_array()
        || return_type.is_opaque()
}

pub fn typecheck_return(
    env: &mut TypeEnvironment,
    namespace: &EnvironmentNamespace,
    return_range: &cx_tokens::TokenRange,
    value: Option<THIRExpression>,
) -> CXResult<TypecheckResult> {
    if env.in_defer_context() {
        return env.log_error(
            return_range,
            "return is not allowed inside a deferred expression".to_string(),
        );
    }

    let return_type = if env.in_staged_context() || env.in_runtime_emit_context() {
        let Some(return_type) = env.materialization_return_type() else {
            return env.log_error(
                return_range,
                "staged return has no materialization context".to_string(),
            );
        };
        return_type
    } else {
        env.current_function().signature().return_type.clone()
    };

    if return_type.is_unreachable() {
        return env.log_error(
            return_range,
            format!(
                "Function {} cannot return because its return type is 'unreachable'",
                env.current_function().pretty_name()
            ),
        );
    }

    let return_value = match (value, &return_type) {
        (Some(mut some_value), return_type) if !return_type.is_void() => {
            let mut _ty = some_value._type.clone();

            // If we are returning a copyable struct T, and we are given a &T, we can inline a bit
            // of the implicit cast behavior here so instead of creating a temporary buffer to copy
            // into, and then memcpy from that buffer, we can just "unsafely" coerce the &T to a T
            // so we will induce in effect just a direct memcpy from the source T to the return buffer.
            if let Some(inner) = env.symbols.mem_ref_inner(&_ty).cloned()
                && !inner.is_nocopy()
                && typechange_can_forward_region(&inner)
            {
                some_value = THIRExpression {
                    _type: inner,
                    token_range: some_value.token_range.clone(),
                    kind: THIRExpressionKind::Typechange(Box::new(some_value)),
                };
            } else if env.symbols.mem_ref_inner(return_type).is_none() {
                some_value = std_rval_promotion(env, some_value)?;
            }

            Some(Box::new(implicit_cast(env, some_value, return_type)?))
        }

        (None, _) if return_type.is_void() => None,

        (Some(value), _) if env.in_comptime_context() => Some(Box::new(value)),

        (Some(value), _) => {
            return env.log_error(
                value.token_range,
                format!(
                    "Cannot return from function {} with a void return type",
                    env.current_function().pretty_name()
                ),
            );
        }

        (None, _) => {
            return env.log_error(
                return_range,
                format!(
                    "Function {} expects a return value, but none was provided",
                    env.current_function().pretty_name()
                ),
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

    if let Some((ret_name, ret_contract)) = env
        .current_function()
        .signature()
        .contract
        .postcondition
        .clone()
    {
        if ret_name.is_some() && return_type.is_void() {
            return env.log_error(
                return_range,
                "Cannot have a named return variable in a function with void return type"
                    .to_string(),
            );
        }

        env.push_scope(false, false);

        for param in env.current_function().signature().params.clone() {
            let Some(name) = param.name else {
                continue;
            };

            env.symbols.insert_local_value(
                QualifiedName::new_raw(name.clone()),
                THIRExpression {
                    kind: THIRExpressionKind::ContractVariable {
                        name: name.clone(),
                        force_param: true,
                    },
                    token_range: TokenRange::internal(),
                    _type: param._type.clone(),
                },
            );
        }

        if let Some(ret_name) = ret_name.as_ref() {
            env.symbols.insert_local_value(
                QualifiedName::new_raw(ret_name.clone()),
                THIRExpression {
                    kind: THIRExpressionKind::ContractVariable {
                        name: ret_name.clone(),
                        force_param: false,
                    },
                    token_range: TokenRange::internal(),
                    _type: return_type.clone(),
                },
            );
        }

        let postcondition = typecheck_expr(env, namespace, &ret_contract, None)
            .and_then(|res| res.standard_ready_coerce(env, ret_contract.token_range()))
            .and_then(|v| implicit_cast(env, v, &THIRType::bool()))?;
        let postcondition = THIRExpression {
            token_range: postcondition.token_range.clone(),
            kind: THIRExpressionKind::Assert {
                condition: Box::new(postcondition),
                message: "Postcondition Failed!".to_string(),
            },
            _type: THIRType::unit(),
        };

        env.pop_scope()
            .map_err(|err| env.complete_err(err, ret_contract.token_range()))?;

        Ok(TypecheckResult::new(
            THIRType::unit(),
            THIRExpressionKind::Return {
                value: return_value,
                postcondition: Some(cx_thir::thir::expression::THIRPostcondition {
                    binding: ret_name.clone(),
                    condition: Box::new(postcondition),
                }),
            },
        ))
    } else {
        Ok(TypecheckResult::new(
            THIRType::unit(),
            THIRExpressionKind::Return {
                value: return_value,
                postcondition: None,
            },
        ))
    }
}
