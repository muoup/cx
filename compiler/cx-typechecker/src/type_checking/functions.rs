use crate::{
    environment::TypeEnvironment,
    symbol::completion::ensure_valid_type_component,
    type_checking::typechecker::{add_implicit_return, typecheck_expr},
};
use cx_hir::ast::expression::HIRExpression;
use cx_hir::ast::function::HIRFunctionContract;
use cx_log::CXResult;
use cx_thir::{
    EnvironmentNamespace,
    thir::{
        comptime::THIRComptimeFn,
        data::{
            THIRComptimeFnPrototype, THIRFnPrototype, THIRFnSignature, THIRFunction, THIRParameter,
        },
        expression::{THIRExpression, THIRExpressionKind},
        r#type::THIRTypeKind,
    },
};
use cx_tokens::TokenRange;
use cx_util::{identifier::CXIdent, linkage::LinkageMode, namespace::QualifiedName};

pub fn typecheck_function(
    env: &mut TypeEnvironment,
    namespace: &EnvironmentNamespace,
    prototype: THIRFnPrototype,
    body: &HIRExpression,
) -> CXResult<()> {
    if prototype.signature().contract.safe && prototype.signature().var_args {
        return env.log_error(
            body.token_range(),
            format!(
                "Safe function '{}' may not use varargs",
                prototype.pretty_name()
            ),
        );
    }

    env.function.begin_function(prototype.clone());
    env.function.flow_mut().push_scope(false, false, body.token_range().clone());

    for THIRParameter {
        name,
        local_id,
        _type,
    } in prototype.signature().params.iter()
    {
        ensure_valid_type_component(env, body.token_range(), _type, "a parameter", true)?;

        let Some(name) = name else {
            continue;
        };

        let ref_type = env.symbols.mem_ref_to(_type.clone());

        env.symbols.insert_local_value(
            QualifiedName::new_raw(name.clone()),
            THIRExpression {
                token_range: TokenRange::internal(),
                kind: THIRExpressionKind::Variable {
                    name: name.clone(),
                    local_id: *local_id,
                },
                _type: ref_type,
            },
        );
    }

    let body_expr = typecheck_expr(env, namespace, body, None)
        .and_then(|v| v.standard_ready_coerce(env, body.token_range()))?;
    let with_implicit_return = add_implicit_return(env, namespace, body_expr)?;

    if let Some((name, range)) = env.function.unresolved_label() {
        return env.log_error(range, format!("Undefined label '{name}'"));
    }

    if prototype.signature().contract.safe {
        crate::type_checking::safety::validate_safe_expression(env, &with_implicit_return)?;
    }

    env.pop_scope()
        .map_err(|err| env.complete_err(err, body.token_range()))?;
    env.function.end_function();

    env.items.push_generated_function(THIRFunction {
        prototype,
        body: Some(with_implicit_return),
    });

    Ok(())
}

/// Typechecks a comptime function body and emits it into the completed THIR.
///
/// Comptime functions are checked mostly like normal functions. Plain
/// parameters and non-parameterized staged parameters (`expr T`) behave like
/// normal typed locals; parameterized staged parameters (`expr(P) T`) bind as
/// staged values that carry no static type and may only be called or passed
/// to other staged parameters.
pub fn typecheck_comptime_function(
    env: &mut TypeEnvironment,
    namespace: &EnvironmentNamespace,
    prototype: THIRComptimeFnPrototype,
    body: &HIRExpression,
) -> CXResult<()> {
    let debug_name = prototype.debug_name().cloned();
    let return_type = prototype.return_type()._type.clone();

    let mut bookkeeping_params = Vec::with_capacity(prototype.params().len());
    for param in prototype.params() {
        let Some(name) = param.name.clone() else {
            continue;
        };
        let local_id = param.local_id;
        let is_parameterized_staged = param.value_type.expr && !param.value_type.params.is_empty();

        if is_parameterized_staged {
            env.symbols.insert_local_staged_expression_function(
                QualifiedName::new_raw(name.clone()),
                local_id,
                param.value_type.params.clone(),
                param.value_type._type.clone(),
            );
        } else {
            if !param.value_type.expr {
                ensure_valid_type_component(
                    env,
                    body.token_range(),
                    &param.value_type._type,
                    "a parameter",
                    true,
                )?;
            }

            let local_type = if param.value_type.expr {
                param.value_type._type.clone()
            } else {
                env.symbols.mem_ref_to(param.value_type._type.clone())
            };
            env.symbols.insert_local_value(
                QualifiedName::new_raw(name.clone()),
                THIRExpression {
                    token_range: TokenRange::internal(),
                    kind: THIRExpressionKind::Variable {
                        name: name.clone(),
                        local_id,
                    },
                    _type: local_type,
                },
            );
        }

        bookkeeping_params.push(THIRParameter {
            name: Some(name),
            local_id,
            _type: if is_parameterized_staged {
                THIRTypeKind::Undefined.into()
            } else {
                param.value_type._type.clone()
            },
        });
    }

    // Synthesize a plain prototype for environment bookkeeping; the rich
    // comptime prototype travels with the emitted function.
    let bookkeeping = THIRFnPrototype::new(
        prototype.symbol_name().to_owned(),
        LinkageMode::Static,
        THIRFnSignature {
            return_type,
            params: bookkeeping_params,
            var_args: false,
            contract: HIRFunctionContract::default(),
        },
    )
    .with_debug_name(debug_name.unwrap_or_else(|| CXIdent::new(prototype.pretty_name())));

    env.function.begin_function(bookkeeping);

    env.enter_comptime_context(prototype.runtime_return_type().cloned());
    let checked = (|| -> CXResult<THIRExpression> {
        let body_expr = typecheck_expr(env, namespace, body, None)?
            .standard_ready_coerce(env, body.token_range())?;
        add_implicit_return(env, namespace, body_expr)
    })();
    env.exit_comptime_context();
    let with_implicit_return = checked?;

    if let Some((name, range)) = env.function.unresolved_label() {
        return env.log_error(range, format!("Undefined label '{name}'"));
    }

    env.pop_scope()
        .map_err(|err| env.complete_err(err, body.token_range()))?;
    env.function.end_function();

    env.items.push_generated_comptime_function(THIRComptimeFn {
        prototype,
        body: Some(with_implicit_return),
    });

    Ok(())
}
