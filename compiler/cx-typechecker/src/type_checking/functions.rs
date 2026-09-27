use crate::{
    environment::{StagingContext, TypeEnvironment},
    symbol::completion::assert_valid_type_component,
    type_checking::control_flow::expr_may_fall_through,
    type_checking::control_flow::r#return::typecheck_return,
    type_checking::safety::{PermissionTier, check_permissions},
    type_checking::typechecker::typecheck_expr,
};
use cx_hir::ast::{
    expression::HIRExpression,
    function::{HIRFunctionBody, HIRFunctionContract},
};
use cx_log::CXResult;
use cx_log::catalogue::typecheck as catalogue;
use cx_namespace::module::{NamespacePath, QualifiedName};
use cx_thir::thir::{
    comptime::THIRComptimeFn,
    data::{
        THIRComptimeFnPrototype, THIRComptimeParameter, THIRFnPrototype, THIRFnSignature,
        THIRFunction, THIRFunctionBody, THIRParameter, THIRType,
    },
    expression::{THIRExpression, THIRExpressionKind},
    r#type::THIRTypeKind,
};
use cx_tokens::TokenRange;
use cx_util::{identifier::CXIdent, linkage::LinkageMode};

pub fn typecheck_function(
    env: &mut TypeEnvironment,
    namespace: &NamespacePath,
    prototype: THIRFnPrototype,
    body: &HIRFunctionBody,
) -> CXResult<()> {
    if prototype.signature().contract().safe && prototype.signature().var_args() {
        return env.log_error(
            body.token_range(),
            &catalogue::INVALID_CONTEXT,
            ("Varargs".into(), "a safe function".into()),
        );
    }

    let body = typecheck_function_scope(env, namespace, prototype.clone(), body, |env| {
        for param in prototype.signature().params() {
            assert_valid_type_component(env, body.token_range(), param.ty(), "a parameter", true)?;

            let Some(name) = param.name() else {
                continue;
            };

            let ref_type = env.symbols.mem_ref_to(param.ty().clone());

            env.symbols.insert_local_value(
                QualifiedName::new_raw(name.clone()),
                THIRExpression {
                    token_range: TokenRange::internal(),
                    kind: THIRExpressionKind::Variable {
                        name: name.clone(),
                        local_id: param.local_id(),
                    },
                    ty: ref_type,
                },
            );
        }

        Ok(())
    })?;

    let tier = PermissionTier::of_function(prototype.signature().contract().safe);
    for expr in body.exprs() {
        check_permissions(env, expr, tier)?;
    }

    env.items.push_generated_function(THIRFunction::new(
        prototype,
        Some(body),
        env.require_explicit_return(),
    ));

    Ok(())
}

pub fn typecheck_comptime_function(
    env: &mut TypeEnvironment,
    namespace: &NamespacePath,
    prototype: THIRComptimeFnPrototype,
    body: &HIRFunctionBody,
    context: StagingContext,
) -> CXResult<()> {
    let debug_name = prototype.debug_name().cloned();
    let return_type = prototype.return_type().ty().clone();

    let bookkeeping_params = prototype
        .params()
        .iter()
        .filter_map(|param| {
            Some(THIRParameter::new(
                Some(param.name().cloned()?),
                param.local_id(),
                if is_parameterized_staged(param) {
                    THIRTypeKind::Undefined.into()
                } else {
                    param.value_type().ty().clone()
                },
            ))
        })
        .collect();

    // Synthesize a plain prototype for environment bookkeeping; the rich
    // comptime prototype travels with the emitted function.
    let bookkeeping = THIRFnPrototype::new(
        prototype.symbol_name().to_owned(),
        LinkageMode::Static,
        THIRFnSignature::new(
            return_type,
            bookkeeping_params,
            false,
            HIRFunctionContract::default(),
        ),
    )
    .with_debug_name(debug_name.unwrap_or_else(|| CXIdent::new(prototype.pretty_name())));

    let previous_context = env.comptime_context.replace(context.clone());
    let checked = typecheck_function_scope(env, namespace, bookkeeping, body, |env| {
        for param in prototype.params() {
            let Some(name) = param.name().cloned() else {
                continue;
            };

            if is_parameterized_staged(param) {
                env.symbols.insert_local_staged_expression_function(
                    QualifiedName::new_raw(name),
                    param.local_id(),
                    param.value_type().params().to_vec(),
                    param.value_type().ty().clone(),
                );
                continue;
            }

            if !param.value_type().is_expr() {
                assert_valid_type_component(
                    env,
                    body.token_range(),
                    param.value_type().ty(),
                    "a parameter",
                    true,
                )?;
            }

            let local_type = if param.value_type().is_expr() {
                param.value_type().ty().clone()
            } else {
                env.symbols.mem_ref_to(param.value_type().ty().clone())
            };
            env.symbols.insert_local_value(
                QualifiedName::new_raw(name.clone()),
                THIRExpression {
                    token_range: TokenRange::internal(),
                    kind: THIRExpressionKind::Variable {
                        name,
                        local_id: param.local_id(),
                    },
                    ty: local_type,
                },
            );
        }

        Ok(())
    });
    env.comptime_context = previous_context;

    env.items
        .push_generated_comptime_function(THIRComptimeFn::new(prototype, Some(checked?), context));

    Ok(())
}

fn is_parameterized_staged(param: &THIRComptimeParameter) -> bool {
    param.value_type().is_expr() && !param.value_type().params().is_empty()
}

/// Typechecks `body` as the body of `prototype`, managing the function context and the
/// function's root scope. `bind_params` is run inside the root scope before the body is checked.
fn typecheck_function_scope(
    env: &mut TypeEnvironment,
    namespace: &NamespacePath,
    prototype: THIRFnPrototype,
    body: &HIRFunctionBody,
    bind_params: impl FnOnce(&mut TypeEnvironment) -> CXResult<()>,
) -> CXResult<THIRFunctionBody> {
    let return_type = prototype.signature().return_type().clone();

    env.function.begin_function(prototype);
    env.push_scope(false, false, body.token_range().clone());

    bind_params(env)?;
    let checked = typecheck_function_body(env, namespace, body, &return_type)?;

    if let Some((name, range)) = env.function.unresolved_label() {
        return env.log_error(range, &catalogue::UNKNOWN_SYMBOL, name.into());
    }

    env.pop_scope()
        .map_err(|err| env.complete_err(err, body.token_range()))?;
    env.function.end_function();

    Ok(checked)
}

fn typecheck_function_body(
    env: &mut TypeEnvironment,
    namespace: &NamespacePath,
    body: &HIRFunctionBody,
    return_type: &THIRType,
) -> CXResult<THIRFunctionBody> {
    let exprs = match body {
        HIRFunctionBody::Block { statements, .. } => statements
            .iter()
            .map(|statement| {
                typecheck_expr(env, namespace, statement, None)
                    .and_then(|result| result.standard_ready_coerce(env, statement.token_range()))
            })
            .collect::<CXResult<Vec<_>>>()?,
        HIRFunctionBody::Expression(expression) => {
            typecheck_expression_body(env, namespace, expression, return_type)?
        }
    };

    Ok(THIRFunctionBody::Block {
        exprs,
        token_range: body.token_range().clone(),
    })
}

/// Typechecks an expression-bodied function (`=> expr`) into the statements of an equivalent block
/// body, synthesizing the implicit return (and with it, any postcondition check).
fn typecheck_expression_body(
    env: &mut TypeEnvironment,
    namespace: &NamespacePath,
    expression: &HIRExpression,
    return_type: &THIRType,
) -> CXResult<Vec<THIRExpression>> {
    let value = if return_type.is_unreachable() {
        typecheck_expr(env, namespace, expression, None)?
            .standard_ready_coerce(env, expression.token_range())?
    } else {
        typecheck_expr(
            env,
            namespace,
            expression,
            (!return_type.is_void()).then_some(return_type),
        )?
        .apply_expected_type(env, namespace, return_type)?
        .standard_ready_coerce(env, expression.token_range())?
    };

    if value.ty.is_unreachable() || !expr_may_fall_through(&value) {
        return Ok(vec![value]);
    }

    if return_type.is_unreachable() {
        return typecheck_return(env, namespace, expression.token_range(), None)
            .map(|_| Vec::new());
    }

    if return_type.is_void() && value.ty.is_void() {
        let mut statements = vec![value];
        statements.push(
            typecheck_return(env, namespace, expression.token_range(), None)?
                .internal_ready_assertion(),
        );
        return Ok(statements);
    }

    Ok(vec![
        typecheck_return(env, namespace, expression.token_range(), Some(value))?
            .internal_ready_assertion(),
    ])
}
