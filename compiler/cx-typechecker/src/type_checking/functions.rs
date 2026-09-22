use crate::{
    environment::{StagingContext, TypeEnvironment},
    symbol::completion::ensure_valid_type_component,
    type_checking::control_flow::expr_may_fall_through,
    type_checking::control_flow::r#return::typecheck_return,
    type_checking::typechecker::{add_implicit_return, typecheck_expr},
};
use cx_hir::ast::expression::HIRExpression;
use cx_hir::ast::function::{HIRFunctionBody, HIRFunctionContract};
use cx_log::CXResult;
use cx_log::catalogue::typecheck as catalogue;
use cx_namespace::module::{NamespacePath, QualifiedName};
use cx_thir::thir::{
    comptime::THIRComptimeFn,
    data::{
        THIRComptimeFnPrototype, THIRFnPrototype, THIRFnSignature, THIRFunction,
        THIRFunctionBody, THIRParameter,
    },
    expression::{THIRBlockKind, THIRExpression, THIRExpressionKind},
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
    if prototype.signature().contract.safe && prototype.signature().var_args {
        return env.log_error(
            body.token_range(),
            &catalogue::INVALID_CONTEXT,
            ("Varargs".into(), "a safe function".into()),
        );
    }

    env.function.begin_function(prototype.clone());
    env.push_scope(false, false, body.token_range().clone());

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

    let statements = typecheck_function_body(
        env,
        namespace,
        body,
        &prototype.signature().return_type,
    )?;

    if let Some((name, range)) = env.function.unresolved_label() {
        return env.log_error(range, &catalogue::UNKNOWN_SYMBOL, name.into());
    }

    if prototype.signature().contract.safe {
        let safety_body = sequence_expression(statements.clone(), body.token_range().clone());
        crate::type_checking::safety::validate_safe_expression(env, &safety_body)?;
    }

    env.pop_scope()
        .map_err(|err| env.complete_err(err, body.token_range()))?;
    env.function.end_function();

    env.items.push_generated_function(THIRFunction {
        require_explicit_return: env.require_explicit_return(),
        prototype,
        body: Some(THIRFunctionBody::Block {
            exprs: statements,
            token_range: body.token_range().clone(),
        }),
    });

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
    let return_type = prototype.return_type()._type.clone();

    let mut bookkeeping_params = Vec::with_capacity(prototype.params().len());
    for param in prototype.params() {
        let Some(name) = param.name.clone() else {
            continue;
        };
        let is_parameterized_staged = param.value_type.expr && !param.value_type.params.is_empty();

        if is_parameterized_staged {
            env.symbols.insert_local_staged_expression_function(
                QualifiedName::new_raw(name.clone()),
                param.local_id,
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
                        local_id: param.local_id,
                    },
                    _type: local_type,
                },
            );
        }

        bookkeeping_params.push(THIRParameter {
            name: Some(name),
            local_id: param.local_id,
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
    env.push_scope(false, false, body.token_range().clone());
    let previous_context = env.comptime_context.replace(context.clone());

    let checked = typecheck_function_body(env, namespace, body, &prototype.return_type()._type);

    env.comptime_context = previous_context;
    let statements = checked?;

    if let Some((name, range)) = env.function.unresolved_label() {
        return env.log_error(range, &catalogue::UNKNOWN_SYMBOL, name.into());
    }

    env.pop_scope()
        .map_err(|err| env.complete_err(err, body.token_range()))?;
    env.function.end_function();

    env.items.push_generated_comptime_function(THIRComptimeFn {
        prototype,
        body: Some(THIRFunctionBody::Block {
            exprs: statements,
            token_range: body.token_range().clone(),
        }),
        context,
    });

    Ok(())
}

fn typecheck_function_body(
    env: &mut TypeEnvironment,
    namespace: &NamespacePath,
    body: &HIRFunctionBody,
    return_type: &cx_thir::thir::data::THIRType,
) -> CXResult<Vec<THIRExpression>> {
    match body {
        HIRFunctionBody::Block { statements, range } => {
            let statements = statements
                .iter()
                .map(|statement| {
                    typecheck_expr(env, namespace, statement, None)
                        .and_then(|result| result.standard_ready_coerce(env, statement.token_range()))
                })
                .collect::<CXResult<Vec<_>>>()?;
            add_implicit_return(env, namespace, statements, range.clone())
        }
        HIRFunctionBody::Expression(expression) => {
            let value = typecheck_expr(
                env,
                namespace,
                expression,
                (!return_type.is_void()).then_some(return_type),
            )?
            .apply_expected_type(env, namespace, return_type)?
            .standard_ready_coerce(env, expression.token_range())?;

            if value._type.is_unreachable() || !expr_may_fall_through(&value) {
                return Ok(vec![value]);
            }

            if return_type.is_void() && value._type.is_void() {
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
    }
}

fn sequence_expression(statements: Vec<THIRExpression>, token_range: TokenRange) -> THIRExpression {
    THIRExpression {
        kind: THIRExpressionKind::Block {
            statements,
            kind: THIRBlockKind::Sequence,
            yields: false,
        },
        _type: cx_thir::thir::data::THIRType::unit(),
        token_range,
    }
}
