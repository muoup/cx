use crate::{
    environment::TypeEnvironment,
    type_checking::{
        typechecker::{add_implicit_return, typecheck_expr},
        value::ensure_valid_allocation_type,
    },
};
use cx_hir::ast::expression::HIRExpression;
use cx_log::CXResult;
use cx_thir::{
    EnvironmentNamespace,
    thir::{
        data::{THIRFnPrototype, THIRFunction, THIRParameter},
        expression::{THIRExpression, THIRExpressionKind},
    },
};
use cx_tokens::TokenRange;
use cx_util::namespace::QualifiedName;

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
    env.push_scope(false, false);
    env.function.set_scope_anchor(body);
    env.function
        .configure_merge_scope(body, Some("fallthrough"));

    for THIRParameter {
        name,
        local_id,
        _type,
    } in prototype.signature().params.iter()
    {
        let Some(name) = name else {
            continue;
        };
        let local_id = local_id.expect("named MIR parameter is missing a local id");
        ensure_valid_allocation_type(env, body.token_range().clone(), "a parameter", _type)?;
        let ref_type = env.symbols.mem_ref_to(_type.clone());

        env.symbols.insert_local_value(
            QualifiedName::new_raw(name.clone()),
            THIRExpression {
                token_range: TokenRange::internal(),
                kind: THIRExpressionKind::Variable {
                    name: name.clone(),
                    local_id,
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
        body: with_implicit_return,
    });

    Ok(())
}
