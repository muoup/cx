use crate::{
    environment::TypeEnvironment,
    type_checking::{
        typechecker::{add_implicit_return, typecheck_expr},
        value::ensure_valid_allocation_type,
    },
};
use cx_ast::ast::expression::CXExpression;
use cx_log::CXResult;
use cx_mir::{
    EnvironmentNamespace,
    mir::{
        data::{MIRFunction, MIRFunctionPrototype, MIRParameter},
        expression::{MIRExpression, MIRExpressionKind, SymbolValueOrigin},
    },
};
use cx_tokens::TokenRange;
use cx_util::namespace::QualifiedName;

pub fn typecheck_function(
    env: &mut TypeEnvironment,
    namespace: &EnvironmentNamespace,
    prototype: MIRFunctionPrototype,
    body: &CXExpression,
) -> CXResult<()> {
    env.function.begin_function(prototype.clone());
    env.push_scope(false, false);
    env.function.set_scope_anchor(body);
    env.function
        .configure_merge_scope(body, Some("fallthrough"), true);

    for MIRParameter { name, _type } in prototype.signature().params.iter() {
        let Some(name) = name else {
            continue;
        };
        ensure_valid_allocation_type(env, body.token_range().clone(), "a parameter", _type)?;
        let ref_type = env.symbols.mem_ref_to(_type.clone());

        env.symbols.insert_local_value(
            QualifiedName::new_raw(name.clone()),
            MIRExpression {
                token_range: TokenRange::internal(),
                kind: MIRExpressionKind::Variable {
                    name: name.clone(),
                    location: SymbolValueOrigin::Local,
                },
                _type: ref_type,
            },
        );
        env.function
            .track_binding(name.as_string(), _type.is_nodrop());
    }

    let body_expr = typecheck_expr(env, namespace, body, None)
        .and_then(|v| v.standard_ready_coerce(env, body.token_range()))?;
    let with_implicit_return = add_implicit_return(env, namespace, body_expr)?;

    env.pop_scope()?;
    env.function.end_function();

    env.items.push_generated_function(MIRFunction {
        prototype,
        body: with_implicit_return,
    });

    Ok(())
}
