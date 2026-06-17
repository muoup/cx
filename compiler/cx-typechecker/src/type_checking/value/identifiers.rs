use crate::{
    environment::TypeEnvironment,
    log_typecheck_error,
    symbol::{completion::complete_template_input, resolution::apply_template},
    type_checking::result::{TypecheckResult, TypecheckedBinding},
    typecheck_error,
};
use cx_ast::ast::{expression::CXExpression, template::CXTemplateInput};
use cx_log::CXResult;
use cx_mir::{
    EnvironmentNamespace,
    mir::expression::{MIRExpressionKind, SymbolValueOrigin},
};
use cx_util::namespace::QualifiedName;

pub(crate) fn typecheck_identifier(
    env: &mut TypeEnvironment,
    namespace: &EnvironmentNamespace,
    expr: &CXExpression,
    name: &QualifiedName,
    template_input: Option<&CXTemplateInput>,
) -> CXResult<TypecheckResult> {
    let Some(mut symbol) = env.get_symbol(namespace, name, Some(expr.token_range()))? else {
        return log_typecheck_error!(env, expr.token_range(), "Identifier '{}' not found", name);
    };

    if let Some(completed_input) = template_input
        .map(|input| complete_template_input(env, namespace, input))
        .transpose()?
    {
        symbol = apply_template(env, &symbol, completed_input)?.unwrap();
    }

    let result = TypecheckResult::from_symbol(symbol, name.clone(), template_input.cloned())
        .map_err(|err| typecheck_error!(env, expr.token_range(), "{}", err.error_content()))?;

    let binding = match result.ready_expression().map(|expr| &expr.kind) {
        Some(MIRExpressionKind::Variable {
            name,
            location: SymbolValueOrigin::Local,
        }) => Some(TypecheckedBinding::local(name.clone())),
        _ => None,
    };

    Ok(match binding {
        Some(binding) => result.with_binding(binding),
        None => result,
    })
}
