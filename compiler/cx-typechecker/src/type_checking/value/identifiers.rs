use crate::{
    environment::TypeEnvironment,
    log_typecheck_error,
    symbol::{completion::complete_template_input, resolution::apply_template},
    type_checking::result::{TypecheckResult, TypecheckedBinding},
};
use cx_ast::ast::{expression::CXExpression, template::CXTemplateInput};
use cx_mir::{
    EnvironmentNamespace,
    mir::expression::{MIRExpressionKind, SymbolValueOrigin},
};
use cx_util::{CXResult, namespace::QualifiedName};

pub(crate) fn typecheck_identifier(
    env: &mut TypeEnvironment,
    namespace: &EnvironmentNamespace,
    expr: &CXExpression,
    name: &QualifiedName,
    template_input: Option<&CXTemplateInput>,
) -> CXResult<TypecheckResult> {
    let Some(mut symbol) = env.get_symbol(namespace, name)? else {
        return log_typecheck_error!(
            env,
            Some(expr.token_range()),
            "Identifier '{}' not found",
            name
        );
    };

    if let Some(completed_input) = template_input
        .map(|input| complete_template_input(env, namespace, input))
        .transpose()?
    {
        symbol = apply_template(env, &symbol, completed_input)?.unwrap();
    }

    let Some(expr) = symbol.as_expression() else {
        return log_typecheck_error!(
            env,
            Some(expr.token_range()),
            "Identifier '{}' does not refer to a value",
            name
        );
    };

    Ok(match &expr.kind {
        MIRExpressionKind::Variable {
            name,
            location: SymbolValueOrigin::Local,
        } => {
            let name = name.clone();
            TypecheckResult::from(expr).with_binding(TypecheckedBinding::local(name))
        }
        
        _ => TypecheckResult::from(expr),
    })
}
