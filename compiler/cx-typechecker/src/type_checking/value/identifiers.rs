use crate::{
    environment::TypeEnvironment,
    log_typecheck_error,
    symbol::{
        completion::complete_template_input, resolution::apply_template,
        type_constructor::query_type_constructor,
    },
    type_checking::result::{TypecheckResult, TypecheckedBinding},
};
use cx_ast::ast::{expression::CXExpression, template::CXTemplateInput};
use cx_mir::{
    EnvironmentNamespace,
    mir::expression::{MIRExpressionKind, SymbolValueOrigin},
    symbol::MIRSymbol,
};
use cx_util::{CXResult, namespace::QualifiedName};

pub(crate) fn typecheck_identifier(
    env: &mut TypeEnvironment,
    namespace: &EnvironmentNamespace,
    expr: &CXExpression,
    name: &QualifiedName,
    template_input: Option<&CXTemplateInput>,
) -> CXResult<TypecheckResult> {
    let (mut symbol, template_input_consumed) = match env.get_symbol(name)? {
        Some(symbol) => (symbol, false),
        None => {
            let Some(constructor) = query_type_constructor(env, namespace, name, template_input)?
            else {
                return log_typecheck_error!(
                    env,
                    Some(expr.token_range()),
                    "Identifier '{}' not found",
                    name
                );
            };

            (constructor, true)
        }
    };

    if !template_input_consumed
        && let Some(completed_input) = template_input
            .map(|input| complete_template_input(env, namespace, input))
            .transpose()?
    {
        symbol = apply_template(env, &symbol, completed_input)?.unwrap();
    }

    match symbol {
        MIRSymbol::Expression(value) => {
            let origin = match &value.kind {
                MIRExpressionKind::Variable { location, .. } => Some(*location),
                _ => None,
            };

            if origin == Some(SymbolValueOrigin::Global) && env.function.in_safe_context() {
                return log_typecheck_error!(
                    env,
                    Some(expr.token_range()),
                    "Safe functions may not access global variables"
                );
            }

            let result = TypecheckResult::from(value);

            if matches!(origin, Some(SymbolValueOrigin::Local)) {
                Ok(result.with_binding(TypecheckedBinding::local(name.name.clone())))
            } else {
                Ok(result)
            }
        }

        MIRSymbol::Template { .. } => Ok(TypecheckResult::incomplete_templated_callee(
            name.clone(),
            None,
        )),

        MIRSymbol::Type(_) => log_typecheck_error!(
            env,
            Some(expr.token_range()),
            "Type '{}' cannot be used as a value",
            name
        ),
    }
}
