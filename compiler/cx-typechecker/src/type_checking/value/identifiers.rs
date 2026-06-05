use crate::{
    environment::TypeEnvironment, log_typecheck_error, symbol::{completion::complete_template_input, resolution::apply_template}, type_checking::result::{TypecheckResult, TypecheckedBinding}
};
use cx_ast::ast::{expression::CXExpression, template::CXTemplateInput};
use cx_mir::{
    EnvironmentNamespace, mir::expression::{MIRExpressionKind, SymbolValueOrigin}, symbol::MIRSymbol
};
use cx_util::{CXResult, namespace::QualifiedName};

pub(crate) fn typecheck_identifier(
    env: &mut TypeEnvironment,
    _namespace: &EnvironmentNamespace,
    expr: &CXExpression,
    name: &QualifiedName,
) -> CXResult<TypecheckResult> {
    let Some(symbol) = env.get_symbol(name)? else {
        return identifier_not_found(env, expr, name);
    };

    symbol_to_typecheck_result(env, expr, name, symbol)
}

pub(crate) fn typecheck_templated_identifier(
    env: &mut TypeEnvironment,
    namespace: &EnvironmentNamespace,
    expr: &CXExpression,
    name: &QualifiedName,
    template_input: &CXTemplateInput,
) -> CXResult<TypecheckResult> {
    let Some(symbol) = env.get_symbol(name)? else {
        return identifier_not_found(env, expr, name);
    };

    let completed_input = complete_template_input(env, namespace, template_input)?;

    let Some(symbol) = apply_template(env, &symbol, completed_input)? else {
        return log_typecheck_error!(
            env,
            Some(expr.token_range()),
            "Identifier '{}' does not accept template arguments",
            name
        );
    };

    symbol_to_typecheck_result(env, expr, name, symbol)
}

fn symbol_to_typecheck_result(
    env: &mut TypeEnvironment,
    expr: &CXExpression,
    name: &QualifiedName,
    symbol: MIRSymbol,
) -> CXResult<TypecheckResult> {
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
            if matches!(
                origin,
                Some(SymbolValueOrigin::Local | SymbolValueOrigin::Contract)
            ) {
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

fn identifier_not_found(
    env: &mut TypeEnvironment,
    expr: &CXExpression,
    name: &QualifiedName,
) -> CXResult<TypecheckResult> {
    log_typecheck_error!(
        env,
        Some(expr.token_range()),
        "Identifier '{}' not found",
        name
    )
}
