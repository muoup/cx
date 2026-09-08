use crate::{
    environment::TypeEnvironment,
    symbol::{completion::complete_template_input, template::apply_template},
    type_checking::result::{StagedBindingTC, TypecheckResult, TypecheckedBinding},
};
use cx_hir::ast::{expression::HIRExpression, template::HIRTemplateInput};
use cx_log::CXResult;
use cx_log::catalogue::typecheck as catalogue;
use cx_namespace::module::NamespacePath;
use cx_namespace::module::QualifiedName;
use cx_thir::{
    symbol::MIRSymbol,
    thir::{
        data::THIRTypeKind,
        expression::{THIRExpression, THIRExpressionKind},
    },
};

pub(crate) fn typecheck_identifier(
    env: &mut TypeEnvironment,
    namespace: &NamespacePath,
    expr: &HIRExpression,
    name: &QualifiedName,
    template_input: Option<&HIRTemplateInput>,
) -> CXResult<TypecheckResult> {
    let Some(mut symbol) = env.get_symbol(namespace, name)? else {
        return env.log_error(
            expr.token_range(),
            &catalogue::IDENTIFIER_NOT_FOUND,
            format!("{}", name),
        );
    };

    // A local staged binding (e.g. a parameterized staged parameter of a
    // comptime function) resolves to an undefined-typed reference that may
    // only be called or passed onward.
    if let MIRSymbol::StagedExpressionFunction {
        local_id,
        params,
        return_type,
    } = symbol
    {
        return Ok(TypecheckResult::staged_binding(StagedBindingTC {
            reference: THIRExpression {
                token_range: expr.token_range().clone(),
                kind: THIRExpressionKind::Variable {
                    name: name.name.clone(),
                    local_id,
                },
                _type: THIRTypeKind::Undefined.into(),
            },
            params,
            return_type,
        }));
    }

    if let Some(completed_input) = template_input
        .map(|input| complete_template_input(env, namespace, input))
        .transpose()?
    {
        symbol = apply_template(env, &symbol, completed_input)
            .map_err(|err| env.complete_maybe_err(err, expr.token_range()))?
            .unwrap();
    }

    let result = TypecheckResult::from_symbol(symbol, name.clone(), template_input.cloned())
        .map_err(|err| env.complete_err(err, expr.token_range()))?;

    if env.function.in_safe_context()
        && let Some(expression) = result.ready_expression()
        && let THIRExpressionKind::FunctionReference {
            name: symbol_name,
            debug_name,
        } = &expression.kind
        && let THIRTypeKind::Function { signature } = &expression._type.kind
        && !signature.contract.safe
    {
        let display_name = debug_name.as_ref().unwrap_or(symbol_name);
        return env.log_error(
            expr.token_range(),
            &catalogue::REFERENCES_TO_UNSAFE_FUNCTION_MAY_NOT_BE_USED_IN_SAFE,
            format!("{}", display_name),
        );
    }

    let binding = match result.ready_expression().map(|expr| &expr.kind) {
        Some(THIRExpressionKind::Variable { name, local_id }) => {
            Some(TypecheckedBinding::local(name.clone(), *local_id))
        }
        _ => None,
    };

    Ok(match binding {
        Some(binding) => result.with_binding(binding),
        None => result,
    })
}
