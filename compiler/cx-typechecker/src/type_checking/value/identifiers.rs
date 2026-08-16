use crate::{
    environment::TypeEnvironment,
    symbol::{completion::complete_template_input, resolution::apply_template},
    type_checking::{
        coercion::implicit::{implicit_cast, promotion::std_rval_promotion},
        result::{TypecheckResult, TypecheckedBinding},
        typechecker::typecheck_expr,
    },
};
use cx_hir::ast::{expression::HIRExpression, template::HIRTemplateInput};
use cx_log::CXResult;
use cx_thir::{
    EnvironmentNamespace,
    symbol::MIRSymbol,
    thir::{data::THIRTypeKind, expression::THIRExpressionKind},
};
use cx_util::namespace::QualifiedName;

pub(crate) fn typecheck_identifier(
    env: &mut TypeEnvironment,
    namespace: &EnvironmentNamespace,
    expr: &HIRExpression,
    name: &QualifiedName,
    template_input: Option<&HIRTemplateInput>,
) -> CXResult<TypecheckResult> {
    let Some(mut symbol) = env.get_symbol(namespace, name)? else {
        return env.log_error(
            expr.token_range(),
            format!("Identifier '{}' not found", name),
        );
    };

    if let MIRSymbol::StagedExpression {
        id,
        namespace,
        expr: staged_expr,
        expected_type,
    } = symbol
    {
        env.push_staged_expansion(id);
        let staged = typecheck_expr(env, &namespace, &staged_expr, Some(&expected_type));
        env.pop_staged_expansion();
        let staged = staged?;
        let staged = staged.standard_ready_coerce(env, staged_expr.token_range())?;

        let staged = if env.type_eq(&staged._type, &expected_type) {
            staged
        } else if expected_type.is_memory_reference() {
            implicit_cast(env, staged, &expected_type)?
        } else {
            let staged = std_rval_promotion(env, staged)?;
            implicit_cast(env, staged, &expected_type)?
        };

        return Ok(TypecheckResult::from(staged));
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
        .map_err(|err| env.error(expr.token_range(), err.message().to_string()))?;

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
            format!(
                "References to unsafe function `{display_name}` may not be used in safe contexts"
            ),
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
