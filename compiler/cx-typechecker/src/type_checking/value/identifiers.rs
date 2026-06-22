use crate::{
    environment::TypeEnvironment,
    symbol::{completion::complete_template_input, resolution::apply_template},
    type_checking::{
        coercion::implicit::{implicit_cast, promotion::std_rval_promotion},
        result::{TypecheckResult, TypecheckedBinding},
        typechecker::typecheck_expr,
    },
};
use cx_ast::ast::{expression::CXExpression, template::CXTemplateInput};
use cx_log::CXResult;
use cx_mir::{
    EnvironmentNamespace,
    mir::expression::{MIRExpressionKind, SymbolValueOrigin},
    symbol::MIRSymbol,
};
use cx_util::namespace::QualifiedName;

pub(crate) fn typecheck_identifier(
    env: &mut TypeEnvironment,
    namespace: &EnvironmentNamespace,
    expr: &CXExpression,
    name: &QualifiedName,
    template_input: Option<&CXTemplateInput>,
) -> CXResult<TypecheckResult> {
    let Some(mut symbol) = env.get_symbol(namespace, name)? else {
        return env.log_error(
            expr.token_range(),
            format!("Identifier '{}' not found", name),
        );
    };

    if let MIRSymbol::StagedExpression {
        namespace,
        expr: staged_expr,
        expected_type,
    } = symbol
    {
        let staged = typecheck_expr(env, &namespace, &staged_expr, Some(&expected_type))?;
        let staged = staged.standard_ready_coerce(env, staged_expr.token_range())?;

        let staged = if env.type_eq(&staged._type, &expected_type) {
            staged
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
        .map_err(|err| env.error(expr.token_range(), format!("{}", err.message())))?;

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
