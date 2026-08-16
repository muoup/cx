use crate::{
    environment::TypeEnvironment, type_checking::result::TypecheckResult,
    type_checking::typechecker::typecheck_expr,
};
use cx_hir::ast::expression::HIRExpression;
use cx_log::CXResult;
use cx_thir::{
    EnvironmentNamespace,
    thir::{data::THIRType, expression::THIRExpressionKind},
};

pub(crate) fn typecheck_unsafe(
    env: &mut TypeEnvironment,
    namespace: &EnvironmentNamespace,
    inner: &HIRExpression,
    expected_type: Option<&THIRType>,
) -> CXResult<TypecheckResult> {
    env.push_unsafe();
    let inner_result = typecheck_expr(env, namespace, inner, expected_type);
    env.pop_unsafe();
    let inner_result = inner_result?;

    let adopting = inner_result.is_adopting();
    let inner_expr = inner_result.standard_ready_coerce(env, inner.token_range())?;

    let result = TypecheckResult::new(
        inner_expr._type.clone(),
        THIRExpressionKind::Unsafe {
            expression: Box::new(inner_expr),
        },
    );

    Ok(if adopting {
        result.with_adopting()
    } else {
        result
    })
}
