use crate::{
    environment::TypeEnvironment, type_checking::result::TypecheckResult,
    type_checking::typechecker::typecheck_expr,
};
use cx_ast::ast::expression::CXExpression;
use cx_mir::{EnvironmentNamespace, mir::data::MIRType};
use cx_util::CXResult;

pub(crate) fn typecheck_unsafe(
    env: &mut TypeEnvironment,
    namespace: &EnvironmentNamespace,
    inner: &CXExpression,
    expected_type: Option<&MIRType>,
) -> CXResult<TypecheckResult> {
    env.push_unsafe();
    let inner_result = typecheck_expr(env, namespace, inner, expected_type)?;
    env.pop_unsafe();

    let adopting = inner_result.is_adopting();
    let inner_expr = inner_result.standard_ready_coerce(env, inner.token_range())?;

    let result = TypecheckResult::from(inner_expr);

    Ok(if adopting {
        result.with_adopting()
    } else {
        result
    })
}
