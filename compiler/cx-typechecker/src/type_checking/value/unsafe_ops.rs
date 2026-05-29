use crate::{
    environment::TypeEnvironment, type_checking::result::TypecheckResult,
    type_checking::typechecker::typecheck_expr,
};
use cx_ast::ast::CXExpression;
use cx_mir::mir::{data::MIRType, expression::MIRExpressionKind, program::EnvironmentNamespace};
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
    let result = TypecheckResult::new_base(
        inner_result.get_type()?,
        MIRExpressionKind::Unsafe {
            expression: Box::new(inner_result.into_expression()?),
        },
    );

    Ok(if adopting {
        result.with_adopting()
    } else {
        result
    })
}
