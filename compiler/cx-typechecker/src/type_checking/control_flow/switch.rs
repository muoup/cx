use crate::environment::TypeEnvironment;
use crate::type_checking::coercion::implicit::{implicit_cast, promotion::std_rval_promotion};
use crate::type_checking::result::TypecheckResult;
use crate::type_checking::typechecker::typecheck_expr;
use cx_hir::ast::expression::HIRExpression;
use cx_log::CXResult;
use cx_log::catalogue::typecheck as catalogue;
use cx_namespace::module::NamespacePath;
use cx_thir::thir::{
    data::{THIRType, THIRTypeKind},
    expression::THIRExpressionKind,
};

pub fn typecheck_switch(
    env: &mut TypeEnvironment,
    namespace: &NamespacePath,
    expr: &HIRExpression,
    condition: &HIRExpression,
    body: &HIRExpression,
) -> CXResult<TypecheckResult> {
    let condition_value = typecheck_expr(env, namespace, condition, None)
        .and_then(|v| v.standard_ready_coerce(env, condition.token_range()))
        .and_then(|v| std_rval_promotion(env, v))?;

    let THIRTypeKind::Integer { .. } = condition_value.ty.kind else {
        return env.log_error(
            &condition_value.token_range,
            &catalogue::TYPE_MISMATCH,
            (
                "switch condition".into(),
                "integer type".into(),
                format!("{}", condition_value.display_with(&env.symbols)),
            ),
        );
    };

    env.push_scope(true, false, expr.token_range().clone());
    env.function
        .flow_mut()
        .enter_switch(condition_value.ty.clone());

    let body = typecheck_expr(env, namespace, body, None)
        .and_then(|v| v.standard_ready_coerce(env, body.token_range()))?;

    env.pop_scope()
        .map_err(|err| env.complete_err(err, condition.token_range()))?;

    Ok(TypecheckResult::new(
        THIRType::unit(),
        THIRExpressionKind::CSwitch {
            condition: Box::new(condition_value),
            body: Box::new(body),
        },
    ))
}

pub fn typecheck_case(
    env: &mut TypeEnvironment,
    namespace: &NamespacePath,
    expr: &HIRExpression,
    value: Option<&HIRExpression>,
    statement: &HIRExpression,
) -> CXResult<TypecheckResult> {
    let Some(condition_type) = env.function.flow_mut().switch_condition_type() else {
        return env.log_error(
            expr.token_range(),
            &catalogue::REQUIRED_CONTEXT,
            ("case label".into(), "an enclosing switch statement".into()),
        );
    };

    let value = match value {
        Some(value) => Some(Box::new(
            typecheck_expr(env, namespace, value, None)
                .and_then(|v| v.standard_ready_coerce(env, value.token_range()))
                .and_then(|v| std_rval_promotion(env, v))
                .and_then(|v| implicit_cast(env, v, &condition_type))?,
        )),
        None if env.function.flow_mut().declare_switch_default() => None,
        None => {
            return env.log_error(
                expr.token_range(),
                &catalogue::DUPLICATE_ITEM,
                ("default label".into(), "switch statement".into()),
            );
        }
    };

    let statement = typecheck_expr(env, namespace, statement, None)
        .and_then(|v| v.standard_ready_coerce(env, statement.token_range()))?;

    Ok(TypecheckResult::new(
        THIRType::unit(),
        THIRExpressionKind::Case {
            value,
            statement: Box::new(statement),
        },
    ))
}
