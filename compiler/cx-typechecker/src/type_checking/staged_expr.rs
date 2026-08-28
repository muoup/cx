use cx_hir::ast::expression::HIRExpression;
use cx_log::CXResult;
use cx_thir::{
    EnvironmentNamespace,
    thir::{
        comptime::{THIRStagedExpr, THIRStagedParameter},
        data::{THIRComptimeValueType, THIRType},
        expression::{THIRExpression, THIRExpressionKind, THIRLocalID},
    },
};
use cx_tokens::TokenRange;
use cx_util::namespace::QualifiedName;

use crate::{
    environment::TypeEnvironment,
    type_checking::{
        coercion::implicit::implicit_cast,
        result::{DeferredStagedExpr, TypecheckResult},
        typechecker::typecheck_expr,
    },
};

pub fn typecheck_staged_expr(
    env: &mut TypeEnvironment,
    namespace: &EnvironmentNamespace,
    inner: &HIRExpression,
    expected_type: Option<&THIRType>,
) -> CXResult<TypecheckResult> {
    let (body, effects) = env.in_runtime_emit(|env| {
        env.in_staged(|env| {
            let result = typecheck_expr(env, namespace, inner, expected_type)?;
            let result = if let Some(expected_type) = expected_type {
                result.apply_expected_type(env, namespace, expected_type)?
            } else {
                result
            };
            result.standard_ready_coerce(env, inner.token_range())
        })
    })?;

    let mut staged = THIRStagedExpr::new(Box::new(body));
    staged.set_effects(env.staged_effects(&effects));
    Ok(TypecheckResult::staged_literal(staged))
}

pub fn complete_staged_expr(
    env: &mut TypeEnvironment,
    namespace: &EnvironmentNamespace,
    deferred: DeferredStagedExpr,
    value_type: &THIRComptimeValueType,
) -> CXResult<THIRStagedExpr> {
    if deferred.params.len() != value_type.params.len() {
        return env.log_error(
            deferred.body.token_range(),
            format!(
                "Staged expression expects {} parameters, found {}",
                value_type.params.len(),
                deferred.params.len()
            ),
        );
    }

    env.symbols.push_local_scope();

    let mut params = Vec::with_capacity(deferred.params.len());
    for (name, ty) in deferred.params.into_iter().zip(&value_type.params) {
        let local_id = THIRLocalID::fresh();
        env.symbols.insert_local_value(
            QualifiedName::new_raw(name.clone()),
            THIRExpression {
                token_range: TokenRange::internal(),
                kind: THIRExpressionKind::Variable {
                    name: name.clone(),
                    local_id,
                },
                _type: ty.clone(),
            },
        );
        params.push(THIRStagedParameter {
            name,
            local_id,
            ty: ty.clone(),
        });
    }

    let body = env.in_staged(|env| {
        let body = typecheck_expr(env, namespace, &deferred.body, Some(&value_type._type))?
            .apply_expected_type(env, namespace, &value_type._type)?
            .standard_ready_coerce(env, deferred.body.token_range())?;
        implicit_cast(env, body, &value_type._type)
    });
    env.symbols.pop_local_scope();

    let (body, effects) = body?;
    let mut staged = THIRStagedExpr::new(Box::new(body));
    staged.add_params(params);
    staged.set_effects(env.staged_effects(&effects));
    Ok(staged)
}

pub fn into_expression(staged: THIRStagedExpr) -> THIRExpression {
    THIRExpression {
        _type: staged.expr()._type.clone(),
        token_range: staged.expr().token_range.clone(),
        kind: THIRExpressionKind::StagedExpression(staged),
    }
}
