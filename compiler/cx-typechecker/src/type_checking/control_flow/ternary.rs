use crate::environment::TypeEnvironment;
use crate::type_checking::coercion::implicit::{implicit_cast, promotion::std_rval_promotion};
use crate::type_checking::result::TypecheckResult;
use crate::type_checking::typechecker::typecheck_expr;
use cx_hir::ast::expression::HIRExpression;
use cx_log::CXResult;
use cx_namespace::module::NamespacePath;
use cx_thir::thir::{
    data::{THIRType, THIRTypeKind},
    expression::{THIRExpression, THIRExpressionKind},
};
use cx_thir::type_context::THIRTypeContext;
use cx_tokens::TokenRange;

pub fn typecheck_ternary(
    env: &mut TypeEnvironment,
    namespace: &NamespacePath,
    expr: &HIRExpression,
    condition: &HIRExpression,
    then_branch: &HIRExpression,
    else_branch: &HIRExpression,
    expected_type: Option<&THIRType>,
) -> CXResult<TypecheckResult> {
    let condition = typecheck_expr(env, namespace, condition, None)
        .and_then(|v| v.standard_ready_coerce(env, expr.token_range()))
        .and_then(|v| std_rval_promotion(env, v))
        .and_then(|v| implicit_cast(env, v, &THIRType::bool()))?;
    let then_result = typecheck_expr(env, namespace, then_branch, expected_type)
        .and_then(|v| v.standard_ready_coerce(env, expr.token_range()))
        .and_then(|v| std_rval_promotion(env, v))?;
    let else_expected = (!then_result.ty.is_unreachable())
        .then_some(&then_result.ty)
        .or(expected_type);
    let else_result = typecheck_expr(env, namespace, else_branch, else_expected)
        .and_then(|v| v.standard_ready_coerce(env, expr.token_range()))
        .and_then(|v| std_rval_promotion(env, v))?;

    let result_type = common_type(env, &then_result.ty, &else_result.ty);
    let (then_branch, else_branch) = if result_type.is_void() {
        (then_result, else_result)
    } else {
        (
            yield_as(env, then_result, &result_type)?,
            yield_as(env, else_result, &result_type)?,
        )
    };

    Ok(TypecheckResult::from(THIRExpression {
        token_range: TokenRange::internal(),
        kind: THIRExpressionKind::If {
            condition: Box::new(condition),
            then_branch: Box::new(then_branch),
            else_branch: Some(Box::new(else_branch)),
        },
        ty: result_type,
    }))
}

fn yield_as(
    env: &mut TypeEnvironment,
    value: THIRExpression,
    ty: &THIRType,
) -> CXResult<THIRExpression> {
    let value = implicit_cast(env, value, ty)?;
    Ok(THIRExpression {
        ty: THIRType::unit(),
        kind: THIRExpressionKind::Yield {
            value: Some(Box::new(value)),
        },
        token_range: TokenRange::internal(),
    })
}

/// The type both arms of a conditional expression are converted to.
fn common_type(env: &mut TypeEnvironment, then_type: &THIRType, else_type: &THIRType) -> THIRType {
    if then_type.is_unreachable() {
        return else_type.clone();
    }
    if else_type.is_unreachable() {
        return then_type.clone();
    }
    if then_type.is_void() || else_type.is_void() {
        return THIRType::unit();
    }

    match (&then_type.kind, &else_type.kind) {
        (THIRTypeKind::Float { ty: then_float }, THIRTypeKind::Float { ty: else_float })
            if else_float.bytes() > then_float.bytes() =>
        {
            else_type.clone()
        }
        (THIRTypeKind::Integer { .. }, THIRTypeKind::Float { .. }) => else_type.clone(),
        (
            THIRTypeKind::Integer {
                ty: then_int,
                signed: then_signed,
            },
            THIRTypeKind::Integer {
                ty: else_int,
                signed: else_signed,
            },
        ) if else_int.rank() > then_int.rank()
            || (else_int.rank() == then_int.rank() && *then_signed && !*else_signed) =>
        {
            else_type.clone()
        }
        // A null pointer constant takes the type of the other arm.
        (THIRTypeKind::Integer { .. }, THIRTypeKind::PointerTo { .. }) => else_type.clone(),
        (
            THIRTypeKind::PointerTo {
                inner_type: then_inner,
            },
            THIRTypeKind::PointerTo {
                inner_type: else_inner,
            },
        ) => {
            let then_inner = env.symbols.resolve_type_id(*then_inner).clone();
            let else_inner = env.symbols.resolve_type_id(*else_inner).clone();
            let specifiers = then_inner.specifiers | else_inner.specifiers;
            let pointee = if else_inner.is_void() {
                else_inner
            } else {
                then_inner
            };
            env.symbols.pointer_to(pointee.add_specifier(specifiers))
        }
        _ => then_type.clone(),
    }
}
