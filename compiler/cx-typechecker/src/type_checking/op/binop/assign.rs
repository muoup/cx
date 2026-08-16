use cx_hir::ast::{
    expression::{HIRBinOp, HIRExpression},
    modifiers::HIR_CONST,
};
use cx_log::CXResult;
use cx_thir::{
    thir::expression::{THIRExpression, THIRExpressionKind},
    type_context::THIRTypeContext,
};

use crate::{
    environment::TypeEnvironment,
    type_checking::{
        coercion::implicit::{implicit_cast, promotion::std_rval_promotion},
        op::typecheck_binop,
        result::TypecheckResult,
    },
};

pub fn typecheck_assignment(
    env: &mut TypeEnvironment,
    lhs: TypecheckResult,
    rhs: THIRExpression,
    op: Option<&HIRBinOp>,
    expr: &HIRExpression,
) -> CXResult<TypecheckResult> {
    let lhs_expr = lhs.standard_ready_coerce(env, expr.token_range())?;
    let lhs_type = lhs_expr._type.clone();

    let Some(inner) = env.symbols.mem_ref_inner(&lhs_type).cloned() else {
        return env.log_error(
            expr.token_range(),
            format!(
                "Cannot assign to non-reference type {}",
                lhs_type.display_with(&env.symbols)
            ),
        );
    };

    let mut rhs = if op.is_some() {
        rhs
    } else {
        implicit_cast(env, rhs, &inner)?
    };

    if let Some(op) = op {
        let loaded_lhs = std_rval_promotion(env, lhs_expr.clone())?;

        rhs = std_rval_promotion(env, rhs)
            .and_then(|v| typecheck_binop(env, op, loaded_lhs, v))
            .and_then(|v| v.standard_ready_coerce(env, expr.token_range()))?;
    }

    if inner.get_specifier(HIR_CONST) {
        return env.log_error(
            expr.token_range(),
            "Cannot assign to a const type".to_string(),
        );
    }

    rhs = implicit_cast(env, rhs, &inner)?;

    Ok(TypecheckResult::new(
        lhs_type,
        THIRExpressionKind::Assign {
            target: Box::new(lhs_expr),
            value: Box::new(rhs),
        },
    ))
}
