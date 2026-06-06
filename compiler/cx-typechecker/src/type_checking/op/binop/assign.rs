use cx_ast::ast::{
    expression::{CXBinOp, CXExpression},
    modifiers::CX_CONST,
};
use cx_mir::{
    mir::expression::{MIRExpression, MIRExpressionKind},
    type_context::MIRTypeContext,
};
use cx_util::CXResult;

use crate::{
    environment::{BindingMoveState, TypeEnvironment},
    log_typecheck_error,
    type_checking::{
        coercion::implicit::{implicit_cast, promotion::std_rval_promotion},
        op::typecheck_binop,
        result::{BindingPlaceKind, TypecheckResult},
        value::locals::{ensure_binding_available, mark_binding},
    },
};

pub fn typecheck_assignment(
    env: &mut TypeEnvironment,
    lhs: TypecheckResult,
    rhs: MIRExpression,
    op: Option<&CXBinOp>,
    expr: &CXExpression,
) -> CXResult<TypecheckResult> {
    let binding = lhs.binding().cloned();
    let lhs_expr = lhs.standard_ready_coerce(env, expr.token_range())?;
    let lhs_type = lhs_expr._type.clone();

    let Some(inner) = env.symbols.mem_ref_inner(&lhs_type).cloned() else {
        return log_typecheck_error!(
            env,
            expr.token_range(),
            "Cannot assign to non-reference type {}",
            lhs_type.display_with(&env.symbols)
        );
    };

    let mut rhs = implicit_cast(env, rhs, &inner)?;

    if let Some(op) = op {
        if let Some(binding) = binding.as_ref() {
            ensure_binding_available(env, lhs_expr.token_range.as_ref(), Some(&binding))?;
        }

        let loaded_lhs = std_rval_promotion(env, lhs_expr.clone())?;

        rhs = std_rval_promotion(env, rhs)
            .and_then(|v| typecheck_binop(env, op, loaded_lhs, v))
            .and_then(|v| v.standard_ready_coerce(env, expr.token_range()))?;
    } else if let Some(binding) = binding.as_ref()
        && binding.kind == BindingPlaceKind::Projection
        && env
            .function
            .tracked_binding(binding.root.as_str())
            .is_some_and(|tracked| tracked.state != crate::environment::BindingMoveState::Available)
    {
        return log_typecheck_error!(
            env,
            expr.token_range(),
            "Assignment to a field or projection of a moved aggregate binding is not implemented"
        );
    }

    if inner.get_specifier(CX_CONST) {
        return log_typecheck_error!(env, expr.token_range(), "Cannot assign to a const type");
    }

    rhs = implicit_cast(env, rhs, &inner)?;

    if let Some(binding) = binding.as_ref() {
        mark_binding(env, binding, BindingMoveState::Available);
    }

    Ok(TypecheckResult::new(
        lhs_type,
        MIRExpressionKind::RegionWrite {
            target: Box::new(lhs_expr),
            value: Box::new(rhs),
        },
    ))
}
