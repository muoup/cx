use cx_ast::ast::{expression::CXBinOp, modifiers::CX_CONST};
use cx_mir::mir::expression::{MIRExpression, MIRExpressionKind};
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
) -> CXResult<TypecheckResult> {
    let binding = lhs.binding().cloned();
    let lhs = lhs.into_expression()?;
    let lhs_type = lhs.get_type();

    let Some(inner) = env.symbols.context.mem_ref_inner(&lhs_type).cloned() else {
        return log_typecheck_error!(
            env,
            lhs.token_range.as_ref(),
            "Cannot assign to non-reference type {}",
            lhs_type.display_with(&env.symbols.context)
        );
    };

    let mut rhs = std_rval_promotion(env, rhs)?;

    if let Some(op) = op {
        if let Some(binding) = binding.as_ref() {
            ensure_binding_available(env, lhs.token_range.clone(), &binding.root)?;
        }

        let loaded_lhs = std_rval_promotion(env, lhs.clone())?;

        rhs = typecheck_binop(env, op, loaded_lhs, rhs)?.into_expression()?;
    } else if let Some(binding) = binding.as_ref()
        && binding.kind == BindingPlaceKind::Projection
        && env
            .function
            .tracked_binding(binding.root.as_str())
            .is_some_and(|tracked| tracked.state != crate::environment::BindingMoveState::Available)
    {
        return log_typecheck_error!(
            env,
            lhs.token_range.as_ref(),
            "Assignment to a field or projection of a moved aggregate binding is not implemented"
        );
    }

    if inner.get_specifier(CX_CONST) {
        return log_typecheck_error!(
            env,
            lhs.token_range.as_ref(),
            "Cannot assign to a const type"
        );
    }

    rhs = implicit_cast(env, rhs, &inner)?;

    if let Some(binding) = binding.as_ref() {
        mark_binding(env, binding, BindingMoveState::Available);
    }

    Ok(TypecheckResult::new_base(
        lhs_type,
        MIRExpressionKind::RegionWrite {
            target: Box::new(lhs),
            value: Box::new(rhs),
        },
    ))
}
