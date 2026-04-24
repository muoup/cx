use cx_ast::{ast::CXBinOp, data::CX_CONST};
use cx_mir::mir::expression::{MIRExpression, MIRExpressionKind};
use cx_util::CXResult;

use crate::{
    environment::TypeEnvironment,
    log_typecheck_error,
    type_checking::{
        coercion::implicit::{implicit_cast, promotion::std_rval_promotion},
        op::typecheck_binop,
        result::TypecheckResult,
    },
};

pub fn typecheck_assignment(
    env: &mut TypeEnvironment,
    lhs: MIRExpression,
    rhs: MIRExpression,
    op: Option<&CXBinOp>,
) -> CXResult<TypecheckResult> {
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
        let loaded_lhs = std_rval_promotion(env, lhs.clone())?;

        rhs = typecheck_binop(env, op, loaded_lhs, rhs)?.into_expression();
    }

    if inner.get_specifier(CX_CONST) {
        return log_typecheck_error!(
            env,
            lhs.token_range.as_ref(),
            "Cannot assign to a const type"
        );
    }

    rhs = implicit_cast(env, rhs, &inner)?;
    
    Ok(TypecheckResult::new_base(
        lhs_type,
        MIRExpressionKind::MemoryWrite {
            target: Box::new(lhs),
            value: Box::new(rhs),
        },
    ))
}
