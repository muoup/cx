use crate::{
    environment::TypeEnvironment,
    type_checking::{
        op::binop::calls::typecheck_callee_method_call, result::TypecheckResult,
        typechecker::typecheck_expr,
    },
};
use cx_ast::ast::expression::{CXBinOp, CXExprKind, CXExpression};
use cx_log::CXResult;
use cx_mir::{
    EnvironmentNamespace,
    mir::{data::MIRType, expression::MIRExpression},
};

pub use unop::typecheck_unop;

pub mod binop;
pub mod unop;

pub fn try_typecheck_special_binop(
    env: &mut TypeEnvironment,
    namespace: &EnvironmentNamespace,
    op: &CXBinOp,
    expr: &CXExpression,
    lhs: &CXExpression,
    rhs: &CXExpression,
    expected_type: Option<&MIRType>,
) -> CXResult<Option<TypecheckResult>> {
    Ok(match op {
        CXBinOp::Pipe => {
            let implicit_param = typecheck_expr(env, namespace, lhs, None)?
                .standard_ready_coerce(env, lhs.token_range())?;

            match &rhs.kind {
                CXExprKind::BinOp {
                    op: CXBinOp::MethodCall,
                    lhs,
                    rhs,
                } => {
                    let callee = typecheck_expr(env, namespace, lhs, None)?;

                    Some(typecheck_callee_method_call(
                        env,
                        namespace,
                        callee,
                        vec![implicit_param],
                        rhs,
                        expr,
                        expected_type,
                    )?)
                }

                _ => None,
            }
        }

        _ => None,
    })
}

pub fn typecheck_binop(
    env: &mut TypeEnvironment,
    op: &CXBinOp,
    lhs: MIRExpression,
    rhs: MIRExpression,
) -> CXResult<TypecheckResult> {
    binop::dispatch(env, op, lhs, rhs)
}
