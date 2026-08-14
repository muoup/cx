use crate::{
    environment::TypeEnvironment,
    type_checking::{
        op::binop::calls::typecheck_callee_method_call, result::TypecheckResult,
        typechecker::typecheck_expr,
    },
};
use cx_ast::ast::expression::{CXBinOp, CXExprKind, CXExpression};
use cx_log::CXResult;
use cx_thir::{
    EnvironmentNamespace,
    thir::{data::THIRType, expression::THIRExpression},
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
    expected_type: Option<&THIRType>,
) -> CXResult<Option<TypecheckResult>> {
    Ok(match op {
        CXBinOp::BackwardPipe => {
            let Some(rewritten) = append_call_argument(lhs, rhs, expr) else {
                return env.log_error(
                    expr.token_range(),
                    "The left side of '<|' must be a function call".to_string(),
                );
            };
            Some(typecheck_expr(env, namespace, &rewritten, expected_type)?)
        }
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

fn append_call_argument(
    call: &CXExpression,
    argument: &CXExpression,
    whole_expr: &CXExpression,
) -> Option<CXExpression> {
    let kind = match &call.kind {
        CXExprKind::BinOp {
            op: CXBinOp::MethodCall,
            lhs,
            rhs,
        } => {
            let arguments = if matches!(rhs.kind, CXExprKind::Void) {
                argument.clone()
            } else {
                CXExpression {
                    kind: CXExprKind::BinOp {
                        lhs: rhs.clone(),
                        rhs: Box::new(argument.clone()),
                        op: CXBinOp::Comma,
                    },
                    range: whole_expr.range.clone(),
                }
            };
            CXExprKind::BinOp {
                lhs: lhs.clone(),
                rhs: Box::new(arguments),
                op: CXBinOp::MethodCall,
            }
        }
        CXExprKind::BinOp {
            op: CXBinOp::Pipe,
            lhs,
            rhs,
        } => {
            let appended = append_call_argument(rhs, argument, whole_expr)?;
            CXExprKind::BinOp {
                lhs: lhs.clone(),
                rhs: Box::new(appended),
                op: CXBinOp::Pipe,
            }
        }
        _ => return None,
    };

    Some(CXExpression {
        kind,
        range: whole_expr.range.clone(),
    })
}

pub fn typecheck_binop(
    env: &mut TypeEnvironment,
    op: &CXBinOp,
    lhs: THIRExpression,
    rhs: THIRExpression,
) -> CXResult<TypecheckResult> {
    binop::dispatch(env, op, lhs, rhs)
}
