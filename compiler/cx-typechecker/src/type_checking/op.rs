use crate::{
    environment::TypeEnvironment,
    type_checking::{
        op::binop::calls::typecheck_callee_call, result::TypecheckResult,
        typechecker::typecheck_expr,
    },
};
use cx_hir::ast::expression::{HIRBinOp, HIRExprKind, HIRExpression};
use cx_log::CXResult;
use cx_namespace::module::NamespacePath;
use cx_thir::thir::{data::THIRType, expression::THIRExpression};

pub use unop::typecheck_unop;

pub mod binop;
pub mod unop;

pub fn try_typecheck_special_binop(
    env: &mut TypeEnvironment,
    namespace: &NamespacePath,
    op: &HIRBinOp,
    expr: &HIRExpression,
    lhs: &HIRExpression,
    rhs: &HIRExpression,
    expected_type: Option<&THIRType>,
) -> CXResult<Option<TypecheckResult>> {
    Ok(match op {
        HIRBinOp::BackwardPipe => {
            let Some(rewritten) = append_call_argument(lhs, rhs, expr) else {
                return env.log_error(
                    expr.token_range(),
                    "The left side of '<|' must be a function call".to_string(),
                );
            };
            Some(typecheck_expr(env, namespace, &rewritten, expected_type)?)
        }
        HIRBinOp::Pipe => {
            let implicit_param = typecheck_expr(env, namespace, lhs, None)?
                .standard_ready_coerce(env, lhs.token_range())?;

            match &rhs.kind {
                HIRExprKind::BinOp {
                    op: HIRBinOp::MethodCall,
                    lhs,
                    rhs,
                } => {
                    let callee = typecheck_expr(env, namespace, lhs, None)?;

                    Some(typecheck_callee_call(
                        env,
                        namespace,
                        callee,
                        vec![implicit_param],
                        rhs,
                        expr,
                        expected_type,
                    )?)
                }

                _ => {
                    return env.log_error(
                        expr.token_range(),
                        "The right side of '|>' must be a method call".to_string(),
                    );
                }
            }
        }

        _ => None,
    })
}

fn append_call_argument(
    call: &HIRExpression,
    argument: &HIRExpression,
    whole_expr: &HIRExpression,
) -> Option<HIRExpression> {
    let kind = match &call.kind {
        HIRExprKind::BinOp {
            op: HIRBinOp::MethodCall,
            lhs,
            rhs,
        } => {
            let arguments = if matches!(rhs.kind, HIRExprKind::Void) {
                argument.clone()
            } else {
                HIRExpression {
                    kind: HIRExprKind::BinOp {
                        lhs: rhs.clone(),
                        rhs: Box::new(argument.clone()),
                        op: HIRBinOp::Comma,
                    },
                    range: whole_expr.range.clone(),
                }
            };
            HIRExprKind::BinOp {
                lhs: lhs.clone(),
                rhs: Box::new(arguments),
                op: HIRBinOp::MethodCall,
            }
        }
        HIRExprKind::BinOp {
            op: HIRBinOp::Pipe,
            lhs,
            rhs,
        } => {
            let appended = append_call_argument(rhs, argument, whole_expr)?;
            HIRExprKind::BinOp {
                lhs: lhs.clone(),
                rhs: Box::new(appended),
                op: HIRBinOp::Pipe,
            }
        }
        _ => return None,
    };

    Some(HIRExpression {
        kind,
        range: whole_expr.range.clone(),
    })
}

pub fn typecheck_binop(
    env: &mut TypeEnvironment,
    op: &HIRBinOp,
    lhs: THIRExpression,
    rhs: THIRExpression,
) -> CXResult<TypecheckResult> {
    binop::dispatch(env, op, lhs, rhs)
}
