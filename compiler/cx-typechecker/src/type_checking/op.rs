use crate::{
    environment::TypeEnvironment,
    type_checking::{
        op::binop::calls::typecheck_callee_call, result::TypecheckResult,
        typechecker::typecheck_expr,
    },
};
use cx_hir::ast::expression::{HIRBinOp, HIRExprKind, HIRExpression};
use cx_log::CXResult;
use cx_log::catalogue::typecheck as catalogue;
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
                    &catalogue::INVALID_FORM,
                    ("non-function call".into(), "left-hand side of backward pipe operator".into())
                );
            };
            
            Some(typecheck_expr(env, namespace, &rewritten, expected_type)?)
        }
        HIRBinOp::Pipe => Some(env.in_argument_scope(|env| {
            typecheck_pipe(env, namespace, expr, lhs, rhs, expected_type)
        })?),

        _ => None,
    })
}

fn typecheck_pipe(
    env: &mut TypeEnvironment,
    namespace: &NamespacePath,
    expr: &HIRExpression,
    lhs: &HIRExpression,
    rhs: &HIRExpression,
    expected_type: Option<&THIRType>,
) -> CXResult<TypecheckResult> {
    let implicit_param = typecheck_expr(env, namespace, lhs, None)?
        .standard_ready_coerce(env, lhs.token_range())?;

    let HIRExprKind::BinOp {
        op: HIRBinOp::MethodCall,
        lhs: callee,
        rhs: arguments,
    } = &rhs.kind
    else {
        return env.log_error(
            expr.token_range(),
            &catalogue::INVALID_FORM,
            ("non-function call".into(), "right-hand side of pipe operator".into())
        );
    };

    let callee = typecheck_expr(env, namespace, callee, None)?;
    typecheck_callee_call(
        env,
        namespace,
        callee,
        vec![implicit_param],
        arguments,
        expr,
        expected_type,
    )
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
