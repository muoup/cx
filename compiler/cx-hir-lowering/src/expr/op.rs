use cx_hir::ast::expression::{HIRBinOp, HIRExprKind, HIRExpression, HIRUnOp};
use cx_hmir::{
    HMIRAggregateOp, HMIRBinaryOp, HMIRCoerceMode, HMIROp, HMIROwnershipOp, HMIRUnaryOp,
    expr::HMIRExprID,
};
use cx_log::{
    CXResult,
    catalogue::{mir, typecheck},
};
use cx_tokens::TokenRange;

use crate::{
    body::BodyLowering,
    expr::{lower_expr, pattern::lower_pattern},
    log::log_hir_error,
    ty::lower_type,
};

fn binary_op(op: &HIRBinOp) -> Option<HMIRBinaryOp> {
    Some(match op {
        HIRBinOp::Add => HMIRBinaryOp::Add,
        HIRBinOp::Subtract => HMIRBinaryOp::Sub,
        HIRBinOp::Multiply => HMIRBinaryOp::Mul,
        HIRBinOp::Divide => HMIRBinaryOp::Div,
        HIRBinOp::Modulus => HMIRBinaryOp::Mod,
        HIRBinOp::Less => HMIRBinaryOp::Lt,
        HIRBinOp::Greater => HMIRBinaryOp::Gt,
        HIRBinOp::LessEqual => HMIRBinaryOp::Le,
        HIRBinOp::GreaterEqual => HMIRBinaryOp::Ge,
        HIRBinOp::Equal => HMIRBinaryOp::Eq,
        HIRBinOp::NotEqual => HMIRBinaryOp::Neq,
        HIRBinOp::LAnd => HMIRBinaryOp::LAnd,
        HIRBinOp::LOr => HMIRBinaryOp::LOr,
        HIRBinOp::BitAnd => HMIRBinaryOp::BAnd,
        HIRBinOp::BitOr => HMIRBinaryOp::BOr,
        HIRBinOp::BitXor => HMIRBinaryOp::BXor,
        HIRBinOp::LShift => HMIRBinaryOp::LShift,
        HIRBinOp::RShift => HMIRBinaryOp::RShift,
        _ => return None,
    })
}

pub(crate) fn lower_binop(
    context: &mut BodyLowering<'_>,
    op: &HIRBinOp,
    lhs: &HIRExpression,
    rhs: &HIRExpression,
    span: &TokenRange,
) -> CXResult<HMIRExprID> {
    if let Some(op) = binary_op(op) {
        let lhs = lower_expr(context, lhs)?;
        let rhs = lower_expr(context, rhs)?;
        return Ok(context.op(HMIROp::BinOp { op, lhs, rhs }, span));
    }

    match op {
        // The value of an unscoped block is that of its last statement
        HIRBinOp::Comma | HIRBinOp::GroupedComma => {
            let statement = lower_expr(context, lhs)?;
            let value = lower_expr(context, rhs)?;
            Ok(context.block(vec![statement, value], false, false, span))
        }
        HIRBinOp::Assign(compound) => {
            let op = match compound.as_deref().map(binary_op) {
                Some(None) => {
                    return log_hir_error(
                        span,
                        &mir::MALFORMED_HIR,
                        "compound assignment without a binary operator".into(),
                    );
                }
                Some(op) => op,
                None => None,
            };
            let target = lower_expr(context, lhs)?;
            let value = lower_expr(context, rhs)?;
            Ok(context.op(HMIROp::Assign { target, op, value }, span))
        }
        HIRBinOp::Access => {
            let HIRExprKind::Identifier { name, .. } = &rhs.kind else {
                return log_hir_error(
                    span,
                    &typecheck::TYPE_REQUIREMENT,
                    ("member access".into(), "a member name".into(), None),
                );
            };
            let base = lower_expr(context, lhs)?;
            let name = name.name.clone();
            Ok(context.aggregate_op(HMIRAggregateOp::Member { base, name }, span))
        }
        HIRBinOp::ArrayIndex => {
            let base = lower_expr(context, lhs)?;
            let index = lower_expr(context, rhs)?;
            Ok(context.aggregate_op(HMIRAggregateOp::Index { base, index }, span))
        }
        _ => log_hir_error(
            span,
            &mir::MALFORMED_HIR,
            "call operator outside of a call".into(),
        ),
    }
}

pub(crate) fn lower_unop(
    context: &mut BodyLowering<'_>,
    op: &HIRUnOp,
    operand: &HIRExpression,
    span: &TokenRange,
) -> CXResult<HMIRExprID> {
    let value = lower_expr(context, operand)?;
    let unary =
        |this: &mut BodyLowering<'_>, op| this.op(HMIROp::UnOp { op, operand: value }, span);

    Ok(match op {
        HIRUnOp::Dereference => context.op(HMIROp::Dereference(value), span),
        HIRUnOp::AddressOf => context.op(HMIROp::AddressOf(value), span),
        HIRUnOp::Negative => unary(context, HMIRUnaryOp::Neg),
        HIRUnOp::BNot => unary(context, HMIRUnaryOp::BNot),
        HIRUnOp::LNot => unary(context, HMIRUnaryOp::LNot),
        HIRUnOp::Move => context.ownership(HMIROwnershipOp::Move(value), span),
        HIRUnOp::ExplicitCast(ty) => {
            let target = lower_type(context, ty)?;
            context.coerce(HMIRCoerceMode::CCast, value, target, span)
        }
        HIRUnOp::Is(pattern) => {
            let pattern = lower_pattern(context, pattern, span)?;
            context.aggregate_op(HMIRAggregateOp::Is { value, pattern }, span)
        }
        HIRUnOp::PreIncrement(delta) if *delta < 0 => unary(context, HMIRUnaryOp::PreDecrement),
        HIRUnOp::PreIncrement(_) => unary(context, HMIRUnaryOp::PreIncrement),
        HIRUnOp::PostIncrement(delta) if *delta < 0 => unary(context, HMIRUnaryOp::PostDecrement),
        HIRUnOp::PostIncrement(_) => unary(context, HMIRUnaryOp::PostIncrement),
    })
}
