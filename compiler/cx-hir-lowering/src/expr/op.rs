use cx_hir::ast::expression::{HIRBinOp, HIRExprKind, HIRExpression, HIRUnOp};
use cx_hmir::{
    HMIRAggregateOp, HMIRBinaryOp, HMIRBlockKind, HMIRCoerceMode, HMIRExprID, HMIRExprKind, HMIROp,
    HMIROwnershipOp, HMIRUnaryOp,
};
use cx_log::catalogue::{mir, typecheck};
use cx_tokens::TokenRange;

use crate::{
    body::BodyLowering,
    expr::{lower_expr, pattern::lower_pattern},
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
    cx: &mut BodyLowering<'_>,
    op: &HIRBinOp,
    lhs: &HIRExpression,
    rhs: &HIRExpression,
    span: &TokenRange,
) -> HMIRExprID {
    if let Some(op) = binary_op(op) {
        let lhs = lower_expr(cx, lhs);
        let rhs = lower_expr(cx, rhs);
        return cx.native(HMIROp::BinOp { op, lhs, rhs }, span);
    }

    match op {
        HIRBinOp::Comma | HIRBinOp::GroupedComma => {
            let statement = lower_expr(cx, lhs);
            let tail = lower_expr(cx, rhs);
            cx.push(
                HMIRExprKind::Block {
                    kind: HMIRBlockKind::Sequence,
                    statements: vec![statement],
                    tail: Some(tail),
                },
                span,
            )
        }
        HIRBinOp::Assign(compound) => {
            let op = match compound.as_deref() {
                Some(compound) => match binary_op(compound) {
                    Some(op) => Some(op),
                    None => {
                        return cx.error(
                            span,
                            &mir::MALFORMED_HIR,
                            "compound assignment without a binary operator".into(),
                        );
                    }
                },
                None => None,
            };
            let target = lower_expr(cx, lhs);
            let value = lower_expr(cx, rhs);
            cx.native(HMIROp::Assign { target, op, value }, span)
        }
        HIRBinOp::Access => {
            let HIRExprKind::Identifier { name, .. } = &rhs.kind else {
                return cx.error(
                    span,
                    &typecheck::TYPE_REQUIREMENT,
                    ("member access".into(), "a member name".into(), None),
                );
            };
            let base = lower_expr(cx, lhs);
            cx.aggregate_op(
                HMIRAggregateOp::Member {
                    base,
                    name: name.name.clone(),
                },
                span,
            )
        }
        HIRBinOp::ArrayIndex => {
            let base = lower_expr(cx, lhs);
            let index = lower_expr(cx, rhs);
            cx.aggregate_op(HMIRAggregateOp::Index { base, index }, span)
        }
        _ => cx.error(
            span,
            &mir::MALFORMED_HIR,
            "call operator outside of a call".into(),
        ),
    }
}

pub(crate) fn lower_unop(
    cx: &mut BodyLowering<'_>,
    op: &HIRUnOp,
    operand: &HIRExpression,
    span: &TokenRange,
) -> HMIRExprID {
    let value = lower_expr(cx, operand);
    let unary =
        |this: &mut BodyLowering<'_>, op| this.native(HMIROp::UnOp { op, operand: value }, span);
    match op {
        HIRUnOp::Dereference => cx.native(HMIROp::Dereference(value), span),
        HIRUnOp::AddressOf => cx.native(HMIROp::AddressOf(value), span),
        HIRUnOp::Negative => unary(cx, HMIRUnaryOp::Neg),
        HIRUnOp::BNot => unary(cx, HMIRUnaryOp::BNot),
        HIRUnOp::LNot => unary(cx, HMIRUnaryOp::LNot),
        HIRUnOp::Move => cx.ownership(HMIROwnershipOp::Move(value), span),
        HIRUnOp::ExplicitCast(ty) => {
            let target = lower_type(cx, ty);
            cx.coerce(HMIRCoerceMode::CCast, value, target, span)
        }
        HIRUnOp::Is(pattern) => {
            let pattern = lower_pattern(cx, pattern, span);
            cx.aggregate_op(HMIRAggregateOp::Is { value, pattern }, span)
        }
        HIRUnOp::PreIncrement(delta) if *delta < 0 => unary(cx, HMIRUnaryOp::PreDecrement),
        HIRUnOp::PreIncrement(_) => unary(cx, HMIRUnaryOp::PreIncrement),
        HIRUnOp::PostIncrement(delta) if *delta < 0 => unary(cx, HMIRUnaryOp::PostDecrement),
        HIRUnOp::PostIncrement(_) => unary(cx, HMIRUnaryOp::PostIncrement),
    }
}
