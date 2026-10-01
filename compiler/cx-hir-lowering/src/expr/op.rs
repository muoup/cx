use cx_hir::ast::expression::{HIRBinOp, HIRExprKind, HIRExpression, HIRUnOp};
use cx_hmir::{
    HMIRAggregateOp, HMIRBinaryOp, HMIRBlockKind, HMIRCoerceMode, HMIRExprID, HMIRExprKind,
    HMIRNativeOp, HMIROwnershipOp, HMIRTypeOp, HMIRUnaryOp,
};
use cx_tokens::TokenRange;

use crate::body::BodyLowering;

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

impl BodyLowering<'_> {
    pub(super) fn lower_binop(
        &mut self,
        op: &HIRBinOp,
        lhs: &HIRExpression,
        rhs: &HIRExpression,
        span: &TokenRange,
    ) -> HMIRExprID {
        if let Some(op) = binary_op(op) {
            let lhs = self.lower_expr(lhs);
            let rhs = self.lower_expr(rhs);
            return self.native(HMIRNativeOp::BinOp { op, lhs, rhs }, span);
        }

        match op {
            HIRBinOp::Comma => {
                let statement = self.lower_expr(lhs);
                let tail = self.lower_expr(rhs);
                self.push(
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
                        None => return self.error(span),
                    },
                    None => None,
                };
                let target = self.lower_expr(lhs);
                let value = self.lower_expr(rhs);
                self.native(HMIRNativeOp::Assign { target, op, value }, span)
            }
            HIRBinOp::Access => {
                let HIRExprKind::Identifier { name, .. } = &rhs.kind else {
                    return self.error(span);
                };
                let base = self.lower_expr(lhs);
                self.aggregate_op(
                    HMIRAggregateOp::Member {
                        base,
                        name: name.name.clone(),
                    },
                    span,
                )
            }
            HIRBinOp::ArrayIndex => {
                let base = self.lower_expr(lhs);
                let index = self.lower_expr(rhs);
                self.aggregate_op(HMIRAggregateOp::Index { base, index }, span)
            }
            _ => self.error(span),
        }
    }

    pub(super) fn lower_unop(
        &mut self,
        op: &HIRUnOp,
        operand: &HIRExpression,
        span: &TokenRange,
    ) -> HMIRExprID {
        let value = self.lower_expr(operand);
        let unary = |this: &mut Self, op| this.native(HMIRNativeOp::UnOp { op, operand: value }, span);
        match op {
            HIRUnOp::Dereference => {
                let source = self.type_op(HMIRTypeOp::TypeOf(value), span);
                let source = self.type_op(HMIRTypeOp::Decay(source), span);
                let pointee = self.type_op(HMIRTypeOp::PointerInner(source), span);
                let target = self.type_op(HMIRTypeOp::Reference(pointee), span);
                self.coerce(HMIRCoerceMode::Implicit, value, target, span)
            }
            HIRUnOp::AddressOf => self.native(HMIRNativeOp::AddressOf(value), span),
            HIRUnOp::Negative => unary(self, HMIRUnaryOp::Neg),
            HIRUnOp::BNot => unary(self, HMIRUnaryOp::BNot),
            HIRUnOp::LNot => unary(self, HMIRUnaryOp::LNot),
            HIRUnOp::Move => self.ownership(HMIROwnershipOp::Move(value), span),
            HIRUnOp::ExplicitCast(ty) => {
                let target = self.lower_type(ty);
                self.coerce(HMIRCoerceMode::CCast, value, target, span)
            }
            HIRUnOp::Is(pattern) => {
                let pattern = self.lower_pattern(pattern, span);
                self.aggregate_op(HMIRAggregateOp::Is { value, pattern }, span)
            }
            HIRUnOp::PreIncrement(delta) if *delta < 0 => unary(self, HMIRUnaryOp::PreDecrement),
            HIRUnOp::PreIncrement(_) => unary(self, HMIRUnaryOp::PreIncrement),
            HIRUnOp::PostIncrement(delta) if *delta < 0 => {
                unary(self, HMIRUnaryOp::PostDecrement)
            }
            HIRUnOp::PostIncrement(_) => unary(self, HMIRUnaryOp::PostIncrement),
        }
    }

    pub(crate) fn coerce(
        &mut self,
        mode: HMIRCoerceMode,
        value: HMIRExprID,
        target: HMIRExprID,
        span: &TokenRange,
    ) -> HMIRExprID {
        self.native(
            HMIRNativeOp::Coerce {
                mode,
                value,
                target,
            },
            span,
        )
    }
}
