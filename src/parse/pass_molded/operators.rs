use std::clone;
use crate::parse::pass_molded::{CXBinOp, CXExpr, CXUnOp};
use crate::parse::pass_molded::expr_molding::mold_expression;
use crate::parse::pass_unverified::{UVBinOp, UVExpr, UVUnOp};

pub(crate) fn mold_binop(left: CXExpr, right: CXExpr, op: &UVBinOp) -> Option<CXExpr> {

    match op {
        UVBinOp::Assignment(op) => {
            let op = op
                .as_ref()
                .map(|op| uv_cx_binop(op.as_ref().clone()));

            Some(
                CXExpr::Assignment {
                    lhs: Box::new(left),
                    rhs: Box::new(right),
                    op
                }
            )
        },

        _ => Some(
            CXExpr::BinOp {
                lhs: Box::new(left),
                rhs: Box::new(right),
                op: uv_cx_binop(op.clone())
            }
        )
    }
}

pub(crate) fn mold_unop(operand: &UVExpr, operator: &UVUnOp) -> Option<CXExpr> {
    Some(
        CXExpr::UnOp {
            operand: Box::new(mold_expression(operand)?),
            operator: uv_cx_unop(operator.clone())
        }
    )
}

pub(crate) fn uv_cx_binop(op: UVBinOp) -> CXBinOp {
    match op {
        UVBinOp::Add        => CXBinOp::Add,
        UVBinOp::Subtract   => CXBinOp::Subtract,
        UVBinOp::Multiply   => CXBinOp::Multiply,
        UVBinOp::Divide     => CXBinOp::Divide,
        UVBinOp::Modulus    => CXBinOp::Modulus,

        UVBinOp::Access     => CXBinOp::Access,

        _ => todo!("uv_cx_binop: {:?}", op)
    }
}

pub(crate) fn uv_cx_unop(op: UVUnOp) -> CXUnOp {
    match op {
        UVUnOp::Dereference     => CXUnOp::Dereference,
        UVUnOp::BNot            => CXUnOp::BNot,
        UVUnOp::LNot            => CXUnOp::LNot,
        UVUnOp::Negative        => CXUnOp::Negative,

        _ => todo!()
    }
}

pub(crate) fn binop_precedence(op: &UVBinOp) -> u8 {
    match op {
        // TODO: Proper operator precedence
        UVBinOp::Add | UVBinOp::Subtract => 1,
        UVBinOp::Multiply | UVBinOp::Divide | UVBinOp::Modulus => 2,

        UVBinOp::Access => 8,
        UVBinOp::Assignment(_) => 9,
        UVBinOp::Comma => 10,

        _ => todo!("binop_precedence({:?})", op)
    }
}
