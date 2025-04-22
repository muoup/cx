use crate::log_error;
use crate::parse::pass_unverified::{UVBinOp, UVExpr};

pub(crate) enum PseudoUVExpr<'a> {
    ID(&'a UVExpr),
    BinOp {
        left: Box<PseudoUVExpr<'a>>,
        right: Box<PseudoUVExpr<'a>>,
        op: UVBinOp
    }
}

pub(crate) fn mold_delimited<'a>(expr: &'a UVExpr, delimited: &'a UVBinOp) -> Option<Vec<PseudoUVExpr<'a>>> {
    match expr {
        UVExpr::Compound { .. } => {
            Some(vec![PseudoUVExpr::ID(expr)])
        },

        UVExpr::Complex { op_stack, expr_stack } => {
            if op_stack.is_empty() && expr_stack.is_empty() {
                return Some(vec![PseudoUVExpr::ID(expr)]);
            }

            let mut op_iter = op_stack.iter();
            let mut expr_iter = expr_stack.iter();

            let mut exprs = Vec::new();

            while op_iter.len() > 0 {
                let next_delim =
                    op_iter
                        .position(|op| op == delimited)
                        .unwrap_or(op_iter.len() - 1);

                let exprs_vec =
                    expr_iter.by_ref().take(next_delim + 1)
                        .collect::<Vec<_>>();
                let ops_vec =
                    op_iter.by_ref().take(next_delim)
                        .collect::<Vec<_>>();

                let expr = mold_expr_stack(
                    exprs_vec.as_slice(),
                    ops_vec.as_slice()
                );

                exprs.push(expr);
            }

            if expr_iter.len() > 0 {
                log_error!("Error parsing expression stack: {}", expr);
            }

            None
        },

        _ => log_error!("Error parsing expression stack: {}", expr)
    }
}

pub(crate) fn mold_expr_stack<'a>(exprs: &[&'a UVExpr], ops: &[&'a UVBinOp]) -> Option<PseudoUVExpr<'a>> {
    todo!()
}