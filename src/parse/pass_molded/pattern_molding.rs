use crate::log_error;
use crate::parse::pass_molded::{CXBinOp, CXExpr};
use crate::parse::pass_molded::expr_molding::{mold_expr_stack, mold_expression, mold_pseudo_expr};
use crate::parse::pass_unverified::{UVBinOp, UVExpr};

#[derive(Debug, Clone)]
pub(crate) enum PseudoUVExpr<'a> {
    ID(&'a UVExpr),
    BinOp {
        left: Box<PseudoUVExpr<'a>>,
        right: Box<PseudoUVExpr<'a>>,
        op: UVBinOp
    }
}

pub(crate) fn mold_delimited(expr: &UVExpr, delimited: UVBinOp) -> Option<Vec<PseudoUVExpr>> {
    match expr {
        // If there is no operators, we can assume there is only one expression
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

            // TODO: This is the wrong heuristic, fix this
            while expr_iter.len() > 0 {
                let next_delim =
                    op_iter
                        .clone()
                        .position(|op| *op == delimited)
                        .unwrap_or(op_iter.len());

                let exprs_vec =
                    expr_iter.by_ref()
                        .take(next_delim + 1)
                        .collect::<Vec<_>>();
                let ops_vec =
                    op_iter.by_ref()
                        .take(next_delim) // Grab up to the next delimiter
                        .collect::<Vec<_>>();
                op_iter.next(); // Skip the delimiter

                if exprs_vec.is_empty() {
                    log_error!("Extraneous operator in expression stack: {}", expr);
                }

                let Some(molded_expr) = mold_expr_stack(
                    exprs_vec.as_slice(),
                    ops_vec.as_slice()
                ) else {
                    log_error!("Error molding expression sub-stack: {:?} | {:?}", exprs_vec, ops_vec);
                };

                exprs.push(molded_expr);
            }

            if expr_iter.len() == 1 {
                let last_expr = expr_iter.next().unwrap();
                exprs.push(PseudoUVExpr::ID(last_expr));
            } else if expr_iter.len() > 1 {
                log_error!("Extraneous expressions in stack: {}", expr);
            }

            Some(exprs)
        },

        _ => Some(vec![PseudoUVExpr::ID(expr)])
    }
}