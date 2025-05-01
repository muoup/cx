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
    },
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

            let mut op_iter = 0usize;
            let mut expr_iter = 0usize;

            let mut exprs = Vec::new();

            // TODO: This is the wrong heuristic, fix this
            while expr_iter != expr_stack.len() {
                let next_delim =
                    op_stack
                        .iter()
                        .skip(op_iter)
                        .position(|op| *op == delimited)
                        .unwrap_or(op_stack.len());

                let exprs_slice = &expr_stack[expr_iter..next_delim + 1];
                let ops_slice = &op_stack[op_iter..next_delim];

                op_iter = next_delim + 1;
                expr_iter = next_delim + 1;

                if exprs_slice.is_empty() {
                    log_error!("Extraneous operator in expression stack: {}", expr);
                }

                let Some(molded_expr) = mold_expr_stack(
                    exprs_slice,
                    ops_slice
                ) else {
                    log_error!("Error molding expression sub-stack: {:?} | {:?}", exprs_slice, ops_slice);
                };

                exprs.push(molded_expr);
            }

            if expr_stack.len() - expr_iter == 1 {
                exprs.push(PseudoUVExpr::ID(&expr_stack[expr_iter]));
            } else if expr_stack.len() - expr_iter > 1 {
                log_error!("Extraneous expressions in stack: {}", expr);
            }

            Some(exprs)
        },

        _ => Some(vec![PseudoUVExpr::ID(expr)])
    }
}