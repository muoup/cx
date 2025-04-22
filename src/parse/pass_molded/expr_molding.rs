use crate::log_error;
use crate::parse::ast::ValueType;
use crate::parse::pass_molded::{CXExpr, CXAST};
use crate::parse::pass_unverified::{UVBinOp, UVExpr};

pub(crate) fn split_initialization(expr: &UVExpr) -> Option<(ValueType, &UVExpr)> {
    match expr {
        UVExpr::Compound { left, right } => {
            Some((
                mold_type(left.as_ref())?,
                right.as_ref()
            ))
        },

        UVExpr::Complex { expr_stack: expression_stack, op_stack: operator_stack } => {
            if operator_stack.len() != 1 || expression_stack.len() != 2 {
                log_error!("Error parsing {} as initialization header.", expr)
            }

            match operator_stack.first().unwrap() {
                UVBinOp::Multiply => {
                    let lhs = &expression_stack[1];
                    let rhs = &expression_stack[2];

                    Some((
                        ValueType::PointerTo(Box::new(mold_type(lhs)?)),
                        rhs
                    ))
                },

                op => log_error!("Cannot create initialization with splitting operator {:?}", op)
            }
        },

        _ => log_error!("Error converting expression {} to an initialization", expr)
    }
}

macro_rules! try_boxed_mold {
    ($expr:expr) => {
        match $expr {
            Some(e) => Some(Box::new(mold_expression(e)?)),
            None => None
        }
    }
}

pub(crate) fn mold_expression(expr: &UVExpr) -> Option<CXExpr> {
    match expr {
        UVExpr::Identifier(ident) =>
            Some(CXExpr::Identifier(ident.clone())),

        UVExpr::ExprChain(exprs) => {
            let exprs = exprs.iter()
                .map(|expr| mold_expression(expr))
                .collect::<Option<Vec<_>>>()?;

            Some(CXExpr::Block { exprs })
        }

        UVExpr::Return { value } => {
            let val = try_boxed_mold!(value);

            Some(CXExpr::Return { value: val })
        }

        UVExpr::If { condition, then_branch, else_branch } => {
            let cond = mold_expression(condition)?;
            let then_cond = mold_expression(then_branch)?;
            let else_cond = try_boxed_mold!(else_branch);

            Some(
                CXExpr::If {
                    condition: Box::new(cond),
                    then_branch: Box::new(then_cond),
                    else_branch: else_cond
                }
            )
        },

        UVExpr::While { condition, body} => {
            let cond = mold_expression(condition.as_ref())?;
            let body = mold_expression(body.as_ref())?;

            Some(
                CXExpr::While {
                    condition: Box::new(cond),
                    body: Box::new(body)
                }
            )
        },

        UVExpr::For {
            init, condition,
            increment, body
        } => {
            let init = try_boxed_mold!(init);
            let condition = try_boxed_mold!(condition);
            let increment = try_boxed_mold!(increment);

            let body = mold_expression(body)?;

            Some(
                CXExpr::For {
                    init, condition, increment,
                    body: Box::new(body)
                }
            )
        },

        _ => todo!("{}", expr)
    }
}

pub(crate) fn mold_type(expr: &UVExpr) -> Option<ValueType> {
    match expr {
        UVExpr::Identifier(ident) => {
            Some(ValueType::Identifier(ident.clone()))
        },

        _ => log_error!("Unknown type: {}", expr)
    }
}