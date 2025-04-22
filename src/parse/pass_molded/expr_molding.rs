use crate::lex::token::OperatorType;
use crate::log_error;
use crate::parse::ast::ValueType;
use crate::parse::pass_molded::{CXBinOp, CXExpr, CXAST};
use crate::parse::pass_molded::operators::{binop_precedence, mold_binop, mold_unop, uv_cx_unop};
use crate::parse::pass_molded::pattern_molding::{mold_delimited, PseudoUVExpr};
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

        UVExpr::Compound { left, right } => mold_compound_expr(left, right),
        UVExpr::Complex { expr_stack, op_stack } => {
            let borrowed_exprs = expr_stack.iter()
                .by_ref()
                .collect::<Vec<_>>();
            let borrowed_ops = op_stack.iter()
                .by_ref()
                .collect::<Vec<_>>();

            let pseudo = mold_expr_stack(borrowed_exprs.as_slice(), borrowed_ops.as_slice())?;

            mold_pseudo_expr(&pseudo)
        },

        UVExpr::IntLiteral(i) => Some(CXExpr::IntLiteral { val: *i, bytes: 8 }),
        UVExpr::FloatLiteral(f) => Some(CXExpr::FloatLiteral { val: *f, bytes: 8 }),
        UVExpr::StringLiteral(s) => Some(CXExpr::StringLiteral { val: s.clone() }),

        UVExpr::Parenthesized(expr) => {
            let Some(expr) = expr.as_ref() else {
                log_error!("Parenthesized expression is empty: {:?}", expr);
            };

            mold_expression(expr)
        },

        _ => Some(CXExpr::Identifier(format!("TODO: {:#?}", expr)))
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

pub(crate) fn mold_expr_stack<'a>(exprs: &[&'a UVExpr], ops: &[&'a UVBinOp]) -> Option<PseudoUVExpr<'a>> {
    let mut cx_expr_stack = vec![PseudoUVExpr::ID(&exprs[0])];
    let mut bin_op_stack : Vec<&UVBinOp> = Vec::new();

    let mut op_iter = ops.iter();
    let mut previous_precedence: u8 = 0;

    for expr in exprs.iter().skip(1) {
        let expr = expr;
        let Some(op) = op_iter.next() else {
            log_error!("Error parsing expression stack: {:?} | {:?}", ops, exprs);
        };
        let op_precedence = binop_precedence(op);

        if op_precedence < previous_precedence {
            let mut collapsee = cx_expr_stack.pop().unwrap();

            while cx_expr_stack.is_empty() {
                let op = bin_op_stack.pop().unwrap();
                let rhs = cx_expr_stack.pop().unwrap();

                collapsee = PseudoUVExpr::BinOp {
                    left: Box::new(collapsee),
                    right: Box::new(rhs),
                    op: op.clone()
                };
            }

            previous_precedence = 0;
        }

        bin_op_stack.push(op);
        cx_expr_stack.push(PseudoUVExpr::ID(expr));
        previous_precedence = op_precedence;
    }

    cx_expr_stack.pop()
}

pub(crate) fn mold_pseudo_expr(expr: &PseudoUVExpr) -> Option<CXExpr> {
    match expr {
        PseudoUVExpr::ID(expr) => mold_expression(expr),

        PseudoUVExpr::BinOp { left, right, op } => {
            let left = mold_pseudo_expr(left)?;
            let right = mold_pseudo_expr(right)?;

            mold_binop(left, right, op)
        }
    }
}

pub(crate) fn mold_compound_expr(left: &UVExpr, right: &UVExpr) -> Option<CXExpr> {
    match (left, right) {
        (_, UVExpr::Identifier(ident)) => {
            Some(
                CXExpr::VarInitialization{
                    type_: mold_type(left)?,
                    name: ident.clone(),
                }
            )
        },

        (UVExpr::Identifier(_), UVExpr::Parenthesized(expr)) => {
            let args =
                expr.as_ref()
                    .map(|expr| mold_delimited(expr, UVBinOp::Comma))
                    .flatten()
                    .unwrap_or(vec![])
                    .iter()
                    .map(|uv_expr| mold_pseudo_expr(uv_expr))
                    .collect::<Option<Vec<_>>>()?;

            Some(
                CXExpr::FunctionCall {
                    callee: Box::new(mold_expression(left)?),
                    args
                }
            )
        },

        (_, _) => todo!("mold_compound_expr(`{}`, `{}`)", left, right)
    }
}