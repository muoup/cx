use std::clone;
use crate::lex::token::OperatorType;
use crate::log_error;
use crate::parse::pass_molded::operators::{op_precedence, tok_cx_binop, tok_cx_unop};
use crate::parse::value_type::CXValType;
use crate::parse::pass_molded::{CXBinOp, CXExpr, CXUnOp, CXInitIndex, CXAST};
use crate::parse::pass_molded::pattern_molding::{mold_delimited, PseudoUVExpr};
use crate::parse::pass_unverified::{UVExpr, UVOp};

pub(crate) fn split_initialization(expr: &UVExpr) -> Option<(CXValType, Option<&UVExpr>)> {
    match expr {
        UVExpr::Compound { left, right } => {
            Some((
                mold_type(left.as_ref())?,
                Some(right.as_ref())
            ))
        },

        UVExpr::Complex { expr_stack: expression_stack, op_stack: operator_stack } => {
            if operator_stack.len() != 1 || expression_stack.len() != 2 {
                log_error!("Error parsing {} as initialization header.", expr)
            }

            match operator_stack.first().unwrap() {
                UVOp::BinOp(OperatorType::Asterisk) => {
                    let lhs = &expression_stack[1];
                    let rhs = &expression_stack[2];

                    Some((
                        CXValType::PointerTo(Box::new(mold_type(lhs)?)),
                        Some(rhs)
                    ))
                },

                op => log_error!("Cannot create initialization with splitting operator {:?}", op)
            }
        },

        _ => {
            let Some(type_) = mold_type(expr) else {
                log_error!("Failed to mold type for initialization: {}", expr);
            };

            Some((type_, None))
        }
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
            Some(CXExpr::VarReference(ident.clone())),

        UVExpr::ExprChain(exprs) => {
            let exprs = exprs.iter()
                .map(|expr| mold_expression(expr))
                .collect::<Option<Vec<_>>>()?;

            Some(CXExpr::Block { exprs, value: None })
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
            let pseudo = mold_expr_stack(expr_stack.as_slice(), op_stack.as_slice())?;

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

        UVExpr::Braced(expr) => mold_initializer(expr),

        _ => Some(CXExpr::VarReference(format!("TODO: {:#?}", expr)))
    }
}

pub(crate) fn mold_type(expr: &UVExpr) -> Option<CXValType> {
    match expr {
        UVExpr::Identifier(ident) => {
            Some(CXValType::Identifier(ident.clone()))
        },

        _ => log_error!("Unknown type: {}", expr)
    }
}

pub(crate) fn mold_expr_stack<'a>(exprs: &'a [UVExpr], ops: &'a [UVOp]) -> Option<PseudoUVExpr<'a>> {
    fn collapse_expr<'a>(
        expr_stack: &mut Vec<PseudoUVExpr<'a>>,
        expr: PseudoUVExpr<'a>,
        op: UVOp
    ) -> Option<PseudoUVExpr<'a>> {
        match op {
            UVOp::UnOpPre(op)
            | UVOp::UnOpPost(op) => {
                Some(
                    PseudoUVExpr::UnOp {
                        expr: Box::new(expr),
                        op: tok_cx_unop(op)?
                    }
                )
            },

            UVOp::BinOp(op) => {
                let left = expr_stack.pop()
                    .expect("Expression stack should not be empty when collapsing binary operation");

                Some(
                    PseudoUVExpr::BinOp {
                        left: Box::new(left),
                        right: Box::new(expr),
                        op
                    }
                )
            },

            UVOp::Assignment(additional_op) => {
                let left = expr_stack.pop()
                    .expect("Expression stack should not be empty when collapsing assignment operation");

                Some(
                    PseudoUVExpr::Assignment {
                        left: Box::new(left),
                        right: Box::new(expr),
                        op: additional_op
                    }
                )
            }
        }
    }

    fn collapse_stack<'a>(
        cx_expr_stack: &mut Vec<PseudoUVExpr<'a>>,
        bin_op_stack: &mut Vec<UVOp>
    ) -> Option<PseudoUVExpr<'a>> {
        let mut old_expr_stack = std::mem::replace(cx_expr_stack, vec![]);
        let mut old_op_stack = std::mem::replace(bin_op_stack, vec![]);

        // At some point this can be redone to replace the first element with a None
        // and just iter skip the first element, but for now we just pop it
        let mut l_expr = old_expr_stack.remove(0);

        while !old_expr_stack.is_empty() {
            let op = old_op_stack.pop()?;

            l_expr = collapse_expr(&mut old_expr_stack, l_expr, op)?;
        }

        Some(l_expr)
    }

    let mut cx_expr_stack = vec![PseudoUVExpr::ID(&exprs[0])];
    let mut op_stack: Vec<UVOp> = Vec::new();

    let mut op_iter = ops.iter();
    let mut previous_precedence: u8 = 0;

    for expr in exprs.iter().skip(1) {
        let expr = expr;
        let Some(op) = op_iter.next() else {
            log_error!("Error parsing expression stack: {:?} | {:?}", ops, exprs);
        };
        let op_precedence = op_precedence(op.clone())?;

        if op_precedence > previous_precedence {
            let collapsed = collapse_stack(
                &mut cx_expr_stack,
                &mut op_stack
            )?;

            cx_expr_stack.push(collapsed);
        }

        op_stack.push(op.clone());
        cx_expr_stack.push(PseudoUVExpr::ID(expr));
        previous_precedence = op_precedence;
    }

    collapse_stack(
        &mut cx_expr_stack,
        &mut op_stack
    )
}

pub(crate) fn mold_pseudo_expr(expr: &PseudoUVExpr) -> Option<CXExpr> {
    match expr {
        PseudoUVExpr::ID(expr) => mold_expression(expr),

        PseudoUVExpr::UnOp { expr, op } => {
              Some(
                  CXExpr::UnOp {
                      operator: op.clone(),
                      operand: Box::new(mold_pseudo_expr(expr)?)
                  }
              )
        },

        PseudoUVExpr::Assignment { left, right, op } =>
            mold_pseudo_assn(left, right, op.clone()),
        PseudoUVExpr::BinOp { left, right, op } => {
            let left = mold_pseudo_expr(left)?;
            let right = mold_pseudo_expr(right)?;

            Some(
                CXExpr::BinOp {
                    lhs: Box::new(left),
                    rhs: Box::new(right),
                    op:  tok_cx_binop(op.clone())?
                }
            )
        }
    }
}

pub(crate) fn mold_pseudo_assn(lhs: &PseudoUVExpr, rhs: &PseudoUVExpr,
                               additional_op: Option<OperatorType>) -> Option<CXExpr> {
    match lhs {
        PseudoUVExpr::ID(UVExpr::Compound { left, right }) => {
            let Some(type_) = mold_type(left.as_ref()) else {
                log_error!("Failed to mold type for assignment: {}", left);
            };
            let UVExpr::Identifier(name) = right.as_ref() else {
                log_error!("Failed to mold name for assignment: {}", left);
            };
            let Some(rhs) = mold_pseudo_expr(rhs) else {
                log_error!("Failed to mold right side of assignment: {}", left);
            };

            Some(
                CXExpr::VarDeclaration {
                    type_,
                    name: name.clone(),
                    initializer: Some(Box::new(rhs)),
                }
            )
        },

        _ => {
            let lhs = mold_pseudo_expr(lhs)?;
            let rhs = mold_pseudo_expr(rhs)?;
            let op = additional_op.map(|op| tok_cx_binop(op.clone()))?;

            Some(
                CXExpr::Assignment {
                    lhs: Box::new(lhs),
                    rhs: Box::new(rhs),
                    op
                }
            )
        }
    }
}

pub(crate) fn mold_compound_expr(left: &UVExpr, right: &UVExpr) -> Option<CXExpr> {
    match (left, right) {
        (_, UVExpr::Identifier(ident)) => {
            Some(
                CXExpr::VarDeclaration {
                    type_: mold_type(left)?,
                    name: ident.clone(),
                    initializer: None,
                }
            )
        },

        (UVExpr::Identifier(_), UVExpr::Parenthesized(expr)) => {
            let args =
                expr.as_ref()
                    .map(|expr| mold_delimited(expr, UVOp::BinOp(OperatorType::Comma)))
                    .flatten()
                    .unwrap_or(vec![])
                    .iter()
                    .map(|uv_expr| mold_pseudo_expr(uv_expr))
                    .collect::<Option<Vec<_>>>()?;

            match left {
                UVExpr::Identifier(ident) =>
                    Some(
                        CXExpr::DirectFunctionCall {
                            name: ident.clone(),
                            args
                        }
                    ),

                _ =>
                    Some(
                        CXExpr::IndirectFunctionCall {
                            callee: Box::new(mold_expression(left)?),
                            args
                        }
                    )
            }
        },

        (_, _) => todo!("mold_compound_expr(`{}`, `{}`)", left, right)
    }
}

pub(crate) fn mold_initializer(inside: &UVExpr) -> Option<CXExpr> {
    let delimited = mold_delimited(inside, UVOp::BinOp(OperatorType::Comma))?;

    let indices = delimited.iter()
        .map(|pseudo| {
            match mold_pseudo_expr(pseudo)? {
                CXExpr::Assignment { lhs, rhs, op: Option::None } => {
                    let CXExpr::UnOp {
                        operator: CXUnOp::InitializerIndex,
                        ref operand
                    } = *lhs else {
                        log_error!("Failed to mold initializer index: {:?}", pseudo);
                    };
                    let CXExpr::VarReference(name) = operand.as_ref() else {
                        log_error!("Failed to mold initializer index: {:?}", pseudo);
                    };

                    Some(CXInitIndex::Named(name.clone(), rhs))
                },

                expr => Some(CXInitIndex::Unnamed(Box::new(expr)))
            }
        })
        .collect::<Option<Vec<_>>>();

    Some(
        CXExpr::InitializerList {
            indices: indices?
        }
    )
}