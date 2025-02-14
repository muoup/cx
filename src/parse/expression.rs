use crate::lex::token::PunctuatorType::{CloseParen, OpenParen, Semicolon};
use crate::lex::token::{KeywordType, OperatorType, PunctuatorType, Token};
use crate::parse::ast::{ControlExpression, Expression, LiteralExpression, UnverifiedExpression, ValueExpression, ValueType};
use crate::parse::global_scope::parse_struct_definition;
use crate::parse::parser::{parse_body, TokenIter};

/**
 *  This function is essentially a function for the sake of backwards compatibility. While modern
 *  languages will have clear types like "u32" or "i32", C has the possibility of multi-word types
 *  like "unsigned int" or "long long". This function will handle those cases.
 */
pub fn parse_identifier(toks: &mut TokenIter) -> Option<String> {
    let mut accumulator = String::new();

    while let Some(Token::Intrinsic(name)) = toks.peek() {
        accumulator.push_str(format!("{:?} ", name).to_lowercase().as_str());
        toks.next();
    }

    accumulator.pop(); // Remove trailing space
    Some(accumulator)
}

pub(crate) fn parse_expressions(toks: &mut TokenIter, splitter: Token, terminator: Token) -> Option<Vec<Expression>> {
    let mut exprs = Vec::new();
    let mut recent_iter = toks.index;

    loop {
        if toks.peek() == Some(&splitter) {
            toks.next();
            recent_iter = toks.index;
        }

        if toks.peek() == Some(&terminator) {
            return Some(exprs);
        }

        let Some (expr) = parse_expression(toks) else {
            println!("{:?}", exprs);
            panic!("Expression could not be formed starting at token, near token: {:?} {:?}", toks.slice[recent_iter], toks.slice[toks.index]);
        };

        exprs.push(expr);
    }
}
fn compress_stack(expr_stack: &mut Vec<Expression>, op_stack: &mut Vec<OperatorType>) -> Option<()> {
    while let Some(op) = op_stack.pop() {
        let right = expr_stack.pop()?;
        let left = expr_stack.pop()?;

        expr_stack.push(
            Expression::Unverified (
                UnverifiedExpression::BinaryOperation {
                    operator: op,
                    left: Box::new(left),
                    right: Box::new(right)
                }
            )
        );
    }

    Some(())
}

fn parse_operator(toks: &mut TokenIter, expr_stack: &mut Vec<Expression>, op_stack: &mut Vec<OperatorType>) -> Option<()> {
    match toks.peek()? {
        Token::Operator(_) => {
            // This is hacky, but I'm not sure how else to make the borrow checker happy
            let Token::Operator(op) = *toks.next().unwrap() else { unreachable!() };

            let prev_precedence = op_stack.last()
                .map(|op: &OperatorType| op.precedence())
                .unwrap_or_else(|| i32::MAX);
            let curr_precedence = op.precedence();

            if curr_precedence < prev_precedence {
                compress_stack(expr_stack, op_stack);
            }

            op_stack.push(op);
            Some(())
        },
        Token::Assignment(_) => {
            compress_stack(expr_stack, op_stack);

            let left = expr_stack.pop()?;
            let Token::Assignment(operator) = toks.next().unwrap().clone() else { unreachable!() };
            let right = parse_expression(toks)?;

            expr_stack.push(Expression::Value (
                ValueExpression::Assignment {
                    left: Box::new(left),
                    right: Box::new(right),
                    operator
                }
            ));
            None
        },
        Token::Punctuator(_) => None,

        _ => {
            let top_expr = expr_stack.pop()?;

            if let Some(expr) = parse_expression(toks) {
                expr_stack.push(
                    Expression::Unverified(
                        UnverifiedExpression::CompoundExpression {
                            prefix: Box::new(top_expr),
                            suffix: Box::new(expr)
                        }
                    )
                );

                parse_operator(toks, expr_stack, op_stack)
            } else {
                expr_stack.push(top_expr);

                None
            }
        }
    }
}

pub(crate) fn parse_expression(toks: &mut TokenIter) -> Option<Expression> {
    let mut expr_stack = Vec::new();
    let mut op_stack = Vec::new();

    loop {
        let expr = parse_expression_value(toks)?;
        let suffixed = parse_expression_suffix(expr, toks)?;
        expr_stack.push(suffixed);

        let Some(_) = parse_operator(toks, &mut expr_stack, &mut op_stack) else {
            break
        };
    }

    compress_stack(&mut expr_stack, &mut op_stack);

    if expr_stack.is_empty() {
        return Some(Expression::NOP);
    }

    assert_eq!(expr_stack.len(), 1);
    Some(expr_stack.pop().unwrap())
}

fn parse_expression_suffix(expr: Expression, toks: &mut TokenIter) -> Option<Expression> {
    let expr = match toks.peek()? {
        Token::Punctuator(OpenParen) => {
            toks.next();
            let mut args = parse_expressions(toks, Token::Punctuator(PunctuatorType::Comma), Token::Punctuator(PunctuatorType::CloseParen))?;
            assert_eq!(toks.next(), Some(&Token::Punctuator(CloseParen)));
            Expression::Unverified (
                UnverifiedExpression::FunctionCall {
                    name: Box::new(expr),
                    args
                }
            )
        },
        Token::Punctuator(PunctuatorType::OpenBracket) => {
            toks.next();
            let index = parse_expression(toks)?;
            assert_eq!(toks.next(), Some(&Token::Punctuator(PunctuatorType::CloseBracket)));

            Expression::Unverified (
                UnverifiedExpression::BinaryOperation {
                    operator: OperatorType::ArrayIndex,
                    left: Box::new(expr),
                    right: Box::new(index)
                }
            )
        }

        _ => return Some(expr),
    };

    parse_expression_suffix(expr, toks)
}


fn parse_expression_value(toks: &mut TokenIter) -> Option<Expression> {
    match toks.next()? {
        Token::Keyword(_) => {
            toks.back();
            parse_keyword_expression(toks)
        },
        Token::Intrinsic(_) => {
            toks.back();
            Some(
                Expression::Unverified(
                    UnverifiedExpression::Identifier(parse_identifier(toks)?)
                )
            )
        },
        Token::Identifier(name) => Some(
            Expression::Unverified (
                UnverifiedExpression::Identifier(name.clone())
            )
        ),
        Token::Punctuator(OpenParen) => {
            let expr = parse_expression(toks)?;
            assert_eq!(toks.next(), Some(&Token::Punctuator(CloseParen)));
            Some(expr)
        },
        Token::Operator(op) => Some(
            Expression::Value(
                ValueExpression::UnaryOperation {
                    operator: *op,
                    operand: parse_expression_value(toks).map(Box::new)
                }
            )
        ),
        Token::IntLiteral(val) => {
            Some(
                Expression::Literal(
                    LiteralExpression::IntLiteral { val: *val, bytes: 4 }
                )
            )
        },
        Token::FloatLiteral(val) => {
            Some(
                Expression::Literal(
                    LiteralExpression::FloatLiteral { val: *val, bytes: 4 }
                )
            )
        },
        Token::StringLiteral(val) => {
            Some(
                Expression::Literal(
                    LiteralExpression::StringLiteral(val.clone())
                )
            )
        },
        Token::Punctuator(OpenParen) => {
            let expr = parse_expression(toks)?;
            assert_eq!(toks.next(), Some(&Token::Punctuator(CloseParen)));
            Some(expr)
        },
        _ => {
            toks.back();
            None
        }
    }
}


fn parse_keyword_expression(toks: &mut TokenIter) -> Option<Expression> {
    let Some(Token::Keyword(keyword)) = toks.next() else {
        unreachable!();
    };

    match keyword {
        KeywordType::Return =>
            Some (
                Expression::Control(
                    ControlExpression::Return(Box::new(
                        if toks.peek() == Some(&&Token::Punctuator(Semicolon)) {
                            toks.next();
                            Expression::Unit
                        } else {
                            parse_expression(toks)?
                        }
                    ))
                )
            ),
        KeywordType::Continue => Some(Expression::Control(ControlExpression::Continue)),
        KeywordType::Break => Some(Expression::Control(ControlExpression::Break)),

        KeywordType::If => {
            let condition = Box::new(parse_expression(toks)?);

            let then = parse_body(toks);

            let else_ = if toks.peek() == Some(&&Token::Keyword(KeywordType::Else)) {
                toks.next();
                parse_body(toks)
            } else {
                Vec::new()
            };

            Some(
                Expression::Control(
                    ControlExpression::If {
                        condition, then, else_
                    }
                )
            )
        },

        KeywordType::While => {
            let condition = Box::new(parse_expression(toks)?);
            let body = parse_body(toks);

            Some(
                Expression::Control(
                    ControlExpression::Loop {
                        condition,
                        body,
                        evaluate_condition_first: true
                    }
                )
            )
        },
        KeywordType::For => {
            assert_eq!(toks.next(), Some(&Token::Punctuator(OpenParen)));

            let mut exprs = parse_expressions(toks, Token::Punctuator(Semicolon), Token::Punctuator(CloseParen))?;
            assert_eq!(exprs.len(), 3);

            let increment = exprs.pop().unwrap();
            let condition = exprs.pop().unwrap();
            let init = exprs.pop().unwrap();
            assert_eq!(toks.next(), Some(&Token::Punctuator(CloseParen)));

            let body = parse_body(toks);

            Some(
                Expression::Control(
                    ControlExpression::ForLoop {
                        init: Box::new(init),
                        increment: Box::new(increment),
                        condition: Box::new(condition), body
                    }
                )
            )
        },
        KeywordType::Do => {
            let body = parse_body(toks);

            assert_eq!(toks.next(), Some(&Token::Keyword(KeywordType::While)));
            let condition = Box::new(parse_expression(toks)?);

            Some(
                Expression::Control (
                    ControlExpression::Loop {
                        condition,
                        body,
                        evaluate_condition_first: false
                    }
                )
            )
        },

        _ => None,
    }
}