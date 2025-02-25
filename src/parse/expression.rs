use crate::assert_token_matches;
use crate::log_error;
use crate::lex::token::PunctuatorType::{CloseParen, OpenParen, Semicolon};
use crate::lex::token::{KeywordType, OperatorType, PunctuatorType, Token};
use crate::parse::ast::{ControlExpression, Expression, FunctionParameter, LiteralExpression, ValueExpression, ValueType, VarInitialization};
use crate::parse::contextless_expression::{contextualize_lvalue, contextualize_rvalue, detangle_initialization, ContextlessExpression};
use crate::parse::parser::{parse_body, ParserData, TokenIter};
use std::fmt::Debug;

/**
 *  This function is essentially a function for the sake of backwards compatibility. While modern
 *  languages will have clear types like "u32" or "i32", C has the possibility of multi-word types
 *  like "unsigned int" or "long long". This function will handle those cases.
 */
pub fn parse_identifier(data: &mut ParserData) -> Option<String> {
    let mut accumulator = String::new();

    while let Some(Token::Intrinsic(name)) = data.toks.peek() {
        accumulator.push_str(format!("{:?} ", name).to_lowercase().as_str());
        data.toks.next();
    }

    accumulator.pop(); // Remove trailing space
    Some(accumulator)
}

pub(crate) fn parse_list<T>(data: &mut ParserData, splitter: Token, terminator: Token, parser: fn(&mut ParserData) -> Option<T>)
    -> Option<Vec<T>>
    where T: Debug
{
    let mut exprs = Vec::new();
    let mut recent_iter = data.toks.index;

    loop {
        if data.toks.peek() == Some(&splitter) {
            data.toks.next();
            recent_iter = data.toks.index;
        }

        if data.toks.peek() == Some(&terminator) {
            return Some(exprs);
        }

        let Some (expr) = parser(data) else {
            println!("{:?}", exprs);
            return None;
        };

        exprs.push(expr);
    }
}

pub(crate) fn parse_rvals(data: &mut ParserData, splitter: Token, terminator: Token) -> Option<Vec<Expression>> {
    parse_list(data, splitter, terminator, parse_rvalue)
}

pub(crate) fn parse_lvals(data: &mut ParserData, splitter: Token, terminator: Token) -> Option<Vec<Expression>> {
    parse_list(data, splitter, terminator, parse_lvalue)
}

fn compress_stack(expr_stack: &mut Vec<ContextlessExpression>, op_stack: &mut Vec<OperatorType>) -> Option<()> {
    while let Some(op) = op_stack.pop() {
        let right = expr_stack.pop()?;
        let left = expr_stack.pop()?;

        expr_stack.push(
            ContextlessExpression::BinaryOperation {
                op,
                left: Box::new(left),
                right: Box::new(right)
            }
        );
    }

    Some(())
}

fn parse_operator(data: &mut ParserData, expr_stack: &mut Vec<ContextlessExpression>, op_stack: &mut Vec<OperatorType>) -> Option<()> {
    match data.toks.peek()? {
        Token::Operator(_) => {
            // This is hacky, but I'm not sure how else to make the borrow checker happy
            let Token::Operator(op) = *data.toks.next().unwrap() else { unreachable!() };

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
            let Token::Assignment(operator) = data.toks.next().unwrap().clone() else { unreachable!() };
            let right = parse_expression(data)?;

            expr_stack.push(
                ContextlessExpression::UnambiguousExpression(
                    Expression::Value(
                        ValueExpression::Assignment {
                            operator,

                            left: Box::new(contextualize_lvalue(left)?),
                            right: Box::new(contextualize_rvalue(right)?)
                        }
                    )
                )
            );
            None
        },
        Token::Punctuator(_) => None,

        _ => {
            let top_expr = expr_stack.pop()?;

            if let Some(expr) = parse_expression(data) {
                expr_stack.push(
                    ContextlessExpression::CompoundExpression {
                        left: Box::new(top_expr),
                        right: Box::new(expr)
                    }
                );

                parse_operator(data, expr_stack, op_stack)
            } else {
                expr_stack.push(top_expr);

                None
            }
        }
    }
}

pub(crate) fn parse_rvalue(data: &mut ParserData) -> Option<Expression> {
    contextualize_rvalue(
        parse_expression(data)?
    )
}

pub(crate) fn parse_lvalue(data: &mut ParserData) -> Option<Expression> {
    contextualize_lvalue(
        parse_expression(data)?
    )
}

pub(crate) fn parse_initialization(data: &mut ParserData) -> Option<VarInitialization> {
    let expr = parse_expression(data)?;
    detangle_initialization(expr)
}

pub(crate) fn parse_expression(data: &mut ParserData) -> Option<ContextlessExpression> {
    let mut expr_stack = Vec::new();
    let mut op_stack = Vec::new();

    loop {
        let expr = parse_expression_value(data)?;
        let suffixed = parse_expression_suffix(expr, data)?;
        expr_stack.push(suffixed);

        let Some(_) = parse_operator(data, &mut expr_stack, &mut op_stack) else {
            break
        };
    }

    compress_stack(&mut expr_stack, &mut op_stack);

    if expr_stack.is_empty() {
        return Some(ContextlessExpression::UnambiguousExpression(Expression::NOP));
    }

    assert_eq!(expr_stack.len(), 1);

    expr_stack.pop()
}

fn parse_expression_suffix(expr: ContextlessExpression, data: &mut ParserData) -> Option<ContextlessExpression> {
    let expr = match data.toks.peek()? {
        Token::Punctuator(OpenParen) => {
            data.toks.next();
            let mut args = parse_list(
                data,
                Token::Punctuator(PunctuatorType::Comma), Token::Punctuator(CloseParen),
                parse_expression
            )?;
            assert_token_matches!(data, Token::Punctuator(CloseParen));

            ContextlessExpression::FunctionCall {
                reference: Box::new(expr),
                args
            }
        },
        Token::Punctuator(PunctuatorType::OpenBracket) => {
            data.toks.next();
            let index = parse_expression(data)?;
            assert_eq!(data.toks.next(), Some(&Token::Punctuator(PunctuatorType::CloseBracket)));

            ContextlessExpression::BinaryOperation {
                op: OperatorType::ArrayIndex,
                left: Box::new(expr),
                right: Box::new(index)
            }
        }

        _ => return Some(expr),
    };

    parse_expression_suffix(expr, data)
}


fn parse_expression_value(data: &mut ParserData) -> Option<ContextlessExpression> {
    match data.toks.next()? {
        Token::Keyword(_) => {
            data.toks.back();
            Some(
                ContextlessExpression::UnambiguousExpression(
                    parse_keyword_expression(data)?
                )
            )
        },
        Token::Intrinsic(_) => {
            data.toks.back();
            Some(
                ContextlessExpression::Identifier(parse_identifier(data)?)
            )
        },
        Token::Identifier(name) => Some(
            ContextlessExpression::Identifier(name.clone())
        ),
        Token::Punctuator(OpenParen) => {
            let expr = parse_expression(data)?;
            assert_eq!(data.toks.next(), Some(&Token::Punctuator(CloseParen)));
            Some(expr)
        },
        Token::Operator(op) => Some(
            ContextlessExpression::UnaryOperation {
                op: *op,
                operand: parse_expression_value(data).map(Box::new)?
            }
        ),
        Token::IntLiteral(val) => {
            Some(
                ContextlessExpression::UnambiguousExpression(
                    Expression::Literal(
                        LiteralExpression::IntLiteral { val: *val, bytes: 4 }
                    )
                )
            )
        },
        Token::FloatLiteral(val) => {
            Some(
                ContextlessExpression::UnambiguousExpression(
                    Expression::Literal(
                        LiteralExpression::FloatLiteral { val: *val, bytes: 4 }
                    )
                )
            )
        },
        Token::StringLiteral(val) => {
            Some(
                ContextlessExpression::UnambiguousExpression(
                    Expression::Literal(
                        LiteralExpression::StringLiteral(val.clone())
                    )
                )
            )
        },
        Token::Punctuator(OpenParen) => {
            let expr = parse_expression(data)?;
            assert_eq!(data.toks.next(), Some(&Token::Punctuator(CloseParen)));
            Some(expr)
        },
        _ => {
            data.toks.back();
            None
        }
    }
}


fn parse_keyword_expression(data: &mut ParserData) -> Option<Expression> {
    let Some(Token::Keyword(keyword)) = data.toks.next() else {
        unreachable!();
    };

    match keyword {
        KeywordType::Return =>
            Some (
                Expression::Control(
                    ControlExpression::Return(
                        Box::new(
                            if data.toks.peek() == Some(&&Token::Punctuator(Semicolon)) {
                                data.toks.next();
                                Expression::Unit
                            } else {
                                parse_rvalue(data)?
                            }
                        )
                    )
                )
            ),
        KeywordType::Continue => Some(Expression::Control(ControlExpression::Continue)),
        KeywordType::Break => Some(Expression::Control(ControlExpression::Break)),

        KeywordType::If => {
            let condition = Box::new(parse_expression(data)?);

            let then = parse_body(data);

            let else_ = if data.toks.peek() == Some(&&Token::Keyword(KeywordType::Else)) {
                data.toks.next();
                parse_body(data)
            } else {
                Vec::new()
            };

            Some(
                Expression::Control(
                    ControlExpression::If {
                        condition: Box::new(contextualize_rvalue(*condition)?),
                        then, else_
                    }
                )
            )
        },

        KeywordType::While => {
            let condition = Box::new(parse_expression(data)?);
            let body = parse_body(data);

            Some(
                Expression::Control(
                    ControlExpression::Loop {
                        condition: Box::new(contextualize_rvalue(*condition)?),
                        body,
                        evaluate_condition_first: true
                    }
                )
            )
        },
        KeywordType::For => {
            assert_eq!(data.toks.next(), Some(&Token::Punctuator(OpenParen)));

            let mut exprs = parse_rvals(data, Token::Punctuator(Semicolon), Token::Punctuator(CloseParen))?;
            assert_eq!(exprs.len(), 3);

            let increment = exprs.pop().unwrap();
            let condition = exprs.pop().unwrap();
            let init = exprs.pop().unwrap();
            assert_eq!(data.toks.next(), Some(&Token::Punctuator(CloseParen)));

            let body = parse_body(data);

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
            let body = parse_body(data);

            assert_eq!(data.toks.next(), Some(&Token::Keyword(KeywordType::While)));
            let condition = Box::new(parse_expression(data)?);

            Some(
                Expression::Control (
                    ControlExpression::Loop {
                        condition: Box::new(contextualize_rvalue(*condition)?),
                        body,
                        evaluate_condition_first: false
                    }
                )
            )
        },

        _ => None,
    }
}