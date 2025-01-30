use crate::lex::token::PunctuatorType::{CloseParen, Comma, OpenParen, Semicolon};
use crate::lex::token::{KeywordType, OperatorType, Token};
use crate::parse::ast::Expression;
use crate::parse::parser::{parse_body, TokenIter};
use crate::parse::val_type::parse_type;

pub(crate) fn parse_expressions(toks: &mut TokenIter, splitter: Token, terminator: Token) -> Option<Vec<Expression>> {
    let mut exprs = Vec::new();
    let mut recent_iter = toks.index;

    while let Some(expression) = parse_expression(toks) {
        exprs.push(expression);

        if toks.peek() == Some(&&splitter) {
            toks.next();
            recent_iter = toks.index;
        }

        if toks.peek() == Some(&&terminator) {
            return Some(exprs);
        }
    }

    panic!("Expression could not be formed starting at token: {:?}", toks.slice[recent_iter]);
}

pub(crate) fn parse_expression(toks: &mut TokenIter) -> Option<Expression> {
    let mut expr_stack = Vec::new();
    let mut op_stack = Vec::new();

    fn condense_stack(expr_stack: &mut Vec<Expression>, op_stack: &mut Vec<OperatorType>) {
        while let Some(op) = op_stack.pop() {
            let right = expr_stack.pop().unwrap();
            let left = expr_stack.pop().unwrap();

            expr_stack.push(Expression::BinaryOperation {
                operator: op,
                left: Box::new(left),
                right: Box::new(right)
            });
        }
    }

    loop {
        expr_stack.push(parse_expression_value(toks)?);

        if !toks.has_next() {
            break;
        }

        match toks.peek().unwrap() {
            Token::Operator(_) => {
                // This is hacky, but I'm not sure how else to make the borrow checker happy
                let Token::Operator(op) = *toks.next().unwrap() else { unreachable!() };

                let prev_precedence = op_stack.last()
                    .map(|op: &OperatorType| op.precedence())
                    .unwrap_or_else(|| i32::MAX);
                let curr_precedence = op.precedence();

                if curr_precedence < prev_precedence {
                    condense_stack(&mut expr_stack, &mut op_stack);
                }

                op_stack.push(op);
            },
            _ => break
        }
    }

    condense_stack(&mut expr_stack, &mut op_stack);

    assert_eq!(expr_stack.len(), 1);
    Some(expr_stack.pop().unwrap())
}

fn parse_expression_value(toks: &mut TokenIter) -> Option<Expression> {
    if let Some(var) = parse_variable_declaration(toks) {
        return Some(var);
    }

    match toks.peek()? {
        Token::Keyword(_) => parse_keyword_expression(toks),
        Token::Identifier(_) => parse_identifier_expression(toks),

        _ => match toks.next().unwrap() {
            Token::Punctuator(OpenParen) => {
                let expr = parse_expression(toks)?;
                assert_eq!(toks.next(), Some(&Token::Punctuator(CloseParen)));
                Some(expr)
            }

            Token::StringLiteral(str) => Some(Expression::StringLiteral(str.clone())),
            Token::IntLiteral(int) => Some(Expression::IntLiteral(*int)),
            Token::FloatLiteral(float) => Some(Expression::FloatLiteral(*float)),

            _ => None
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
                if toks.peek() == Some(&&Token::Punctuator(Semicolon)) {
                    Expression::Return(Box::new(Expression::Unit))
                } else {
                    Expression::Return(Box::new(parse_expression(toks)?))
                }
            ),
        KeywordType::Continue => Some(Expression::Continue),
        KeywordType::Break => Some(Expression::Break),

        KeywordType::If => {
            let condition = Box::new(parse_expression(toks)?);

            let then = parse_body(toks);

            let else_ = if toks.peek() == Some(&&Token::Keyword(KeywordType::Else)) {
                toks.next();
                parse_body(toks)
            } else {
                Vec::new()
            };

            Some(Expression::If { condition, then, else_ })
        },

        _ => {
            toks.back();
            let type_ = parse_type(toks)?;
            let name = match toks.next()? {
                Token::Identifier(name) => name.clone(),
                _ => return None
            };

            if matches!(toks.peek(), Some(&Token::Punctuator(_))) {
                Some(Expression::VariableDeclaration { type_, name })
            } else {
                unimplemented!("Variable assignment");
            }
        }
    }
}

fn parse_variable_declaration(toks: &mut TokenIter) -> Option<Expression> {
    let pre_index = toks.index;
    let type_ = parse_type(toks)?;
    let name = match toks.next()? {
        Token::Identifier(name) => name.clone(),
        _ => {
            toks.index = pre_index;
            return None;
        }
    };

    Some(Expression::VariableDeclaration { type_, name })
}

fn parse_identifier_expression(toks: &mut TokenIter) -> Option<Expression> {
    let identifier = match toks.next()? {
        Token::Identifier(identifier) => identifier.clone(),
        _ => panic!("Called parse_identifier_expression with non-identifier")
    };

    match toks.next()? {
        // Function call (identifier followed by open paren)
        Token::Punctuator(OpenParen) => {
            let args = parse_expressions(toks, Token::Punctuator(Comma), Token::Punctuator(CloseParen))?;
            assert_eq!(toks.next(), Some(&Token::Punctuator(CloseParen)));
            Some(
                Expression::FunctionCall {
                    name: Box::new(Expression::Identifier(identifier.clone())),
                    args
                }
            )
        },
        _ => {
            toks.back();
            Some(Expression::Identifier(identifier.clone()))
        }
    }
}
