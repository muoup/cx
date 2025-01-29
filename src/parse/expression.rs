use crate::lex::token::Punctuator::{CloseParen, Comma, OpenParen, Semicolon};
use crate::lex::token::{KeywordType, Token};
use crate::parse::ast::Expression;
use crate::parse::parser::{parse_body, TokenIter};
use crate::parse::val_type::parse_type;

pub(crate) fn parse_expressions(toks: &mut TokenIter, splitter: Token, terminator: Token) -> Option<Vec<Expression>> {
    let mut exprs = Vec::new();

    while let Some(expression) = parse_expression(toks) {
        exprs.push(expression);

        if toks.peek() == Some(&&splitter) {
            toks.next();
        }

        if toks.peek() == Some(&&terminator) {
            return Some(exprs);
        }
    }

    None
}

pub(crate) fn parse_expression(toks: &mut TokenIter) -> Option<Expression> {
    match toks.peek()? {
        Token::Keyword(_) => parse_keyword_expression(toks),
        Token::Identifier(_) => parse_identifier_expression(toks),

        _ => match toks.next().unwrap() {
            Token::Punctuator(OpenParen) => {
                let expr = parse_expression(toks)?;
                assert_eq!(toks.next(), Some(&Token::Punctuator(CloseParen)));
                Some(expr)
            }

            Token::StringLiteral(value) => Some(Expression::StringLiteral(value.clone())),
            Token::IntLiteral(value) => Some(Expression::IntLiteral(*value)),
            Token::FloatLiteral(value) => Some(Expression::FloatLiteral(*value)),

            other => panic!("Unexpected expression token: {:?}", other)
        }
    }
}

fn parse_keyword_expression(toks: &mut TokenIter) -> Option<Expression> {
    let keyword = match toks.next()? {
        Token::Keyword(keyword) => keyword,
        _ => return None
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
            assert_eq!(toks.next(), Some(&Token::Punctuator(OpenParen)));
            let condition = Box::new(parse_expression(toks)?);
            assert_eq!(toks.next(), Some(&Token::Punctuator(CloseParen)));

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

            if toks.peek() == Some(&&Token::Punctuator(Semicolon)) {
                Some(Expression::VariableDeclaration { type_, name })
            } else {
                unimplemented!("Variable assignment");
            }
        }
    }
}

fn parse_identifier_expression(toks: &mut TokenIter) -> Option<Expression> {
    let identifier = match toks.next()? {
        Token::Identifier(identifier) => identifier.clone(),
        _ => return None
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
        _ => None
    }
}
