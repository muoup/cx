use std::clone;
use crate::lex::token::{KeywordType, OperatorType, PunctuatorType, Token};
use crate::{assert_token_matches, log_error, point_log_error, try_next};
use crate::mangling::namespace_mangle;
use crate::parse::macros::error_pointer;
use crate::parse::parser::{ParserData};
use crate::parse::pass_unverified::{UVExpr, UVIdent, UVOp};
use crate::parse::pass_unverified::global_scope::parse_body;
use crate::parse::pass_unverified::typing::parse_intrinisic;

pub(crate) fn requires_semicolon(expr: &UVExpr) -> bool {
    match expr {
        UVExpr::If { .. } => false,
        UVExpr::While { .. } => false,
        UVExpr::For { .. } => false,

        _ => true
    }
}

pub(crate) fn parse_expr(data: &mut ParserData) -> Option<UVExpr> {
    let mut op_stack = Vec::new();
    let mut expr_stack = Vec::new();

    if let Some(Token::Keyword(keyword)) = data.toks.peek() {
        let keyword = keyword.clone();
        data.toks.next();

        if let Some(expr) = parse_keyword_val(data, keyword) {
            return Some(expr);
        }
    }

    parse_expr_index(data, &mut expr_stack, &mut op_stack)?;

    loop {
        match data.toks.next() {
            Some(Token::Operator(op)) => {
                op_stack.push(UVOp::BinOp(op.clone()));

                let Some(()) = parse_expr_index(data, &mut expr_stack, &mut op_stack) else {
                    log_error!("Failed to parse expression index after operator: {:#?}", data.toks.peek());
                };
            }

            Some(Token::Assignment(op)) => {
                op_stack.push(UVOp::Assignment(op.clone()));

                let Some(()) = parse_expr_index(data, &mut expr_stack, &mut op_stack) else {
                    log_error!("Failed to parse expression index after assignment: {:#?}", data.toks.peek());
                };
            },

            _ => {
                let Some(()) = parse_expr_index(data.back(), &mut expr_stack, &mut op_stack) else {
                    break;
                };
                let r_expr = expr_stack.pop()?;
                let l_expr = expr_stack.pop()?;

                expr_stack.push(
                    UVExpr::Compound {
                        left: Box::new(r_expr),
                        right: Box::new(l_expr)
                    }
                )
            }
        }
    }

    if op_stack.is_empty() {
        assert_eq!(expr_stack.len(), 1, "Expression stack should have exactly one element");

        Some(expr_stack.pop().unwrap())
    } else {
        Some(UVExpr::Complex { op_stack, expr_stack })
    }
}

fn parse_expr_index(data: &mut ParserData, expr_stack: &mut Vec<UVExpr>, op_stack: &mut Vec<UVOp>)
                    -> Option<()> {
    while let Some(Token::Operator(op)) = data.toks.next() {
        op_stack.push(UVOp::UnOpPre(op.clone()));
    }

    expr_stack.push(parse_expr_val(data.back())?);

    Some(())
}

pub(crate) fn parse_expr_val(data: &mut ParserData) -> Option<UVExpr> {
    let mut acc = match data.toks.next()? {
        Token::IntLiteral(value) => Some(UVExpr::IntLiteral(value.clone())),
        Token::FloatLiteral(value) => Some(UVExpr::FloatLiteral(value.clone())),
        Token::StringLiteral(value) => Some(UVExpr::StringLiteral(value.clone())),

        Token::Intrinsic(_) => Some(
            UVExpr::Identifier(UVIdent::Identifier(parse_intrinisic(data.back())?))
        ),
        Token::Identifier(_) => Some(
            UVExpr::Identifier(parse_identifier(data.back())?)
        ),

        Token::Punctuator(PunctuatorType::OpenBrace) => parse_braced_expr(data),

        Token::Punctuator(PunctuatorType::OpenParen) => {
            let expr = parse_expr(data)?;

            assert_token_matches!(data, Token::Punctuator(PunctuatorType::CloseParen));

            Some(UVExpr::Parenthesized(Some(Box::new(expr))))
        },

        _ => {
            data.toks.back();
            return None;
        }
    }?;

    loop {
        if matches!(data.toks.peek(), None) {
            return Some(acc);
        }

        match data.toks.next()? {
            Token::Punctuator(PunctuatorType::OpenParen) => {
                let index = parse_expr(data);
                assert_token_matches!(data, Token::Punctuator(PunctuatorType::CloseParen));

                acc = UVExpr::Compound {
                    left: Box::new(acc),
                    right: Box::new(UVExpr::Parenthesized(index.map(|i| Box::new(i))))
                };
            },

            _ => {
                data.toks.back();
                return Some(acc)
            }
        }
    }
}

pub(crate) fn parse_keyword_val(data: &mut ParserData, keyword: KeywordType) -> Option<UVExpr> {
    match keyword {
        KeywordType::Return => {
            let val = parse_expr(data);

            Some(
                UVExpr::Return {
                    value: val.map(|v| Box::new(v))
                }
            )
        },
        KeywordType::If => {
            let expr = parse_expr_val(data)?;
            let then_body = parse_body(data)?;
            let else_body =
                if try_next!(data, Token::Keyword(KeywordType::Else)) {
                    parse_body(data)
                } else {
                    None
                };

            Some(
                UVExpr::If {
                    condition: Box::new(expr),
                    then_branch: Box::new(then_body),
                    else_branch: else_body.map(|b| Box::new(b))
                }
            )
        },
        KeywordType::While => {
            let expr = parse_expr_val(data)?;
            let body = parse_body(data)?;

            Some(
                UVExpr::While {
                    condition: Box::new(expr),
                    body: Box::new(body)
                }
            )
        },

        _ => log_error!("Unsupported keyword: {:#?}", keyword)
    }
}

pub(crate) fn parse_braced_expr(data: &mut ParserData) -> Option<UVExpr> {
    let expr = parse_expr(data)?;

    assert_token_matches!(data, Token::Punctuator(PunctuatorType::CloseBrace));

    Some(
        UVExpr::Braced(Box::new(expr))
    )
}

pub(crate) fn parse_name(data: &mut ParserData) -> Option<String> {
    let Some(Token::Identifier(name)) = data.toks.next() else {
        log_error!("PARSER ERROR: Expected identifier for name found: {:#?}", data.toks.peek());
    };

    Some(name.clone())
}

pub(crate) fn parse_identifier(data: &mut ParserData) -> Option<UVIdent> {
    let Some(Token::Identifier(name)) = data.toks.next().cloned() else {
        point_log_error!(data, "PARSER ERROR: Expected identifier, found: {:#?}", data.toks.prev());
    };

    let Some(Token::Operator(OperatorType::ScopeRes)) = data.toks.peek() else {
        return Some(UVIdent::Identifier(name));
    };

    let mut names = vec![name];

    while let Some(Token::Operator(OperatorType::ScopeRes)) = data.toks.peek() {
        data.toks.next(); // consume the scope resolution operator

        let Some(Token::Identifier(name)) = data.toks.next().cloned() else {
            log_error!("PARSER ERROR: Expected identifier after scope resolution operator");
        };

        names.push(name);
    }

    Some(UVIdent::ScopedIdentifier(names))
}