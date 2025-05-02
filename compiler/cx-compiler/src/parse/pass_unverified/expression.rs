use crate::lex::token::{KeywordType, OperatorType, PunctuatorType, Token};
use crate::{assert_token_matches, log_error, try_next};
use crate::mangling::namespace_mangle;
use crate::parse::macros::error_pointer;
use crate::parse::parser::{ParserData};
use crate::parse::pass_unverified::{UVExpr, UVOp};
use crate::parse::pass_unverified::global_scope::parse_body;

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

    while data.toks.has_next() {
        if let Some(Token::Operator(op)) = data.toks.peek() {
            op_stack.push(UVOp::BinOp(op.clone()));
            data.toks.next(); // consume the operator token

            let Some(()) = parse_expr_index(data, &mut expr_stack, &mut op_stack) else {
                log_error!("Failed to parse expression index after operator: {:#?}", data.toks.peek());
            };
        } else if let Some(Token::Assignment(op)) = data.toks.peek() {
            op_stack.push(UVOp::Assignment(op.clone()));
            data.toks.next(); // consume the assignment token

            let Some(()) = parse_expr_index(data, &mut expr_stack, &mut op_stack) else {
                log_error!("Failed to parse expression index after assignment: {:#?}", data.toks.peek());
            };
        } else {
            let Some(()) = parse_expr_index(data, &mut expr_stack, &mut op_stack) else {
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

    if op_stack.is_empty() {
        assert_eq!(expr_stack.len(), 1, "Expression stack should have exactly one element");

        Some(expr_stack.pop().unwrap())
    } else {
        Some(UVExpr::Complex { op_stack, expr_stack })
    }
}

fn parse_expr_index(data: &mut ParserData, expr_stack: &mut Vec<UVExpr>, op_stack: &mut Vec<UVOp>)
                    -> Option<()> {
    while let Some(Token::Operator(op)) = data.toks.peek() {
        op_stack.push(UVOp::UnOpPre(op.clone()));
        data.toks.next(); // consume the operator token
    }

    let Some(expr) = parse_expr_val(data) else {
        return None;
    };

    expr_stack.push(expr);

    Some(())
}

pub(crate) fn parse_expr_val(data: &mut ParserData) -> Option<UVExpr> {
    let mut acc = match data.toks.next()? {
        Token::Identifier(name) => Some(UVExpr::Identifier(name.clone())),
        Token::IntLiteral(value) => Some(UVExpr::IntLiteral(value.clone())),
        Token::FloatLiteral(value) => Some(UVExpr::FloatLiteral(value.clone())),
        Token::StringLiteral(value) => Some(UVExpr::StringLiteral(value.clone())),
        Token::Intrinsic(_) => {
            data.toks.back();

            Some(
                UVExpr::Identifier(
                    parse_identifier(data)?
                )
            )
        }

        Token::Punctuator(PunctuatorType::OpenBrace) => parse_braced_expr(data),

        Token::Keyword(keyword) =>
            log_error!("Statement Expressioning not supported: {:#?}", keyword),

        Token::Punctuator(PunctuatorType::OpenParen) => {
            let expr = parse_expr(data)?;

            assert_token_matches!(data, Token::Punctuator(PunctuatorType::CloseParen));

            Some(UVExpr::Parenthesized(Some(Box::new(expr))))
        },

        _ => {
            data.toks.back();

            let Some(ident) = parse_identifier(data) else {
                return None
            };

            Some(UVExpr::Identifier(ident))
        },
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

pub(crate) fn parse_identifier(data: &mut ParserData) -> Option<String> {
    if let Some(Token::Identifier(name)) = data.toks.next().cloned() {
        if let Some(Token::Operator(OperatorType::ScopeRes)) = data.toks.peek() {
            data.toks.next();
            let Token::Identifier(scoped_name) = data.toks.next()? else {
                log_error!("PARSER ERROR: Expected identifier after scope resolution operator! Found token: {:?}", {
                    data.toks.back();
                    data.toks.peek()
                });
            };

            return Some(
                namespace_mangle(
                    name.as_str(),
                    scoped_name.as_str()
                )
            )
        }

        return Some(name);
    }

    data.toks.back();

    let mut ss = String::new();

    while let Some(Token::Intrinsic(intrinsic)) = data.toks.next() {
        ss.push_str(format!("{:?}", intrinsic).to_lowercase().as_str());
    }

    data.toks.back();

    if ss.is_empty() {
        None
    } else {
        Some(ss)
    }
}