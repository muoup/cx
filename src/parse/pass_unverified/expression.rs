use std::clone;
use crate::lex::token::{KeywordType, OperatorType, PunctuatorType, Token};
use crate::{assert_token_matches, log_error, try_next, try_token_matches};
use crate::parse::parser::{ParserData};
use crate::parse::pass_unverified::{UVBinOp, UVExpr, UVUnOp};
use crate::parse::pass_unverified::global_scope::parse_body;
use crate::parse::pass_unverified::operators::{tok_to_binop, tok_to_unop};

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

    let Some(expr) = parse_expr_val(data) else {
        return None
    };

    expr_stack.push(expr);

    while data.toks.has_next() {
        if let Some(binop) = tok_to_binop(data.toks.peek().cloned()?) {
            op_stack.push(binop);
            data.toks.next(); // consume the operator token
        } else {
            if matches!(expr_stack.last()?, UVExpr::Identifier(_)) {
                if let Some(rhs) = parse_expr_val(data) {
                    let lhs = expr_stack.pop()?;

                    expr_stack.push(UVExpr::Compound {
                        left: Box::new(lhs),
                        right: Box::new(rhs),
                    });

                    continue;
                }
            }

            break;
        }

        let Some(expr) = parse_expr_val(data) else {
            log_error!("Failed to parse expression value after operator: {:#?}", op_stack.last());
        };

        expr_stack.push(expr);
    }

    if op_stack.is_empty() {
        assert_eq!(expr_stack.len(), 1, "Expression stack should have exactly one element");

        Some(expr_stack.pop().unwrap())
    } else {
        Some(UVExpr::Complex {
            op_stack: op_stack,
            expr_stack: expr_stack
        })
    }
}

pub(crate) fn parse_expr_val(data: &mut ParserData) -> Option<UVExpr> {
    let mut acc = match data.toks.next()? {
        Token::Operator(op) => {
            if let Some(op) = tok_to_unop(op.clone()) {
                return Some(UVExpr::UnOp {
                    operator: op,
                    operand: Box::new(parse_expr_val(data)?)
                });
            }

            None
        },
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
    if let Some(Token::Identifier(name)) = data.toks.next() {
        return Some(name.clone());
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