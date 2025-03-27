use crate::lex::token::{KeywordType, OperatorType, PunctuatorType, Token};
use crate::{assert_token_matches, log_error};
use crate::parse::expression::parse_identifier;
use crate::parse::parser::ParserData;
use crate::parse::unverified::{UVBinOp, UVExpr, UVUnOp};
use crate::parse::unverified::operators::{tok_to_binop, tok_to_unop};

pub(crate) fn parse_expr(data: &mut ParserData) -> Option<UVExpr> {
    let mut op_stack = Vec::new();
    let mut expr_stack = Vec::new();

    let Some(expr) = parse_expr_val(data) else {
        return None
    };

    expr_stack.push(expr);

    while !matches!(data.toks.peek(), None) {
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
                }
            }

            break;
        }

        let Some(expr) = parse_expr_val(data) else {
            log_error!("Failed to parse expression value after operator: {:#?}", data.toks.peek());
        };

        expr_stack.push(expr);
    }

    if op_stack.is_empty() {
        assert_eq!(expr_stack.len(), 1, "Expression stack should have exactly one element");

        Some(expr_stack.pop().unwrap())
    } else {
        Some(UVExpr::Complex {
            operator_stack: op_stack,
            expression_stack: expr_stack
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

        Token::Keyword(KeywordType::Return) => {
            let Some(expr) = parse_expr(data) else {
                return None
            };

            Some(UVExpr::Identifier("return".to_string())) // Placeholder for return statement
        }

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
