use crate::parse::ParserData;
use crate::{log::parse_point_error, next_kind};
use cx_hir::ast::expression::{HIRBinOp, HIRUnOp};
use cx_log::CXResult;
use cx_tokens::token::{OperatorType, PunctuatorType, TokenKind};
use cx_tokens::{operator, punctuator};

use crate::parse::expressions::parse_pattern;
use crate::parse::types::{is_type_decl, parse_initializer};

#[derive(Debug, Clone)]
pub(crate) enum PrecOperator {
    BinOp(HIRBinOp),
    UnOp(HIRUnOp),
}

impl PrecOperator {
    pub(crate) fn get_precedence(&self) -> u8 {
        match self {
            PrecOperator::BinOp(op) => binop_prec(op.clone()),
            PrecOperator::UnOp(op) => unop_prec(op.clone()),
        }
    }
}

pub(crate) fn binop_prec(op: HIRBinOp) -> u8 {
    match op {
        HIRBinOp::Access | HIRBinOp::MethodCall | HIRBinOp::ArrayIndex => 1,
        HIRBinOp::Pipe | HIRBinOp::BackwardPipe => 2,

        HIRBinOp::Multiply | HIRBinOp::Divide | HIRBinOp::Modulus => 4,
        HIRBinOp::Add | HIRBinOp::Subtract => 5,

        HIRBinOp::LShift | HIRBinOp::RShift => 6,
        HIRBinOp::Less | HIRBinOp::Greater | HIRBinOp::LessEqual | HIRBinOp::GreaterEqual => 7,

        HIRBinOp::BitAnd => 8,
        HIRBinOp::BitXor => 9,
        HIRBinOp::BitOr => 10,

        HIRBinOp::Equal | HIRBinOp::NotEqual => 10,

        HIRBinOp::LAnd => 14,
        HIRBinOp::LOr => 15,

        HIRBinOp::Assign(_) => 17,

        HIRBinOp::Comma => 18,
    }
}

pub(crate) fn unop_prec(op: HIRUnOp) -> u8 {
    match op {
        HIRUnOp::PostIncrement(_) => 1,
        HIRUnOp::Move => 1,

        HIRUnOp::PreIncrement(_) => 2,
        HIRUnOp::BNot => 2,
        HIRUnOp::LNot => 2,
        HIRUnOp::Negative => 2,
        HIRUnOp::Dereference => 2,
        HIRUnOp::AddressOf => 2,

        HIRUnOp::Is(_) => 3,
        HIRUnOp::ExplicitCast(_) => 3,
    }
}

pub(crate) fn parse_prefix_unop(data: &mut ParserData) -> CXResult<Option<HIRUnOp>> {
    Ok(match &next_kind!(data.tokens)? {
        TokenKind::Operator(op) => match op {
            OperatorType::Ampersand => Some(HIRUnOp::AddressOf),
            OperatorType::Asterisk => Some(HIRUnOp::Dereference),
            OperatorType::Increment => Some(HIRUnOp::PreIncrement(1)),
            OperatorType::Decrement => Some(HIRUnOp::PreIncrement(-1)),
            OperatorType::Minus => Some(HIRUnOp::Negative),
            OperatorType::Exclamation => Some(HIRUnOp::LNot),
            OperatorType::Tilda => Some(HIRUnOp::BNot),
            OperatorType::Move => Some(HIRUnOp::Move),

            _ => {
                data.tokens.back();
                None
            }
        },

        // Maybe a type cast
        punctuator!(OpenParen) => {
            let pre_index = data.tokens.index - 1;

            if !is_type_decl(data)? {
                data.tokens.index = pre_index;
                return Ok(None);
            }

            let Some((None, _type, _)) = parse_initializer(data).ok() else {
                data.tokens.index = pre_index;
                return Ok(None);
            };

            if !matches!(
                data.tokens.next().map(|t| &t.kind),
                Some(punctuator!(CloseParen))
            ) {
                data.tokens.index = pre_index;
                return Ok(None);
            }

            if matches!(
                data.tokens.peek().map(|token| &token.kind),
                Some(
                    punctuator!(Semicolon)
                        | punctuator!(CloseParen)
                        | punctuator!(CloseBracket)
                        | punctuator!(CloseBrace)
                        | operator!(Comma)
                )
            ) {
                data.tokens.index = pre_index;
                return Ok(None);
            }

            Some(HIRUnOp::ExplicitCast(_type))
        }

        _ => {
            data.tokens.back();
            None
        }
    })
}

pub(crate) fn parse_postfix_unop(data: &mut ParserData) -> CXResult<Option<HIRUnOp>> {
    let Some(token) = data.tokens.next() else {
        return Ok(None);
    };

    Ok(match &token.kind {
        operator!(Is) => {
            let pattern = parse_pattern(data)?;

            Some(HIRUnOp::Is(Box::new(pattern)))
        }

        TokenKind::Operator(op) => match op {
            OperatorType::Increment => Some(HIRUnOp::PostIncrement(1)),
            OperatorType::Decrement => Some(HIRUnOp::PostIncrement(-1)),

            _ => {
                data.tokens.back();
                None
            }
        },

        _ => {
            data.tokens.back();
            None
        }
    })
}

fn op_to_binop(data: &ParserData, op: OperatorType) -> CXResult<HIRBinOp> {
    Ok(match op {
        OperatorType::Plus => HIRBinOp::Add,
        OperatorType::Minus => HIRBinOp::Subtract,
        OperatorType::Asterisk => HIRBinOp::Multiply,
        OperatorType::Slash => HIRBinOp::Divide,
        OperatorType::Percent => HIRBinOp::Modulus,

        OperatorType::Access => HIRBinOp::Access,
        OperatorType::Comma => HIRBinOp::Comma,

        OperatorType::Equal => HIRBinOp::Equal,
        OperatorType::NotEqual => HIRBinOp::NotEqual,
        OperatorType::Less => HIRBinOp::Less,
        OperatorType::Greater => HIRBinOp::Greater,
        OperatorType::LessEqual => HIRBinOp::LessEqual,
        OperatorType::GreaterEqual => HIRBinOp::GreaterEqual,
        OperatorType::LShift => HIRBinOp::LShift,
        OperatorType::RShift => HIRBinOp::RShift,

        OperatorType::Ampersand => HIRBinOp::BitAnd,
        OperatorType::Bar => HIRBinOp::BitOr,
        OperatorType::Caret => HIRBinOp::BitXor,
        OperatorType::DoubleBar => HIRBinOp::LOr,
        OperatorType::DoubleAmpersand => HIRBinOp::LAnd,

        OperatorType::Pipe => HIRBinOp::Pipe,
        OperatorType::BackwardPipe => HIRBinOp::BackwardPipe,

        _ => return parse_point_error(&data.tokens, format!("Invalid binary operator: {:?}", op)),
    })
}

pub(crate) fn parse_binop(data: &mut ParserData) -> CXResult<HIRBinOp> {
    Ok(match next_kind!(data.tokens).cloned() {
        Ok(TokenKind::Operator(OperatorType::Comma)) => {
            if data.get_comma_mode() {
                op_to_binop(data, OperatorType::Comma)?
            } else {
                data.tokens.back();
                return parse_point_error(
                    &data.tokens,
                    "Invalid token: expected binary operator, found comma".to_string(),
                );
            }
        }
        // Handle >> as shift operator (two consecutive Greater tokens)
        Ok(TokenKind::Operator(OperatorType::Greater)) => {
            if let Some(next) = data.tokens.peek() {
                if matches!(next.kind, TokenKind::Operator(OperatorType::Greater)) {
                    data.tokens.next(); // consume the second Greater
                    HIRBinOp::RShift
                } else {
                    HIRBinOp::Greater
                }
            } else {
                HIRBinOp::Greater
            }
        }
        // Handle << as shift operator (two consecutive Less tokens)
        Ok(TokenKind::Operator(OperatorType::Less)) => {
            if let Some(next) = data.tokens.peek() {
                if matches!(next.kind, TokenKind::Operator(OperatorType::Less)) {
                    data.tokens.next(); // consume the second Less
                    HIRBinOp::LShift
                } else {
                    HIRBinOp::Less
                }
            } else {
                HIRBinOp::Less
            }
        }
        Ok(TokenKind::Operator(op)) => op_to_binop(data, op)?,
        Ok(TokenKind::Punctuator(punc)) => {
            data.tokens.back();
            match punc {
                PunctuatorType::OpenBracket => HIRBinOp::ArrayIndex,
                PunctuatorType::OpenParen => HIRBinOp::MethodCall,

                _ => {
                    return parse_point_error(
                        &data.tokens,
                        format!("Invalid binary operator: {:?}", punc),
                    )
                }
            }
        }
        Ok(TokenKind::Assignment(op)) => {
            let op = match op {
                Some(op) => Some(Box::new(op_to_binop(data, op)?)),
                None => None,
            };

            HIRBinOp::Assign(op)
        }

        _ => {
            data.tokens.back();
            return parse_point_error(&data.tokens, "Expected binary operator".to_string());
        }
    })
}
