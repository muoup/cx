use cx_parsing_data::ast::{CXBinOp, CXUnOp};
use crate::parse::ParserData;
use cx_parsing_data::{assert_token_matches, next_kind};
use cx_lexer_data::token::{OperatorType, PunctuatorType, TokenKind};
use cx_util::{CXError, CXResult};

use crate::parse::expressions::is_type_decl;
use crate::parse::types::parse_initializer;

#[derive(Debug, Clone)]
pub(crate) enum PrecOperator {
    BinOp(CXBinOp),
    UnOp(CXUnOp),
}

impl PrecOperator {
    pub(crate) fn get_precedence(&self) -> u8 {
        match self {
            PrecOperator::BinOp(op) => binop_prec(op.clone()),
            PrecOperator::UnOp(op) => unop_prec(op.clone()),
        }
    }
}

pub(crate) fn binop_prec(op: CXBinOp) -> u8 {
    match op {
        CXBinOp::Access | CXBinOp::MethodCall | CXBinOp::ArrayIndex | CXBinOp::Is => 1,
        CXBinOp::Multiply | CXBinOp::Divide | CXBinOp::Modulus => 4,
        CXBinOp::Add | CXBinOp::Subtract => 5,

        CXBinOp::LShift | CXBinOp::RShift => 6,
        CXBinOp::Less | CXBinOp::Greater | CXBinOp::LessEqual | CXBinOp::GreaterEqual => 7,

        CXBinOp::BitAnd => 8,
        CXBinOp::BitXor => 9,
        CXBinOp::BitOr => 10,

        CXBinOp::Equal | CXBinOp::NotEqual => 10,

        CXBinOp::LAnd => 14,
        CXBinOp::LOr => 15,
        CXBinOp::Assign(_) => 16,

        CXBinOp::Comma => 17,
    }
}

pub(crate) fn unop_prec(op: CXUnOp) -> u8 {
    match op {
        CXUnOp::PostIncrement(_) => 2,
        CXUnOp::BNot => 2,

        CXUnOp::LNot => 3,
        CXUnOp::Negative => 3,
        CXUnOp::AddressOf => 3,
        CXUnOp::Dereference => 3,
        CXUnOp::PreIncrement(_) => 3,
        CXUnOp::ExplicitCast(_) => 3,
    }
}

pub(crate) fn parse_pre_unop(data: &mut ParserData) -> CXResult<Option<CXUnOp>> {
    Ok(match &next_kind!(data.tokens)? {
        TokenKind::Operator(op) => match op {
            OperatorType::Ampersand => Some(CXUnOp::AddressOf),
            OperatorType::Asterisk => Some(CXUnOp::Dereference),
            OperatorType::Increment => Some(CXUnOp::PreIncrement(1)),
            OperatorType::Decrement => Some(CXUnOp::PreIncrement(-1)),
            OperatorType::Minus => Some(CXUnOp::Negative),
            OperatorType::Exclamation => Some(CXUnOp::LNot),

            _ => {
                data.tokens.back();
                None
            }
        },

        // Maybe a type cast
        TokenKind::Punctuator(PunctuatorType::OpenParen) => {
            let pre_index = data.tokens.index - 1;

            if !is_type_decl(data) {
                data.tokens.index = pre_index;
                return Ok(None);
            }

            let Some((None, type_)) = parse_initializer(data).ok() else {
                data.tokens.index = pre_index;
                return Ok(None);
            };

            assert_token_matches!(
                data.tokens,
                TokenKind::Punctuator(PunctuatorType::CloseParen)
            );

            Some(CXUnOp::ExplicitCast(type_))
        }

        _ => {
            data.tokens.back();
            None
        }
    })
}

pub(crate) fn parse_post_unop(data: &mut ParserData) -> Option<CXUnOp> {
    Some(match &data.tokens.next()?.kind {
        TokenKind::Operator(op) => match op {
            OperatorType::Increment => CXUnOp::PostIncrement(1),
            OperatorType::Decrement => CXUnOp::PostIncrement(-1),

            _ => {
                data.tokens.back();
                return None;
            }
        },

        _ => {
            data.tokens.back();
            return None;
        }
    })
}

fn op_to_binop(data: &ParserData, op: OperatorType) -> CXResult<CXBinOp> {
    Ok(match op {
        OperatorType::Plus => CXBinOp::Add,
        OperatorType::Minus => CXBinOp::Subtract,
        OperatorType::Asterisk => CXBinOp::Multiply,
        OperatorType::Slash => CXBinOp::Divide,
        OperatorType::Percent => CXBinOp::Modulus,

        OperatorType::Access => CXBinOp::Access,
        OperatorType::Comma => CXBinOp::Comma,

        OperatorType::Equal => CXBinOp::Equal,
        OperatorType::NotEqual => CXBinOp::NotEqual,
        OperatorType::Less => CXBinOp::Less,
        OperatorType::Greater => CXBinOp::Greater,
        OperatorType::LessEqual => CXBinOp::LessEqual,
        OperatorType::GreaterEqual => CXBinOp::GreaterEqual,

        OperatorType::Ampersand => CXBinOp::LAnd,
        OperatorType::DoubleBar => CXBinOp::LOr,
        OperatorType::DoubleAmpersand => CXBinOp::LAnd,

        OperatorType::DoubleLT => CXBinOp::LShift,
        OperatorType::DoubleGT => CXBinOp::RShift,

        OperatorType::Is => CXBinOp::Is,

        _ => return log_parse_error!(data, 
            "Invalid binary operator: {:?}",
            op
        ),
    })
}

pub(crate) fn parse_binop(data: &mut ParserData) -> CXResult<CXBinOp> {
    Ok(match next_kind!(data.tokens) {
        Ok(TokenKind::Operator(OperatorType::Comma)) => {
            if data.get_comma_mode() {
                op_to_binop(data, OperatorType::Comma)?
            } else {
                data.tokens.back();
                return Err(CXError::new("Comma operator not allowed in this context"));
            }
        }
        Ok(TokenKind::Operator(op)) => op_to_binop(data, op)?,
        Ok(TokenKind::Punctuator(punc)) => {
            let punc = punc;
            data.tokens.back();
            match punc {
                PunctuatorType::OpenBracket => CXBinOp::ArrayIndex,
                PunctuatorType::OpenParen => CXBinOp::MethodCall,

                _ => return log_parse_error!(data,
                    "Invalid binary operator: {:?}",
                    punc
                ),
            }
        }
        Ok(TokenKind::Assignment(op)) => {
            let op = match op {
                Some(op) => Some(Box::new(op_to_binop(data, op)?)),
                None => None,
            };

            CXBinOp::Assign(op)
        }

        _ => {
            data.tokens.back();
            return Err(CXError::new("Expected binary operator"));
        }
    })
}
