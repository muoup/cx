use crate::parse::ParserData;
use cx_lexer_data::token::{OperatorType, PunctuatorType, TokenKind};
use cx_parsing_data::ast::{CXBinOp, CXUnOp};
use cx_parsing_data::{assert_token_matches, next_kind};
use cx_util::CXResult;

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
        CXBinOp::Access | CXBinOp::ScopeRes | CXBinOp::MethodCall | CXBinOp::ArrayIndex | CXBinOp::Is => 1,
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
        CXUnOp::PostIncrement(_) => 1,
        
        CXUnOp::PreIncrement(_) => 2,
        CXUnOp::BNot => 2,
        CXUnOp::LNot => 2,
        CXUnOp::Negative => 2,
        CXUnOp::Dereference => 2,
        CXUnOp::AddressOf => 2,
       
        CXUnOp::ExplicitCast(_) => 3,
    }
}

pub(crate) fn parse_prefix_unop(data: &mut ParserData) -> CXResult<Option<CXUnOp>> {
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

            let Some((None, _type)) = parse_initializer(data).ok() else {
                data.tokens.index = pre_index;
                return Ok(None);
            };

            assert_token_matches!(
                data.tokens,
                TokenKind::Punctuator(PunctuatorType::CloseParen)
            );

            Some(CXUnOp::ExplicitCast(_type))
        }

        _ => {
            data.tokens.back();
            None
        }
    })
}

pub(crate) fn parse_postfix_unop(data: &mut ParserData) -> Option<CXUnOp> {
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
        OperatorType::ScopeRes => CXBinOp::ScopeRes,
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

        _ => return log_parse_error!(data, "Invalid binary operator: {:?}", op),
    })
}

pub(crate) fn parse_binop(data: &mut ParserData) -> CXResult<CXBinOp> {
    Ok(match next_kind!(data.tokens).cloned() {
        Ok(TokenKind::Operator(OperatorType::Comma)) => {
            if data.get_comma_mode() {
                op_to_binop(data, OperatorType::Comma)?
            } else {
                data.tokens.back();
                return log_parse_error!(data, "Comma operator not allowed in this context");
            }
        }
        Ok(TokenKind::Operator(op)) => op_to_binop(data, op)?,
        Ok(TokenKind::Punctuator(punc)) => {
            data.tokens.back();
            match punc {
                PunctuatorType::OpenBracket => CXBinOp::ArrayIndex,
                PunctuatorType::OpenParen => CXBinOp::MethodCall,

                _ => return log_parse_error!(data, "Invalid binary operator: {:?}", punc),
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
            return log_parse_error!(data, "Expected binary operator");
        }
    })
}
