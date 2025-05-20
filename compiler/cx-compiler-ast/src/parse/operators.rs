use std::clone;
use cx_data_ast::assert_token_matches;
use cx_data_ast::lex::token::{OperatorType, PunctuatorType, Token};
use cx_data_ast::parse::ast::{CXBinOp, CXExpr, CXUnOp};
use cx_data_ast::parse::parser::ParserData;
use crate::parse::typing::{is_type_decl, parse_initializer};

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

    pub(crate) fn is_binop(&self) -> bool {
        matches!(self, PrecOperator::BinOp(_))
    }

    pub(crate) fn is_unop(&self) -> bool {
        matches!(self, PrecOperator::UnOp(_))
    }
}

pub(crate) fn binop_prec(op: CXBinOp) -> u8 {
    match op {
        CXBinOp::Access | CXBinOp::MethodCall | CXBinOp::ArrayIndex => 1,
        CXBinOp::Multiply | CXBinOp::Divide | CXBinOp::Modulus => 4,
        CXBinOp::Add | CXBinOp::Subtract => 5,

        CXBinOp::LShift | CXBinOp::RShift => 6,
        CXBinOp::Less | CXBinOp::Greater | CXBinOp::LessEqual | CXBinOp::GreaterEqual => 7,

        CXBinOp::Equal | CXBinOp::NotEqual => 10,

        CXBinOp::LAnd => 14,
        CXBinOp::LOr => 15,
        CXBinOp::Assign(_) => 16,

        CXBinOp::Comma => 17,

        _ => todo!("binop_prec {op:?}")
    }
}

pub(crate) fn unop_prec(op: CXUnOp) -> u8 {
    match op {
        CXUnOp::Negative => 2,
        
        CXUnOp::AddressOf => 3,
        CXUnOp::Dereference => 3,
        CXUnOp::PreIncrement(_) => 3,
        CXUnOp::ExplicitCast(_) => 3,

        CXUnOp::PostIncrement(_) => 4,

        _ => todo!("unop_prec {op:?}")
    }
}

pub(crate) fn parse_pre_unop(data: &mut ParserData) -> Option<CXUnOp> {
    Some(
        match data.toks.next()? {
            Token::Operator(op) => match op {
                OperatorType::BAnd          => CXUnOp::AddressOf,
                OperatorType::Asterisk      => CXUnOp::Dereference,
                OperatorType::Increment     => CXUnOp::PreIncrement(1),
                OperatorType::Decrement     => CXUnOp::PreIncrement(-1),
                OperatorType::Minus         => CXUnOp::Negative,

                _ => {
                    data.toks.back();
                    return None;
                }
            },

            // Maybe a type cast
            Token::Punctuator(PunctuatorType::OpenParen) => {
                let pre_index = data.toks.index - 1;

                if !is_type_decl(data) {
                    data.toks.index = pre_index;
                    return None;
                }

                let Some((None, type_)) = parse_initializer(data) else {
                    data.toks.index = pre_index;
                    return None;
                };

                assert_token_matches!(data, Token::Punctuator(PunctuatorType::CloseParen));

                return Some(CXUnOp::ExplicitCast(type_));
            },

            _ => {
                data.toks.back();
                return None;
            }
        }
    )
}

pub(crate) fn parse_post_unop(data: &mut ParserData) -> Option<CXUnOp> {
    Some(
        match data.toks.next()? {
            Token::Operator(op) => match op {
                OperatorType::Increment     => CXUnOp::PostIncrement(1),
                OperatorType::Decrement     => CXUnOp::PostIncrement(-1),

                _ => {
                    data.toks.back();
                    return None;
                }
            },

            _ => {
                data.toks.back();
                return None;
            }
        }
    )
}

fn op_to_binop(op: OperatorType) -> Option<CXBinOp> {
    Some(
        match op {
            OperatorType::Plus              => CXBinOp::Add,
            OperatorType::Minus             => CXBinOp::Subtract,
            OperatorType::Asterisk          => CXBinOp::Multiply,
            OperatorType::Slash             => CXBinOp::Divide,
            OperatorType::Percent           => CXBinOp::Modulus,

            OperatorType::Access            => CXBinOp::Access,
            OperatorType::Comma             => CXBinOp::Comma,

            OperatorType::Equal             => CXBinOp::Equal,
            OperatorType::NotEqual          => CXBinOp::NotEqual,
            OperatorType::Less              => CXBinOp::Less,
            OperatorType::Greater           => CXBinOp::Greater,
            OperatorType::LessEqual         => CXBinOp::LessEqual,
            OperatorType::GreaterEqual      => CXBinOp::GreaterEqual,

            OperatorType::BAnd              => CXBinOp::LAnd,
            OperatorType::LOr               => CXBinOp::LOr,

            _ => todo!("op_to_binop: {op:?}")
        }
    )
}

pub(crate) fn parse_binop(data: &mut ParserData) -> Option<CXBinOp> {
    Some(
        match data.toks.next() {
            Some(Token::Operator(OperatorType::Comma)) => {
                if data.get_comma_mode() {
                    op_to_binop(OperatorType::Comma)?
                } else {
                    return None;
                }
            },
            Some(Token::Operator(op)) => op_to_binop(op.clone())?,
            Some(Token::Punctuator(punc)) => {
                let punc = punc.clone();
                data.toks.back();
                match punc {
                    PunctuatorType::OpenBracket => CXBinOp::ArrayIndex,
                    PunctuatorType::OpenParen   => CXBinOp::MethodCall,

                    _ => return None
                }
            },
            Some(Token::Assignment(op)) => {
                let op = match op {
                    Some(op) => Some(Box::new(op_to_binop(op.clone())?)),
                    None => None
                };

                CXBinOp::Assign(op)
            }

            _ => {
                data.back();
                return None;
            }
        }
    )
}

pub fn comma_separated_owned(expr: CXExpr) -> Vec<CXExpr> {
    let CXExpr::BinOp { lhs, rhs, op: CXBinOp::Comma } = expr else {
        return vec![expr];
    };

    let mut lresults = comma_separated_owned(*lhs);
    let rresults = comma_separated_owned(*rhs);

    lresults.extend(rresults);
    lresults
}

pub fn comma_separated_mut(expr: &mut CXExpr) -> Vec<&mut CXExpr> {
    if matches!(expr, CXExpr::Unit) {
        return vec![];
    }

    let CXExpr::BinOp { lhs, rhs, op: CXBinOp::Comma } = expr else {
        return vec![expr];
    };

    let mut lresults = comma_separated_mut(lhs.as_mut());
    let rresults = comma_separated_mut(rhs.as_mut());

    lresults.extend(rresults);
    lresults
}

pub fn comma_separated(expr: &CXExpr) -> Vec<&CXExpr> {
    if matches!(expr, CXExpr::Unit) {
        return vec![];
    }

    let CXExpr::BinOp { lhs, rhs, op: CXBinOp::Comma } = expr else {
        return vec![expr];
    };

    let mut lresults = comma_separated(lhs.as_ref());
    let rresults = comma_separated(rhs.as_ref());

    lresults.extend(rresults);
    lresults
}