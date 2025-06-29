use cx_data_ast::{assert_token_matches, next_kind};
use cx_data_ast::lex::token::{OperatorType, PunctuatorType, TokenKind};
use cx_data_ast::parse::ast::{CXBinOp, CXExpr, CXExprKind, CXUnOp};
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
        CXUnOp::PostIncrement(_) => 2,
        
        CXUnOp::LNot => 3,
        CXUnOp::Negative => 3,
        CXUnOp::AddressOf => 3,
        CXUnOp::Dereference => 3,
        CXUnOp::PreIncrement(_) => 3,
        CXUnOp::ExplicitCast(_) => 3,

        _ => todo!("unop_prec {op:?}")
    }
}

pub(crate) fn parse_pre_unop(data: &mut ParserData) -> Option<CXUnOp> {
    Some(
        match &data.toks.next()?.kind {
            TokenKind::Operator(op) => match op {
                OperatorType::BAnd          => CXUnOp::AddressOf,
                OperatorType::Asterisk      => CXUnOp::Dereference,
                OperatorType::Increment     => CXUnOp::PreIncrement(1),
                OperatorType::Decrement     => CXUnOp::PreIncrement(-1),
                OperatorType::Minus         => CXUnOp::Negative,
                OperatorType::LNot          => CXUnOp::LNot,

                _ => {
                    data.toks.back();
                    return None;
                }
            },

            // Maybe a type cast
            TokenKind::Punctuator(PunctuatorType::OpenParen) => {
                let pre_index = data.toks.index - 1;

                if !is_type_decl(data) {
                    data.toks.index = pre_index;
                    return None;
                }

                let Some((None, type_)) = parse_initializer(data) else {
                    data.toks.index = pre_index;
                    return None;
                };

                assert_token_matches!(data, TokenKind::Punctuator(PunctuatorType::CloseParen));

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
        match &data.toks.next()?.kind {
            TokenKind::Operator(op) => match op {
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
            OperatorType::LAnd              => CXBinOp::LAnd,
            
            OperatorType::LShift            => CXBinOp::LShift,
            OperatorType::RShift            => CXBinOp::RShift,

            _ => todo!("op_to_binop: {op:?}")
        }
    )
}

pub(crate) fn parse_binop(data: &mut ParserData) -> Option<CXBinOp> {
    Some(
        match next_kind!(data) {
            Some(TokenKind::Operator(OperatorType::Comma)) => {
                if data.get_comma_mode() {
                    op_to_binop(OperatorType::Comma)?
                } else {
                    return None;
                }
            },
            Some(TokenKind::Operator(op)) => op_to_binop(op.clone())?,
            Some(TokenKind::Punctuator(punc)) => {
                let punc = punc.clone();
                data.toks.back();
                match punc {
                    PunctuatorType::OpenBracket => CXBinOp::ArrayIndex,
                    PunctuatorType::OpenParen   => CXBinOp::MethodCall,

                    _ => return None
                }
            },
            Some(TokenKind::Assignment(op)) => {
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

pub fn comma_separated_mut<'a>(expr: &'a mut CXExpr) -> Vec<&'a mut CXExpr> {
    if matches!(expr.kind, CXExprKind::Unit) {
        return vec![];
    }
    
    if !matches!(&expr.kind, CXExprKind::BinOp { op: CXBinOp::Comma, .. }) {
        return vec![expr];
    }

    let CXExprKind::BinOp { lhs, rhs, op: CXBinOp::Comma } = &mut expr.kind else {
        unreachable!()
    };

    let mut lresults : Vec<&'a mut CXExpr> = comma_separated_mut(lhs.as_mut());
    let rresults : Vec<&'a mut CXExpr> = comma_separated_mut(rhs.as_mut());

    lresults.extend(rresults);
    lresults
}

pub fn comma_separated(expr: &CXExpr) -> Vec<&CXExpr> {
    if matches!(expr.kind, CXExprKind::Unit) {
        return vec![];
    }

    let CXExprKind::BinOp { lhs, rhs, op: CXBinOp::Comma } = &expr.kind else {
        return vec![expr];
    };

    let mut lresults = comma_separated(lhs.as_ref());
    let rresults = comma_separated(rhs.as_ref());

    lresults.extend(rresults);
    lresults
}