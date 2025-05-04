use crate::lex::token::{OperatorType, PunctuatorType, Token};
use crate::parse::parser::ParserData;
use crate::parse::pass_ast::{CXBinOp, CXExpr, CXUnOp};

pub(crate) fn binop_prec(op: CXBinOp) -> u8 {
    match op {
        CXBinOp::Access | CXBinOp::MethodCall => 1,
        CXBinOp::Multiply | CXBinOp::Divide | CXBinOp::Modulus => 4,
        CXBinOp::Add | CXBinOp::Subtract => 5,

        CXBinOp::Assign(_) => 14,

        CXBinOp::Comma => 15,

        _ => todo!("binop_prec {op:?}")
    }
}

pub(crate) fn parse_pre_unop(data: &mut ParserData) -> Option<CXUnOp> {
    Some(
        match data.toks.next()? {
            Token::Operator(op) => match op {
                OperatorType::Ampersand     => CXUnOp::AddressOf,
                OperatorType::Asterisk      => CXUnOp::Dereference,

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

            _ => todo!("op_to_binop: {op:?}")
        }
    )
}

pub(crate) fn parse_binop(data: &mut ParserData) -> Option<CXBinOp> {
    Some(
        match data.toks.next() {
            Some(Token::Operator(op)) => op_to_binop(op.clone())?,
            Some(Token::Punctuator(punc)) => {
                let punc = punc.clone();
                data.toks.back();
                match punc {
                    PunctuatorType::OpenBrace   => CXBinOp::ArrayIndex,
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

pub(crate) fn comma_separated_owned(expr: CXExpr) -> Vec<CXExpr> {
    let CXExpr::BinOp { lhs, rhs, op: CXBinOp::Comma } = expr else {
        return vec![expr];
    };

    let mut lresults = comma_separated_owned(*lhs);
    let rresults = comma_separated_owned(*rhs);

    lresults.extend(rresults);
    lresults
}

pub(crate) fn comma_separated_mut(expr: &mut CXExpr) -> Vec<&mut CXExpr> {
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

pub(crate) fn comma_separated(expr: &CXExpr) -> Vec<&CXExpr> {
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