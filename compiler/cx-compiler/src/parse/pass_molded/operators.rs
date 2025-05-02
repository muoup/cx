use crate::lex::token::OperatorType;
use crate::log_error;
use crate::parse::pass_molded::{CXBinOp, CXUnOp};
use crate::parse::pass_unverified::UVOp;

pub(crate) fn op_precedence(op: UVOp) -> Option<u8> {
    Some(
        match op {
            UVOp::UnOpPost(OperatorType::Plus) => 2,
            UVOp::UnOpPost(OperatorType::Minus) => 2,
            UVOp::UnOpPre(OperatorType::LNot) => 2,
            UVOp::UnOpPre(OperatorType::BNot) => 2,
            UVOp::UnOpPre(OperatorType::Asterisk) => 2,
            UVOp::UnOpPre(OperatorType::BAnd) => 2,
            UVOp::BinOp(OperatorType::ScopeRes) => 2,
            UVOp::BinOp(OperatorType::Access) => 2,

            UVOp::BinOp(OperatorType::Asterisk) => 3,
            UVOp::BinOp(OperatorType::Slash) => 3,
            UVOp::BinOp(OperatorType::Percent) => 3,

            UVOp::BinOp(OperatorType::Plus) => 4,
            UVOp::BinOp(OperatorType::Minus) => 4,

            UVOp::Assignment(_) => 14,

            _ => log_error!("get_precedence: Unhandled operator precedence for {:?}", op),
        }
    )
}

pub(crate) fn tok_cx_binop(op: OperatorType) -> Option<CXBinOp> {
    Some(
        match op {
            OperatorType::Plus          => CXBinOp::Add,
            OperatorType::Minus         => CXBinOp::Subtract,
            OperatorType::Asterisk      => CXBinOp::Multiply,
            OperatorType::Slash         => CXBinOp::Divide,
            OperatorType::Percent       => CXBinOp::Modulus,

            OperatorType::Less          => CXBinOp::Less,
            OperatorType::Greater       => CXBinOp::Greater,
            OperatorType::LessEqual     => CXBinOp::LessEqual,
            OperatorType::GreaterEqual  => CXBinOp::GreaterEqual,

            _ => log_error!("tok_cx_binop: Unhandled operator type {:?}", op),
        }
    )
}

pub(crate) fn tok_cx_unop(op: OperatorType) -> Option<CXUnOp> {
    Some(
        match op {
            OperatorType::Asterisk      => CXUnOp::Dereference,
            OperatorType::BAnd          => CXUnOp::AddressOf,
            OperatorType::Minus         => CXUnOp::Negative,
            OperatorType::BNot          => CXUnOp::BNot,
            OperatorType::LNot          => CXUnOp::LNot,
            OperatorType::Access        => CXUnOp::ArrayIndex,
            OperatorType::ScopeRes      => CXUnOp::InitializerIndex,

            _ => log_error!("tok_cx_unop: Unhandled operator type {:?}", op),
        }
    )
}