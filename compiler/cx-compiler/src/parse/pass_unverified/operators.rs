use crate::lex::token::{OperatorType, Token};
use crate::parse::pass_unverified::{UVBinOp, UVUnOp};

pub(crate) fn tok_to_unop(op: OperatorType) -> Option<UVUnOp> {
    Some (
        match op {
            OperatorType::Minus     => UVUnOp::Negative,
            OperatorType::Asterisk  => UVUnOp::Dereference,
            OperatorType::LNot      => UVUnOp::LNot,
            OperatorType::BNot      => UVUnOp::BNot,
            OperatorType::Access    => UVUnOp::UnaryAccess,

            _ => return None
        }
    )
}

pub(crate) fn tok_to_binop(op: Token) -> Option<UVBinOp> {
    Some (
        match op {
            Token::Operator(op) => match op {
                OperatorType::Plus          => UVBinOp::Add,
                OperatorType::Minus         => UVBinOp::Subtract,
                OperatorType::Asterisk      => UVBinOp::Multiply,
                OperatorType::Slash         => UVBinOp::Divide,
                OperatorType::Modulo        => UVBinOp::Modulus,
                OperatorType::Less          => UVBinOp::Less,
                OperatorType::Greater       => UVBinOp::Greater,
                OperatorType::LessEqual     => UVBinOp::LessEqual,
                OperatorType::GreaterEqual  => UVBinOp::GreaterEqual,
                OperatorType::Access        => UVBinOp::Access,
                OperatorType::Comma         => UVBinOp::Comma,

                _ => return None
            },

            Token::Assignment(op) =>
                UVBinOp::Assignment(
                    op.map(|op| Box::new(tok_to_binop(Token::Operator(op)).unwrap()) )
                ),

            _ => return None
        }
    )
}
