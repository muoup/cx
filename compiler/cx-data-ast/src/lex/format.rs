use std::fmt::{Display, Formatter};
use crate::lex::token::{IntrinsicType, TokenKind, Token};

impl Display for Token {
    fn fmt(&self, f: &mut Formatter<'_>) -> std::fmt::Result {
        write!(f, "{}", self.kind)
    }
}

impl Display for TokenKind {
    fn fmt(&self, f: &mut Formatter<'_>) -> std::fmt::Result {
        match self {
            TokenKind::Identifier(name) => write!(f, "{}", name),
            TokenKind::Intrinsic(intrin) => write!(f, "{}", intrin),

            _ => write!(f, "{:?}", self),
        }
    }
}

impl Display for IntrinsicType {
    fn fmt(&self, f: &mut Formatter<'_>) -> std::fmt::Result {
        match self {
            IntrinsicType::Int => write!(f, "int"),
            IntrinsicType::Float => write!(f, "float"),
            IntrinsicType::Double => write!(f, "double"),
            IntrinsicType::Void => write!(f, "void"),
            IntrinsicType::Bool => write!(f, "bool"),

            _ => write!(f, "{:?}", self),
        }
    }
}