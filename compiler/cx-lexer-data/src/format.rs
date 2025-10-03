use crate::token::{IntrinsicType, Token, TokenKind};
use std::fmt::{Display, Formatter};

impl Display for Token {
    fn fmt(&self, f: &mut Formatter<'_>) -> std::fmt::Result {
        write!(f, "{}", self.kind)
    }
}

impl Display for TokenKind {
    fn fmt(&self, f: &mut Formatter<'_>) -> std::fmt::Result {
        match self {
            TokenKind::Identifier(name) => write!(f, "{name}"),
            TokenKind::Intrinsic(intrin) => write!(f, "{intrin}"),
            TokenKind::Keyword(keyword) => write!(f, "{}", format!("{keyword:?}").to_lowercase()),

            _ => write!(f, "{self:?}"),
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

            _ => write!(f, "{self:?}"),
        }
    }
}
