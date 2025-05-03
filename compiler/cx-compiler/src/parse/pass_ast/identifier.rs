use std::fmt::{Display, Formatter};
use crate::lex::token::{OperatorType, Token};
use crate::log_error;
use crate::mangling::namespace_mangle;
use crate::parse::parser::ParserData;

#[derive(Debug, Clone, PartialEq)]
pub struct CXIdent {
    pub data: String
}

impl CXIdent {
    pub fn as_str(&self) -> &str {
        self.data.as_str()
    }

    pub fn to_owned(&self) -> String {
        self.data.clone()
    }

    pub fn from(str: &str) -> Self {
        CXIdent {
            data: str.to_string()
        }
    }
}

impl Display for CXIdent {
    fn fmt(&self, f: &mut Formatter<'_>) -> std::fmt::Result {
        write!(f, "{}", self.data)
    }
}

#[derive(Debug, Clone, PartialEq)]
pub(crate) enum CXTypedIdent {
    Intrinsic(CXIdent),
    Namespace(CXIdent),
    Standard(CXIdent)
}

impl CXTypedIdent {
    pub(crate) fn assert_standard(self) -> CXIdent {
        match self {
            CXTypedIdent::Standard(ident) => ident,
            CXTypedIdent::Namespace(_) => panic!("Expected standard identifier, found namespace!"),
            CXTypedIdent::Intrinsic(_) => panic!("Expected standard identifier, found intrinsic!")
        }
    }
}

pub(crate) fn parse_identifier(data: &mut ParserData) -> Option<CXTypedIdent> {
    if matches!(data.toks.peek()?, Token::Intrinsic(_)) {
        return Some(
            CXTypedIdent::Intrinsic(
                parse_intrinsic(data)?
            )
        );
    }

    let Token::Identifier(ident) = data.toks.peek().cloned()? else {
        return None;
    };
    data.toks.next();

    let mut idents = vec![ident];

    while let Some(Token::Operator(OperatorType::ScopeRes)) = data.toks.next() {
        let Token::Identifier(ident) = data.toks.peek().cloned()? else {
            log_error!("Invalid token in namespace identifier: {:?}", data.toks.prev());
        };
        data.toks.next();

        idents.push(ident);
    }

    data.toks.back();

    match idents.len() {
        1 => Some(
            CXTypedIdent::Standard(
                CXIdent {
                    data: idents.remove(0)
                }
            )
        ),
        _ => Some(
            CXTypedIdent::Namespace(
                CXIdent {
                    data: namespace_mangle( & idents)
                }
            )
        )
    }
}

pub(crate) fn parse_intrinsic(data: &mut ParserData) -> Option<CXIdent> {
    let mut ss = String::new();

    while let Some(Token::Intrinsic(ident)) = data.toks.peek() {
        ss.push_str(format!("{:?}", ident).to_lowercase().as_str());
        data.toks.next();
    }

    if ss.is_empty() {
        return None;
    }

    Some(
        CXIdent {
            data: ss
        }
    )
}

pub(crate) fn parse_namespace_ident(data: &mut ParserData) -> Option<CXIdent> {
    match parse_identifier(data)? {
        CXTypedIdent::Namespace(ident) |
        CXTypedIdent::Standard(ident) => Some(ident),

        CXTypedIdent::Intrinsic(_) =>
            log_error!("PARSER ERROR: Expected namespace identifier, found intrinsic!")
    }
}

pub(crate) fn parse_std_ident(data: &mut ParserData) -> Option<CXIdent> {
    let Token::Identifier(ident) = data.toks.peek().cloned()? else {
        return None;
    };

    data.toks.next();

    Some(
        CXIdent {
            data: ident
        }
    )
}