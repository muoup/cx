use std::fmt::{Display, Formatter};
use cx_util::log_error;
use crate::lex::token::{OperatorType, Token};
use crate::parse::parser::ParserData;

#[derive(Debug, Clone, PartialEq)]
pub struct CXIdent {
    pub data: String
}

impl CXIdent {
    pub fn as_str(&self) -> &str {
        self.data.as_str()
    }

    pub fn as_string(&self) -> String {
        self.data.clone()
    }

    pub fn from(str: &str) -> Self {
        CXIdent {
            data: str.to_string()
        }
    }

    pub fn from_owned(str: String) -> Self {
        CXIdent {
            data: str
        }
    }
}

impl Display for CXIdent {
    fn fmt(&self, f: &mut Formatter<'_>) -> std::fmt::Result {
        write!(f, "{}", self.data)
    }
}

#[derive(Debug, Clone, PartialEq)]
pub enum CXTypedIdent {
    Intrinsic(CXIdent),
    Namespace(Vec<CXIdent>),
    Standard(CXIdent)
}

impl CXTypedIdent {
    pub fn assert_standard(self) -> CXIdent {
        match self {
            CXTypedIdent::Standard(ident) => ident,
            CXTypedIdent::Namespace(_) => panic!("Expected standard identifier, found namespace!"),
            CXTypedIdent::Intrinsic(_) => panic!("Expected standard identifier, found intrinsic!")
        }
    }
}

pub fn parse_identifier(data: &mut ParserData) -> Option<CXTypedIdent> {
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

    let mut idents = vec![CXIdent::from_owned(ident)];

    while let Some(Token::Operator(OperatorType::ScopeRes)) = data.toks.next() {
        let Token::Identifier(ident) = data.toks.peek().cloned()? else {
            log_error!("Invalid token in namespace identifier: {:?}", data.toks.prev());
        };
        data.toks.next();

        idents.push(CXIdent::from_owned(ident));
    }

    data.toks.back();

    match idents.len() {
        1 => Some(
            CXTypedIdent::Standard(
                CXIdent {
                    data: idents.remove(0).to_string()
                }
            )
        ),
        _ => Some(
            CXTypedIdent::Namespace(idents)
        )
    }
}

pub fn parse_intrinsic(data: &mut ParserData) -> Option<CXIdent> {
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

pub fn parse_std_ident(data: &mut ParserData) -> Option<CXIdent> {
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