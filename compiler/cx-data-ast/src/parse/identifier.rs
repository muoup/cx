use std::fmt::{Display, Formatter};
use serde::{Deserialize, Serialize};
use cx_util::log_error;
use crate::lex::token::{OperatorType, TokenKind};
use crate::parse::parser::{ParserData, TokenIter};
use crate::try_next;

#[derive(Debug, Clone, PartialEq, Serialize, Deserialize)]
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

pub fn parse_identifier(tokens: &mut TokenIter) -> Option<CXTypedIdent> {
    if matches!(tokens.peek()?.kind, TokenKind::Intrinsic(_)) {
        return Some(
            CXTypedIdent::Intrinsic(
                parse_intrinsic(tokens)?
            )
        );
    }

    let TokenKind::Identifier(ident) = tokens.peek().cloned()?.kind else {
        return None;
    };
    tokens.next();

    let mut idents = vec![CXIdent::from_owned(ident)];

    while try_next!(tokens, TokenKind::Operator(OperatorType::ScopeRes)) {
        let TokenKind::Identifier(ident) = tokens.peek().cloned()?.kind else {
            log_error!("Invalid token in namespace identifier: {:?}", tokens.prev());
        };
        tokens.next();

        idents.push(CXIdent::from_owned(ident));
    }

    tokens.back();

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

pub fn parse_intrinsic(tokens: &mut TokenIter) -> Option<CXIdent> {
    let mut ss = String::new();

    while let Some(TokenKind::Intrinsic(ident)) = tokens.peek().map(|tok| &tok.kind) {
        ss.push_str(format!("{ident:?}").to_lowercase().as_str());
        tokens.next();
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

pub fn parse_std_ident(tokens: &mut TokenIter) -> Option<CXIdent> {
    let TokenKind::Identifier(ident) = tokens.peek().cloned()?.kind else {
        return None;
    };

    tokens.next();

    Some(
        CXIdent {
            data: ident
        }
    )
}