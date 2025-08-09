use std::fmt::{Display, Formatter};
use speedy::{Readable, Writable};

#[derive(Debug, Clone, PartialEq, Eq, Readable, Writable)]
#[derive(Default)]
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