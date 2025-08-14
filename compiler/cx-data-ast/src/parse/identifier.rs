use std::fmt::{Display, Formatter};
use std::sync::Arc;
use speedy::{Context, Readable, Reader, Writable, Writer};

#[derive(Debug, Default, Clone, PartialEq, Eq)]
pub struct CXIdent {
    data: Arc<str>
}

impl<'a, C: Context> Readable<'a, C> for CXIdent {
    fn read_from<R: Reader<'a, C>>(reader: &mut R) -> Result<Self, C::Error> {
        let s = String::read_from(reader)?;
        Ok(CXIdent { data: Arc::from(s) })
    }
}

impl<C: Context> Writable<C> for CXIdent {
    fn write_to<T: ?Sized + Writer<C>>(&self, writer: &mut T) -> Result<(), C::Error> {
        self.data.as_ref().write_to(writer)
    }
}

impl CXIdent {
    pub fn as_str(&self) -> &str {
        self.data.as_ref()
    }

    pub fn as_string(&self) -> String {
        self.data.to_string()
    }
    
    pub fn set_data<T: Into<Arc<str>>>(&mut self, data: T) {
        self.data = data.into();
    }

    pub fn from<T: Into<Arc<str>>>(str: T) -> Self {
        CXIdent {
            data: str.into()
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