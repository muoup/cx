use speedy::{Context, Readable, Reader, Writable, Writer};
use std::fmt::{Display, Formatter};
use std::hash::Hash;
use std::sync::Arc;

#[derive(Debug, Default, Clone, PartialEq, Eq)]
pub struct CHLIdent {
    data: Arc<str>,
}

impl Hash for CHLIdent {
    fn hash<H: std::hash::Hasher>(&self, state: &mut H) {
        self.data.hash(state);
    }
}

impl<'a, C: Context> Readable<'a, C> for CHLIdent {
    fn read_from<R: Reader<'a, C>>(reader: &mut R) -> Result<Self, C::Error> {
        let s = String::read_from(reader)?;
        Ok(CHLIdent { data: Arc::from(s) })
    }
}

impl<C: Context> Writable<C> for CHLIdent {
    fn write_to<T: ?Sized + Writer<C>>(&self, writer: &mut T) -> Result<(), C::Error> {
        self.data.as_ref().write_to(writer)
    }
}

impl Display for CHLIdent {
    fn fmt(&self, f: &mut Formatter<'_>) -> std::fmt::Result {
        write!(f, "{}", self.data)
    }
}

impl CHLIdent {
    pub fn as_str(&self) -> &str {
        self.data.as_ref()
    }

    pub fn as_string(&self) -> String {
        self.data.to_string()
    }

    pub fn set_data<T: Into<Arc<str>>>(&mut self, data: T) {
        self.data = data.into();
    }

    pub fn new<T: Into<Arc<str>>>(str: T) -> Self {
        CHLIdent { data: str.into() }
    }

    pub fn map_data<F: FnOnce(&str) -> String>(&mut self, f: F) {
        self.data = f(self.data.as_ref()).into();
    }
}

impl From<String> for CHLIdent {
    fn from(value: String) -> Self {
        CHLIdent {
            data: Arc::from(value),
        }
    }
}

impl From<&str> for CHLIdent {
    fn from(value: &str) -> Self {
        CHLIdent {
            data: Arc::from(value),
        }
    }
}
