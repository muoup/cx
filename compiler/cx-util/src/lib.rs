use std::fmt::Formatter;

pub mod char_iter;
pub mod format;
pub mod identifier;
pub mod macros;
pub mod rwlockser;
pub mod scoped_map;
pub mod hashable_float;

pub struct CXError {
    pub message: String,
}
pub type CXResult<T> = Result<T, CXError>;

impl CXError {
    pub fn new<T: Into<String>>(msg: T) -> Self {
        CXError { message: msg.into() }
    }
}

impl std::fmt::Debug for CXError {
    fn fmt(&self, f: &mut Formatter<'_>) -> std::fmt::Result {
        write!(f, "CXError: {}", self.message)
    }
}

impl std::fmt::Display for CXError {
    fn fmt(&self, f: &mut Formatter<'_>) -> std::fmt::Result {
        write!(f, "CXError: {}", self.message)
    }
}