use std::fmt::Formatter;

pub mod char_iter;
pub mod format;
pub mod identifier;
pub mod macros;
pub mod rwlockser;
pub mod scoped_map;
pub mod unsafe_float;

pub trait CXErrorTrait {
    fn pretty_print(&self);
}

pub struct CXError {
    pub message: String,
}

impl CXErrorTrait for CXError {
    fn pretty_print(&self) {
        println!("CXError: {}", self.message);
    }
}

pub type CXResult<T> = Result<T, Box<dyn CXErrorTrait>>;

impl CXError {
    pub fn new<T: Into<String>>(msg: T) -> Self {
        CXError { message: msg.into() }
    }
    
    pub fn unimplemented<T, U: Into<String>>(msg: U) -> CXResult<T> {
        Err(Box::new(CXError::new(format!("Unimplemented: {}", msg.into()))))
    }
    
    pub fn create_result<T, U: Into<String>>(msg: U) -> CXResult<T> {
        Err(Box::new(CXError::new(msg)))
    }
    
    pub fn create_boxed<U: Into<String>>(msg: U) -> Box<dyn CXErrorTrait> {
        Box::new(CXError::new(msg))
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