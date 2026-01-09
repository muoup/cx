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

    /// Attempt to downcast this error to a concrete type.
    /// Returns Some if the error is of the given type, None otherwise.
    fn as_any(&self) -> &dyn std::any::Any {
        &()
    }

    /// Get the error as a string for LSP diagnostics
    fn error_message(&self) -> String {
        "".to_string()
    }

    /// Get the compilation unit for this error, if applicable
    fn compilation_unit(&self) -> Option<std::path::PathBuf> {
        None
    }

    /// Get the token start index for this error, if applicable
    fn token_start(&self) -> Option<usize> {
        None
    }

    /// Get the token end index for this error, if applicable
    fn token_end(&self) -> Option<usize> {
        None
    }
}

pub struct CXError {
    pub message: String,
}

impl CXErrorTrait for CXError {
    fn pretty_print(&self) {
        println!("CXError: {}", self.message);
    }

    fn error_message(&self) -> String {
        self.message.clone()
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