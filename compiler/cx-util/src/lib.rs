use std::rc::Rc;

pub mod format;
pub mod scoped_map;
pub mod macros;
pub mod char_iter;
pub mod mangling;
pub mod rwlockser;

pub type CXResult<T> = Option<T>;

