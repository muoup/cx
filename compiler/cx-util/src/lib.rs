use std::fmt::Display;
use std::hash::Hash;
use std::rc::Rc;
use speedy::{Context, Readable, Writable};

pub mod format;
pub mod scoped_map;
pub mod macros;
pub mod char_iter;
pub mod mangling;

pub type CXResult<T> = Option<T>;

pub type ModuleIdentifier = Rc<String>;