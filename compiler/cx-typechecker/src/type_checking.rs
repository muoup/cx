pub mod aggregate;
pub mod coercion;
pub mod constexpr;
pub mod contracts;
pub mod control_flow;
pub mod functions;
pub mod globals;
pub mod op;
pub mod pattern;
pub mod result;
pub mod typechecker;
pub mod value;

pub use functions::{complete_base_functions, realize_fn_implementation};
pub use globals::complete_base_globals;
