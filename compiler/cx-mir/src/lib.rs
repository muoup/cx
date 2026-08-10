use crate::global::{MIRFunction, MIRGlobalVariable};

pub mod expr;
pub mod module;
pub mod global;
pub mod ty;
pub mod op;

pub struct MIRUnit {
    functions: Box<[MIRFunction]>,
    globals: Box<[MIRGlobalVariable]>,
}