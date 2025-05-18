use crate::builder::BytecodeFunction;
use cx_data_ast::parse::ast::{FunctionMap, TypeMap};
use std::fmt::{Display, Formatter};

pub mod builder;
pub mod types;
mod format;

#[derive(Debug)]
pub struct ProgramBytecode {
    pub fn_map: FunctionMap,
    pub type_map: TypeMap,

    pub global_strs: Vec<String>,
    pub fn_defs: Vec<BytecodeFunction>,
}