use crate::builder::BytecodeFunction;
use cx_data_ast::parse::ast::{FunctionMap, TypeMap};
use std::fmt::{Display, Formatter};

pub mod builder;

#[derive(Debug)]
pub struct ProgramBytecode {
    pub fn_map: FunctionMap,
    pub type_map: TypeMap,

    pub global_strs: Vec<String>,
    pub fn_defs: Vec<BytecodeFunction>,
}

impl Display for ProgramBytecode {
    fn fmt(&self, f: &mut Formatter<'_>) -> std::fmt::Result {
        for func in self.fn_defs.iter() {
            writeln!(f, "{:#?}", func)?;
        }

        Ok(())
    }
}