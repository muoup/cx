use std::fmt::{Display, Formatter};
use cx_data_ast::parse::ast::{CXGlobalStmt, FunctionMap, TypeMap, CXAST};
use cx_data_ast::parse::value_type::{get_type_size, CXValType};
use crate::builder::{BytecodeBuilder, BytecodeFunction, BytecodeFunctionPrototype, BytecodeParameter, VirtualInstruction};

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