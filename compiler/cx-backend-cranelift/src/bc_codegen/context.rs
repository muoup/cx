use std::collections::HashMap;

use cranelift::{codegen::ir, prelude::{FunctionBuilder, Value}};
use cx_bytecode_data::instruction::BCAddress;

use crate::GlobalState;

pub(crate) struct BCFunctionState<'a> {
    pub global_state: &'a GlobalState<'a>,
    pub variable_table: HashMap<BCAddress, Value>,
    pub builder: FunctionBuilder<'a>,
    pub pointer_type: ir::Type,
}
