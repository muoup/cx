use std::env::args;
use cranelift::codegen::ir::Inst;
use cranelift::prelude::Value;
use cx_data_ast::parse::value_type::{get_type_size, is_structure, CXValType};
use cx_data_bytecode::builder::{BytecodeFunctionPrototype, ValueID};
use crate::{CodegenValue, FunctionState};
use crate::routines::allocate_variable;

pub(crate) fn prepare_method_call<'a>(
    context: &'a mut FunctionState,
    func: ValueID,
    prototype: &BytecodeFunctionPrototype,
    args: &'a [ValueID],
) -> Option<(CodegenValue, Vec<Value>)> {
    let val = context.variable_table.get(&func).cloned().unwrap();
    let mut params = args.iter()
        .map(|arg| context.variable_table.get(arg).unwrap().as_value())
        .collect::<Vec<_>>();

    if is_structure(context.type_map, &prototype.return_type) {
        let type_size = get_type_size(context.type_map, &context.function_prototype.return_type)?;
        let temp_buffer = allocate_variable(context, type_size as u32, None)?;

        params.insert(0, temp_buffer);
    }

    Some((val, params))
}

pub(crate) fn get_method_return(
    context: &FunctionState,
    inst: Inst
) -> Option<CodegenValue> {
    context.builder.inst_results(inst)
        .first()
        .cloned()
        .map(|res| CodegenValue::Value(res))
}