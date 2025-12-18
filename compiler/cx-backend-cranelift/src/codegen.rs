use std::collections::HashMap;

use crate::inst_calling::prepare_function_sig;
use crate::instruction::codegen_instruction;
use crate::routines::convert_linkage;
use crate::value_type::get_cranelift_type;
use crate::{FunctionState, GlobalState, VariableTable};
use cranelift::codegen::ir::{Function, UserFuncName};
use cranelift::prelude::{FunctionBuilder, FunctionBuilderContext, Signature};
use cranelift_module::{FuncId, Module};
use cx_bytecode_data::{BCBasicBlock, BCFunction, BCFunctionPrototype};
use cx_util::format::dump_data;

pub(crate) fn codegen_fn_prototype(
    global_state: &mut GlobalState,
    prototype: &BCFunctionPrototype,
) -> Option<()> {
    let sig = prepare_function_sig(&mut global_state.object_module, prototype)?;
    let linkage = convert_linkage(prototype.linkage);

    let id = global_state
        .object_module
        .declare_function(prototype.name.as_str(), linkage, &sig)
        .unwrap();

    global_state
        .function_ids
        .insert(prototype.name.to_owned(), id);
    global_state
        .function_sigs
        .insert(prototype.name.to_owned(), sig);

    Some(())
}

pub(crate) fn codegen_block(context: &mut FunctionState, fn_block: &BCBasicBlock) {
    let block = context.get_block(&fn_block.id);
    context.builder.switch_to_block(block);

    for instr in fn_block.body.iter() {
        if let Some(val) = codegen_instruction(context, instr) {
            if let Some(result) = instr.result.as_ref() {
                context.variable_table.insert(result.clone(), val);
            }
        };

        if instr.kind.is_block_terminating() {
            break;
        }
    }
}

pub(crate) fn codegen_function(
    global_state: &mut GlobalState,
    func_id: FuncId,
    func_sig: Signature,
    bc_func: &BCFunction,
) -> Option<()> {
    let mut func = Function::with_name_signature(UserFuncName::user(0, func_id.as_u32()), func_sig);

    let mut binding = FunctionBuilderContext::new();
    let builder = FunctionBuilder::new(&mut func, &mut binding);

    let pointer_type = global_state.object_module.target_config().pointer_type();

    let mut context = FunctionState {
        object_module: &mut global_state.object_module,
        target_frontend_config: &global_state.target_frontend_config,

        function_ids: &mut global_state.function_ids,

        variable_table: VariableTable::new(),
        block_map: HashMap::new(),
        fn_params: Vec::new(),

        builder,
        pointer_type,
    };

    for fn_block in bc_func.blocks.iter() {
        let block = context.builder.create_block();

        context.block_map.insert(fn_block.id.clone(), block);
    }

    let first_block = bc_func.blocks.first().map(|b| &b.id).unwrap();
    let first_block = context.get_block(first_block);

    for arg in bc_func.prototype.params.iter() {
        let cranelift_type = get_cranelift_type(&arg._type);
        let arg = context
            .builder
            .append_block_param(first_block, cranelift_type);

        context.fn_params.push(arg);
    }

    for fn_block in bc_func.blocks.iter() {
        codegen_block(&mut context, fn_block);
    }

    context.builder.seal_all_blocks();
    context.builder.finalize();

    let GlobalState {
        object_module,
        context,
        ..
    } = global_state;

    dump_data(&func);

    context.func = func;
    object_module
        .define_function(func_id, context)
        .unwrap_or_else(|err| {
            // dump_data(&context.func);
            panic!("Failed to define function: {err:#?}");
        });

    object_module.clear_context(context);

    Some(())
}
