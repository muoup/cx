use crate::inst_calling::prepare_function_sig;
use crate::instruction::codegen_instruction;
use crate::value_type::get_cranelift_type;
use crate::{FunctionState, GlobalState, VariableTable};
use cranelift::codegen::ir::{Function, UserFuncName};
use cranelift::prelude::{FunctionBuilder, FunctionBuilderContext, Signature};
use cranelift_module::{FuncId, Linkage, Module};
use cx_util::format::dump_data;
use std::collections::HashMap;
use cx_data_bytecode::{BCFunctionPrototype, BytecodeFunction, ValueID, VirtualInstruction};

pub(crate) fn codegen_fn_prototype(global_state: &mut GlobalState, prototype: &BCFunctionPrototype) -> Option<()> {
    let sig = prepare_function_sig(&mut global_state.object_module, &prototype)?;

    let id = global_state.object_module
        .declare_function(prototype.name.as_str(), Linkage::Preemptible, &sig)
        .unwrap();

    global_state.function_ids.insert(prototype.name.to_owned(), id);
    global_state.function_sigs.insert(prototype.name.to_owned(), sig);

    Some(())
}

pub(crate) fn codegen_function(global_state: &mut GlobalState, func_id: FuncId, func_sig: Signature, bc_func: &BytecodeFunction) -> Option<()> {
    let mut func = Function::with_name_signature(
        UserFuncName::user(0, func_id.as_u32()),
        func_sig
    );

    let mut binding = FunctionBuilderContext::new();
    let builder = FunctionBuilder::new(&mut func, &mut binding);

    let pointer_type = global_state.object_module.target_config().pointer_type();

    let mut context = FunctionState {
        object_module: &mut global_state.object_module,
        target_frontend_config: &global_state.target_frontend_config,

        function_ids: &global_state.function_ids,

        type_map: &global_state.type_map,
        fn_map: &global_state.fn_map,

        variable_table: VariableTable::new(),
        block_map: Vec::new(),
        local_defined_functions: HashMap::new(),
        fn_params: Vec::new(),

        builder,
        function_prototype: &bc_func.prototype,
        global_strs: &global_state.global_strs,
        pointer_type,

        current_block_exited: false
    };

    for _ in 0..bc_func.blocks.len() {
        context.block_map.push(
            context.builder.create_block()
        );
    }

    for (block_id, fn_block) in bc_func.blocks.iter().enumerate() {
        let block = context.block_map.get(block_id).unwrap();
        context.builder.switch_to_block(*block);

        if block_id == 0 {
            if bc_func.prototype.return_type.is_structure() {
                context.builder.append_block_param(*block, context.pointer_type);
            }

            for arg in bc_func.prototype.params.iter() {
                let cranelift_type = get_cranelift_type(&arg.type_);
                let arg = context.builder.append_block_param(*block, cranelift_type);

                context.fn_params.push(arg);
            }
        }

        for (value_id, instr) in fn_block.body.iter().enumerate() {
            if let Some(val) = codegen_instruction(&mut context, &instr) {
                context.variable_table.insert(
                    ValueID {
                        block_id: block_id as u32,
                        value_id: value_id as u32
                    },
                    val
                );
            }
            
            if instr.instruction.is_block_terminating() {
                break;
            }
        }
    }

    context.builder.seal_all_blocks();
    context.builder.finalize();

    dump_data(&func);
    println!("Function: {}", func);

    let GlobalState { object_module, context, .. } = global_state;

    context.func = func;
    object_module
        .define_function(func_id, context)
        .unwrap_or_else(|err| {
            panic!("Failed to define function: {err:#?}");
        });

    object_module.clear_context(context);

    Some(())
}