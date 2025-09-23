use crate::inst_calling::prepare_function_sig;
use crate::instruction::codegen_instruction;
use crate::value_type::get_cranelift_type;
use crate::{FunctionState, GlobalState, VariableTable};
use cranelift::codegen::ir::{Function, UserFuncName};
use cranelift::prelude::{FunctionBuilder, FunctionBuilderContext, Signature};
use cranelift_module::{FuncId, Module};
use cx_data_mir::{MIRFunctionPrototype, BlockID, MIRFunction, ElementID, FunctionBlock, MIRValue};
use crate::routines::convert_linkage;

pub(crate) fn codegen_fn_prototype(global_state: &mut GlobalState, prototype: &MIRFunctionPrototype) -> Option<()> {
    let sig = prepare_function_sig(&mut global_state.object_module, prototype)?;
    let linkage = convert_linkage(prototype.linkage);

    let id = global_state.object_module
        .declare_function(prototype.name.as_str(), linkage, &sig)
        .unwrap();

    global_state.function_ids.insert(prototype.name.to_owned(), id);
    global_state.function_sigs.insert(prototype.name.to_owned(), sig);

    Some(())
}

pub(crate) fn codegen_block(
    context: &mut FunctionState,
    fn_block: &FunctionBlock,
    block_id: BlockID
) {
    let block = context.get_block(block_id);
    context.builder.switch_to_block(block);

    for (value_id, instr) in fn_block.body.iter().enumerate() {
        if let Some(val) = codegen_instruction(context, instr) {
            context.variable_table.insert(
                MIRValue::BlockResult {
                    block_id,
                    value_id: value_id as u32
                },
                val
            );
        };

        if instr.instruction.is_block_terminating() {
            break;
        }
    }
}

pub(crate) fn codegen_function(global_state: &mut GlobalState, func_id: FuncId, func_sig: Signature, bc_func: &MIRFunction) -> Option<()> {
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

        function_ids: &mut global_state.function_ids,

        fn_map: global_state.fn_map,
        
        defer_offset: bc_func.blocks.len(),

        variable_table: VariableTable::new(),
        block_map: Vec::new(),
        fn_params: Vec::new(),

        builder,
        pointer_type,
    };

    for _ in 0..bc_func.blocks.len() + bc_func.defer_blocks.len() {
        context.block_map.push(context.builder.create_block());
    }

    let first_block = context.get_block(BlockID::Block(0));

    for arg in bc_func.prototype.params.iter() {
        let cranelift_type = get_cranelift_type(&arg._type);
        let arg = context.builder.append_block_param(first_block, cranelift_type);

        context.fn_params.push(arg);
    }
    
    for (block_id, fn_block) in bc_func.blocks.iter().enumerate() {
        codegen_block(&mut context, fn_block, BlockID::Block(block_id as ElementID));
    }

    for (block_id, fn_block) in bc_func.defer_blocks.iter().enumerate() {
        codegen_block(&mut context, fn_block, BlockID::DeferredBlock(block_id as ElementID));
    }
    
    context.builder.seal_all_blocks();
    context.builder.finalize();

    let GlobalState { object_module, context, .. } = global_state;

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