use cranelift::codegen::ir;
use crate::codegen::instruction::codegen_instruction;
use crate::codegen::value_type::{get_cranelift_abi_type, get_cranelift_type};
use crate::codegen::{FunctionState, GlobalState, VariableTable};
use crate::parse::ast::{ValueType, VarInitialization};
use crate::parse::pass_verified::bytecode::{ValueID, VerifiedFunction, VirtualInstruction};
use crate::parse::pass_verified::context::FunctionPrototype;
use cranelift::codegen::ir::{Function, UserFuncName};
use cranelift::prelude::{EntityRef, FunctionBuilder, FunctionBuilderContext, InstBuilder, Signature};
use cranelift_module::{FuncId, Linkage, Module};

pub(crate) fn codegen_fn_prototype(global_state: &mut GlobalState, prototype: &FunctionPrototype) -> Option<()> {
    let mut sig = Signature::new(
        global_state.object_module.target_config().default_call_conv
    );

    for VarInitialization { type_, .. } in prototype.args.iter() {
        sig.params.push(get_cranelift_abi_type(global_state.type_map, type_));
    }

    match &prototype.return_type {
        ValueType::Unit => {},
        ValueType::Structured { .. } => {
            let _type = global_state.object_module.target_config().pointer_type();
            sig.returns.push(ir::AbiParam::new(_type));
        }
        _ => sig.returns.push(get_cranelift_abi_type(global_state.type_map, &prototype.return_type))
    }

    let id = global_state.object_module
        .declare_function(prototype.name.as_str(), Linkage::Preemptible, &sig)
        .unwrap();

    global_state.function_ids.insert(prototype.name.clone(), id);
    global_state.function_sigs.insert(prototype.name.clone(), sig);

    Some(())
}

pub(crate) fn codegen_function(global_state: &mut GlobalState, func_id: FuncId, func_sig: Signature, bc_func: &VerifiedFunction) -> Option<()> {
    let mut func = Function::with_name_signature(
        UserFuncName::user(0, func_id.as_u32()),
        func_sig
    );

    let mut binding = FunctionBuilderContext::new();
    let mut builder = FunctionBuilder::new(&mut func, &mut binding);

    let block = builder.create_block();
    builder.switch_to_block(block);

    let mut var_table = VariableTable::new();
    let pointer_type = global_state.object_module.target_config().pointer_type();

    let mut context = FunctionState {
        object_module: &mut global_state.object_module,
        target_frontend_config: &global_state.target_frontend_config,

        function_ids: &global_state.function_ids,

        type_map: &global_state.type_map,
        fn_map: &global_state.fn_map,

        fn_params: Vec::new(),

        builder,
        function_prototype: &bc_func.prototype,
        variable_table: var_table,
        global_strs: &global_state.global_strs,
        pointer_type,

        current_block_exited: false
    };

    for (block_id, fn_block) in bc_func.blocks.iter().enumerate() {
        let block = context.builder.create_block();
        context.builder.switch_to_block(block);

        if block_id == 0 {
            for arg in bc_func.prototype.args.iter() {
                let cranelift_type = get_cranelift_type(&arg.type_, context.type_map);
                let arg = context.builder.append_block_param(block, cranelift_type);

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

            match instr.instruction {
                VirtualInstruction::Return { .. } |
                VirtualInstruction::Branch { .. } |
                VirtualInstruction::Jump { .. } => {
                    if value_id + 1 < fn_block.body.len() {
                        println!("Redundant instructions after terminator in block {}", block_id);

                        for instr in fn_block.body.iter().skip(value_id + 1) {
                            println!("{:?}", instr);
                        }
                    }
                    break;
                },
                _ => {}
            }
        }
    }

    context.builder.seal_all_blocks();
    context.builder.finalize();

    println!("{:?}", func);

    let GlobalState { object_module, context, .. } = global_state;

    context.func = func;
    object_module
        .define_function(func_id, context)
        .expect("Failed to define function");

    object_module.clear_context(context);

    Some(())
}