use std::collections::HashMap;

use crate::inst_calling::prepare_function_sig;
use crate::instruction::codegen_instruction;
use crate::routines::convert_linkage;
use crate::value_type::get_cranelift_type;
use crate::{FunctionState, GlobalState, VariableTable};
use cranelift::codegen::ir::{Function, UserFuncName};
use cranelift::prelude::{FunctionBuilder, FunctionBuilderContext, Signature};
use cranelift_module::{FuncId, Module};
use cx_lmir::{LMIRBasicBlock, LMIRFunction, LMIRFunctionPrototype};
use cx_log::error::context::CXInternalContext;
use cx_log::error::{CXErr, CXResult};
use cx_log::CXRawResult;
use cx_util::format::dump_data;

pub(crate) fn codegen_fn_prototype(
    global_state: &mut GlobalState,
    prototype: &LMIRFunctionPrototype,
) -> CXRawResult<FuncId> {
    let sig = prepare_function_sig(&mut global_state.object_module, prototype.signature())?;
    let linkage = convert_linkage(prototype.linkage);

    let id = global_state
        .object_module
        .declare_function(prototype.name.as_str(), linkage, &sig)
        .unwrap();

    global_state
        .function_ids
        .insert(prototype.name.to_string(), id);
    global_state.function_sigs.insert(id, sig);

    Ok(id)
}

pub(crate) fn codegen_block(
    context: &mut FunctionState,
    fn_block: &LMIRBasicBlock,
) -> CXResult<()> {
    let block = context.get_block(&fn_block.id);
    context.builder.switch_to_block(block);

    for instr in fn_block.body.iter() {
        let ret = codegen_instruction(context, instr).map_err(|err| {
            CXErr::new(
                err,
                CXInternalContext::error(format!("Failed to codegen instruction: {instr}")),
            )
        })?;

        if let Some(result) = instr.result.as_ref() {
            context.variable_table.insert(result.clone(), ret);
        }

        if instr.kind.is_block_terminating() {
            break;
        }
    }

    Ok(())
}

pub(crate) fn codegen_function(
    global_state: &mut GlobalState,
    func_id: FuncId,
    func_sig: Signature,
    bc_func: &LMIRFunction,
) -> CXResult<()> {
    let mut func = Function::with_name_signature(UserFuncName::user(0, func_id.as_u32()), func_sig);

    let mut binding = FunctionBuilderContext::new();
    let builder = FunctionBuilder::new(&mut func, &mut binding);

    let pointer_type = global_state.object_module.target_config().pointer_type();

    let mut context = FunctionState {
        object_module: &mut global_state.object_module,

        function_ids: &mut global_state.function_ids,
        global_ids: &global_state.global_ids,

        variable_table: VariableTable::new(),
        block_map: HashMap::new(),
        fn_params: Vec::new(),

        builder,
        pointer_type,
        signature: bc_func.prototype.signature.clone(),
    };

    for fn_block in bc_func.blocks.iter() {
        let block = context.builder.create_block();

        context.block_map.insert(fn_block.id.clone(), block);
    }

    let first_block = bc_func.blocks.first().map(|b| &b.id).unwrap();
    let first_block = context.get_block(first_block);

    for index in 0..bc_func.prototype.signature.expanded_param_count() {
        let cranelift_type = get_cranelift_type(
            &bc_func
                .prototype
                .signature
                .expanded_param_type(global_state.architecture, index)
                .unwrap(),
        )
        .map_err(|err| {
            CXErr::new(
                err,
                CXInternalContext::error("Failed to get Cranelift type for function parameter"),
            )
        })?;

        let arg = context
            .builder
            .append_block_param(first_block, cranelift_type);

        context.fn_params.push(arg);
    }

    for fn_block in &bc_func.blocks {
        let block = context.get_block(&fn_block.id);
        for parameter in &fn_block.params {
            let parameter_type = if parameter._type.is_memory_resident() {
                context.pointer_type
            } else {
                get_cranelift_type(&parameter._type).map_err(|err| {
                    CXErr::new(
                        err,
                        CXInternalContext::error(format!(
                            "Failed to lower block parameter {} in {}",
                            parameter.register, fn_block.id
                        )),
                    )
                })?
            };
            let value = context.builder.append_block_param(block, parameter_type);
            context.variable_table.insert(
                parameter.register.clone(),
                crate::CodegenValue::Value(value),
            );
        }
    }

    for fn_block in bc_func.blocks.iter() {
        codegen_block(&mut context, fn_block)?;
    }

    context.builder.seal_all_blocks();
    context.builder.finalize();

    let GlobalState { object_module, .. } = global_state;

    dump_data(&func);

    let mut codegen_context = cranelift::codegen::Context::for_function(func);
    codegen_context.compute_cfg();
    codegen_context.compute_domtree();
    object_module
        .define_function(func_id, &mut codegen_context)
        .unwrap_or_else(|err| panic!("Failed to define function: {err:#?}"));

    object_module.clear_context(&mut codegen_context);

    Ok(())
}
