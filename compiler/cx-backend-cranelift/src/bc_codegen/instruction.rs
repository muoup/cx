use std::collections::HashMap;

use cranelift::{
    codegen::ir::{self, Function, UserFuncName},
    prelude::{
        FunctionBuilder, FunctionBuilderContext, Ieee32, Imm64, InstBuilder, Signature,
        StackSlotData, StackSlotKind, Value, isa::TargetFrontendConfig,
    },
};
use cranelift_module::{FuncId, Module};
use cx_bytecode_data::{
    bc_type::FloatType,
    compilation_unit::BCFunction,
    instruction::{BCInstruction, BCValue},
};

use crate::{
    bc_codegen::{context::BCFunctionState, types::get_cranelift_int_type},
    GlobalState,
};

pub(crate) fn generate_function(
    global_state: &mut GlobalState,
    func_id: FuncId,
    func_sig: Signature,
    bc_func: &BCFunction,
) -> Option<()> {
    let mut func = Function::with_name_signature(UserFuncName::user(0, func_id.as_u32()), func_sig);

    let mut binding = FunctionBuilderContext::new();
    let builder = FunctionBuilder::new(&mut func, &mut binding);

    let pointer_type = global_state.object_module.target_config().pointer_type();
    let target_config = global_state.object_module.target_config();

    let mut context = BCFunctionState {
        global_state,
        builder,
        variable_table: HashMap::new(),
        pointer_type,
    };

    for block in bc_func.blocks.iter() {
        for instr in block.instructions.iter() {
            generate_instruction(&mut context, instr);
        }
    }

    Some(())
}

pub(crate) fn generate_instruction(
    state: &mut BCFunctionState,
    instr: &BCInstruction,
) -> Option<()> {
    match instr {
        BCInstruction::Alias {
            source,
            result: destination,
        } => {
            let value = state.variable_table.get(source).unwrap();
            state.variable_table.insert(destination.clone(), *value);
        }

        BCInstruction::Allocate {
            result: destination,
            type_,
            alignment,
        } => {
            let allocation = state.builder.create_sized_stack_slot(StackSlotData::new(
                StackSlotKind::ExplicitSlot,
                type_.size_in_bytes() as u32,
                *alignment,
            ));

            let stack_slot = state
                .builder
                .ins()
                .stack_addr(state.pointer_type, allocation, 0);

            state.variable_table.insert(destination.clone(), stack_slot);
        }

        BCInstruction::Store {
            destination,
            value,
            store_type,
        } => {
            let val_value = generate_value(state, value).unwrap().clone();
            let dest_value = state.variable_table.get(destination).unwrap().clone();

            if store_type.is_structured() {
                let size_imm = state.builder.ins().iconst(
                    state.pointer_type,
                    Imm64::new(store_type.size_in_bytes() as i64),
                );
                
                state.builder.call_memcpy(state.global_state.target_frontend_config, dest_value, val_value, size_imm);
            } else {
                state.builder.ins().store(
                    cranelift::codegen::ir::MemFlags::new(),
                    val_value,
                    dest_value,
                    0,
                );
            }
        }

        BCInstruction::Load {
            result,
            source,
            load_type,
        } => {
            
        },
        
        BCInstruction::Return { value } => todo!(),
        BCInstruction::IntBinOp {
            result: destination,
            left,
            right,
            op,
        } => todo!(),
        BCInstruction::FloatBinOp {
            result: destination,
            left,
            right,
            op,
        } => todo!(),
        BCInstruction::PointerBinOp {
            result: destination,
            ptr_type,
            left,
            right,
            op,
        } => todo!(),
        BCInstruction::IntUnOp {
            result: destination,
            value,
            op,
        } => todo!(),
        BCInstruction::FloatUnOp {
            result: destination,
            value,
            op,
        } => todo!(),
        BCInstruction::CallDirect {
            result: destination,
            function,
            arguments,
        } => todo!(),
        BCInstruction::CallIndirect {
            result: destination,
            prototype,
            function_pointer,
            arguments,
        } => todo!(),
        BCInstruction::Branch {
            condition,
            true_target,
            false_target,
        } => todo!(),
        BCInstruction::Jump { target } => todo!(),
        BCInstruction::GetElementPtr {
            result: destination,
            base,
            index,
            offset,
            structure_type,
        } => todo!(),
        BCInstruction::GetFunctionPtr { result, function } => todo!(),
        BCInstruction::ValueCoercion {
            result: destination,
            value,
            coercion,
        } => todo!(),
        BCInstruction::Memset {
            result: destination,
            value,
            _type,
        } => todo!(),
        BCInstruction::Phi {
            result: destination,
            predecessors,
        } => todo!(),
        BCInstruction::JumpTable {
            index,
            targets,
            default_target,
        } => todo!(),
    }

    Some(())
}

pub(crate) fn generate_value(state: &mut BCFunctionState, value: &BCValue) -> Option<Value> {
    match value {
        BCValue::Address(addr) => Some(*state.variable_table.get(addr).unwrap()),
        BCValue::Integer { value, type_ } => Some(
            state
                .builder
                .ins()
                .iconst(get_cranelift_int_type(type_), Imm64::new(*value)),
        ),
        BCValue::Float {
            value,
            type_: FloatType::F32,
        } => {
            let as_float: f32 = value.into();
            Some(state.builder.ins().f32const(as_float))
        }
        BCValue::Float {
            value,
            type_: FloatType::F64,
        } => {
            let as_float: f64 = value.into();
            Some(state.builder.ins().f64const(as_float))
        }
    }
}
