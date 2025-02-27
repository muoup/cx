use cranelift::prelude::{InstBuilder, Value};
use cranelift_module::{DataId, Module};
use crate::codegen::FunctionState;
use crate::parse::verify::bytecode::{BlockInstruction, VirtualInstruction};

pub(crate) fn codegen_instruction(context: &mut FunctionState, instruction: &BlockInstruction) -> Option<Value> {
    match &instruction.instruction {
        VirtualInstruction::Literal {
            val
        } => Some(context.builder.ins().iconst(context.pointer_type, *val as i64)),

        VirtualInstruction::StringLiteral { str_id } => {
            let pointer_type = context.pointer_type;
            let global_val = context.global_strs.get(*str_id as usize).cloned().unwrap();

            context.object_module.declare_data_in_func(
                DataId::from_u32(global_val.as_u32()),
                &mut context.builder.func
            );

            Some(context.builder.ins().global_value(pointer_type, global_val))
        },

        VirtualInstruction::DirectCall {
            function, args
        } => {
            let func_id = context.function_ids.get(function).cloned().unwrap();
            let func_ref = context.object_module.declare_func_in_func(func_id, &mut context.builder.func);

            let args = args.iter()
                .map(|arg| {
                    context.variable_table.get(&arg).cloned().unwrap()
                })
                .collect::<Vec<Value>>();

            let inst = context.builder.ins().call(func_ref, args.as_slice());

            context.builder.inst_results(inst)
                .first()
                .cloned()
                .or_else(|| Some(Value::from_u32(0)))
        }

        VirtualInstruction::Return { value } => {
            match value {
                Some(value) => {
                    let val = context.variable_table.get(&value).cloned().unwrap();

                    context.builder.ins().return_(&[val])
                },
                None => {
                    context.builder.ins().return_(&[])
                },
            };
            Some(Value::from_u32(0))
        },

        _ => unimplemented!("Instruction not implemented: {:?}", instruction.instruction)
    }
}