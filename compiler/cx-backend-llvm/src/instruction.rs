use std::clone;
use std::env::args;
use crate::attributes::noundef;
use crate::mangling::string_literal_name;
use crate::typing::{any_to_basic_val, bc_llvm_prototype, cx_llvm_prototype};
use crate::{CodegenValue, FunctionState, GlobalState};
use cx_data_bytecode::builder::{BlockInstruction, VirtualInstruction};
use inkwell::attributes::AttributeLoc;
use inkwell::values::{AnyValue, FunctionValue};
use inkwell::Either;
use std::sync::Mutex;

fn inst_num() -> String {
    static NUM: Mutex<usize> = Mutex::new(0);
    
    let mut num = NUM.lock().unwrap();
    *num += 1;
    
    format!("inst_{}", *num)
}

pub(crate) fn generate_instruction<'a>(
    global_state: &GlobalState<'a>,
    function_state: &FunctionState<'a>,
    function_val: &FunctionValue<'a>,
    block_instruction: &BlockInstruction
) -> Option<CodegenValue<'a>> {
    Some(
        match &block_instruction.instruction {
            VirtualInstruction::Allocate { size } =>
                CodegenValue::Value(
                    function_state
                        .builder
                        .build_alloca(
                            global_state.context.i8_type().array_type(*size as u32),
                            inst_num().as_str()
                        )
                        .ok()?
                        .as_any_value_enum()
                ),
            
            VirtualInstruction::DirectCall { func, args, .. } => {
                let function_name =
                    function_state
                        .get_val_ref(func)?
                        .get_function_ref();
                
                let function_val = global_state
                    .module
                    .get_function(function_name)
                    .unwrap();

                let arg_vals = args
                    .iter()
                    .map(|arg| {
                        let val = function_state
                            .get_val_ref(arg)?
                            .get_value();
                        
                        let basic_val = any_to_basic_val(val)?;
                        
                        Some(basic_val.into())
                    })
                    .collect::<Option<Vec<_>>>()?;

                let val = function_state.builder
                    .build_direct_call(function_val.clone(), arg_vals.as_slice(), inst_num().as_str())
                    .ok()?;
                
                for i in 0..args.len() {
                    val.add_attribute(
                        AttributeLoc::Param(i as u32),
                        noundef(global_state.context)
                    )
                }

                match val.try_as_basic_value() {
                    Either::Left(val) 
                        => CodegenValue::Value(val.as_any_value_enum()),
                    Either::Right(_) 
                        => CodegenValue::NULL
                }
            },
            
            VirtualInstruction::IndirectCall { func_ptr, args, method_sig } => {
                let ptr = function_state
                    .get_val_ref(func_ptr)?
                    .get_value();
                let fn_type = bc_llvm_prototype(global_state, method_sig)
                    .unwrap();
                let args = args
                    .iter()
                    .map(|arg| {
                        let val = function_state
                            .get_val_ref(arg)?
                            .get_value();
                        
                        let basic_val = any_to_basic_val(val)?;
                        
                        Some(basic_val.into())
                    })
                    .collect::<Option<Vec<_>>>()?;
                
                let val = function_state.builder
                    .build_indirect_call(
                        fn_type,
                        ptr.into_pointer_value(),
                        args.as_slice(),
                        inst_num().as_str()
                    )
                    .ok()?;
                
                match val.try_as_basic_value() {
                    Either::Left(val) 
                        => CodegenValue::Value(val.as_any_value_enum()),
                    Either::Right(_) 
                        => CodegenValue::NULL
                }
            },

            VirtualInstruction::FunctionReference { name } =>
                CodegenValue::FunctionRef(name.clone()),

            VirtualInstruction::StringLiteral { str_id } => {
                let global = global_state
                    .module
                    .get_global(string_literal_name(*str_id as usize).as_str())
                    .unwrap();
               
                CodegenValue::Value(global.as_any_value_enum())
            },

            VirtualInstruction::BitCast { value } => {
                function_state.value_map
                    .get(value)
                    .cloned()?
            },

            VirtualInstruction::Immediate { value } => {
                let val = global_state
                    .context
                    .i32_type()
                    .const_int(*value as u64, false);

                CodegenValue::Value(val.as_any_value_enum())
            },
            
            VirtualInstruction::Jump { target } => {
                function_state
                    .builder
                    .build_unconditional_branch(
                        function_val
                            .get_basic_blocks()
                            .get(*target as usize)
                            .unwrap()
                            .clone()
                    ).ok()?;
                
                CodegenValue::NULL
            },
            
            VirtualInstruction::Return { value } => {
                let Some(value) = value else {
                    function_state
                        .builder
                        .build_return(None)
                        .ok()?;
                    
                    return Some(CodegenValue::NULL);
                };
                
                let value = function_state
                    .value_map
                    .get(value)
                    .cloned()
                    .unwrap();
                
                let basic_val = any_to_basic_val(value.get_value())?;
                
                function_state
                    .builder
                    .build_return(Some(&basic_val))
                    .ok()?;
                
                CodegenValue::NULL
            },

            _ => todo!("LLVM: generate_instruction: {:?}", block_instruction)
        }
    )
}