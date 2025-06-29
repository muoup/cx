use crate::arithmetic::{generate_int_binop, generate_ptr_binop};
use crate::attributes::noundef;
use crate::mangling::string_literal_name;
use crate::typing::{any_to_basic_type, any_to_basic_val, bc_llvm_prototype, bc_llvm_type};
use crate::{CodegenValue, FunctionState, GlobalState};
use cx_data_bytecode::types::BCTypeKind;
use cx_data_bytecode::{BCFloatBinOp, BCFloatUnOp, BCIntUnOp, BlockInstruction, VirtualInstruction};
use inkwell::attributes::AttributeLoc;
use inkwell::types::BasicType;
use inkwell::values::{AnyValue, AnyValueEnum, BasicValue, FunctionValue, IntMathValue};
use inkwell::Either;
use std::sync::Mutex;

pub(crate) fn inst_num() -> String {
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
            
            VirtualInstruction::VariableAllocate { size } => {
                let size = function_state
                    .get_val_ref(size)?
                    .get_value()
                    .into_int_value();
                
                let allocation = function_state.builder
                    .build_array_alloca(
                        global_state.context.i8_type(),
                        size,
                        inst_num().as_str()
                    )
                    .ok()?
                    .as_any_value_enum();
                
                function_state.builder
                    .get_insert_block()?
                    .get_last_instruction()?
                    .set_alignment(8)
                    .ok()?;
                
                CodegenValue::Value(allocation)
            },
            
            VirtualInstruction::DirectCall { func, args, method_sig } => {
                let function_name =
                    function_state
                        .get_val_ref(func)?
                        .get_function_ref();
                
                let function_val = global_state
                    .module
                    .get_function(function_name)
                    .unwrap();

                let mut arg_vals = args
                    .iter()
                    .map(|arg| {
                        let val = function_state
                            .get_val_ref(arg)?
                            .get_value();
                        
                        let basic_val = any_to_basic_val(val)?;
                        
                        Some(basic_val.into())
                    })
                    .collect::<Option<Vec<_>>>()?;

                if method_sig.return_type.is_structure() {
                    let llvm_type = bc_llvm_type(global_state, &method_sig.return_type)?;
                    let temp_buffer = function_state
                        .builder
                        .build_alloca(
                            any_to_basic_type(llvm_type)?,
                            inst_num().as_str()
                        )
                        .ok()?;

                    arg_vals.insert(0, temp_buffer.into());
                }

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
                let mut args = args
                    .iter()
                    .map(|arg| {
                        let val = function_state
                            .get_val_ref(arg)?
                            .get_value();
                        
                        let basic_val = any_to_basic_val(val)?;
                        
                        Some(basic_val.into())
                    })
                    .collect::<Option<Vec<_>>>()?;

                if method_sig.return_type.is_structure() {
                    let llvm_type = bc_llvm_type(global_state, &method_sig.return_type)?;
                    let temp_buffer = function_state
                        .builder
                        .build_alloca(
                            any_to_basic_type(llvm_type)?,
                            inst_num().as_str()
                        )
                        .ok()?;

                    args.insert(0, temp_buffer.into());
                }
                
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
                let val = function_state.value_map
                    .get(value)
                    .cloned()?
                    .get_value();
                let basic_val = any_to_basic_val(val)?;
                
                let bit_cast_type = bc_llvm_type(
                    global_state, 
                    &block_instruction.value.type_
                )?;
                let basic_type = any_to_basic_type(bit_cast_type).unwrap();
                
                CodegenValue::Value(
                    function_state.builder
                        .build_bit_cast(
                            basic_val,
                            basic_type,
                            inst_num().as_str()
                        )
                        .ok()?
                        .as_any_value_enum()
                )
            },

            VirtualInstruction::IntToPtrDiff { value, .. } => {
                function_state.get_val_ref(value)?.clone()
            },

            VirtualInstruction::Immediate { value } => {
                let imm_type = bc_llvm_type(
                    global_state, 
                    &block_instruction.value.type_
                )?.into_int_type();
                
                CodegenValue::Value(imm_type.const_int(*value as u64, false).as_any_value_enum())
            },

            VirtualInstruction::FloatImmediate { value } => {
                let imm_type = bc_llvm_type(
                    global_state,
                    &block_instruction.value.type_
                )?.into_float_type();

                CodegenValue::Value(imm_type.const_float(*value).as_any_value_enum())
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
                
                let current_prototype = global_state.function_map
                    .get(&function_state.current_function)
                    .unwrap();
                
                if current_prototype.return_type.is_structure() {
                    let llvm_type = bc_llvm_type(
                        global_state, 
                        &current_prototype.return_type
                    )?;
                    let type_size = any_to_basic_type(llvm_type)?
                        .size_of()
                        .expect("Failed to get size of type");
                    
                    let return_param = function_val
                        .get_nth_param(0)
                        .unwrap()
                        .into_pointer_value();
                    
                    function_state.builder
                        .build_memcpy(
                            return_param, 1,
                            value.get_value().into_pointer_value(), 1,
                            type_size
                        )
                        .unwrap();
                    
                    function_state
                        .builder
                        .build_return(Some(&return_param.as_basic_value_enum()))
                        .ok()?;
                } else {
                    let basic_val = any_to_basic_val(value.get_value())?;

                    function_state
                        .builder
                        .build_return(Some(&basic_val))
                        .ok()?;   
                }
                
                CodegenValue::NULL
            },
            
            VirtualInstruction::FunctionParameter { param_index } => {
                let function = global_state
                    .function_map
                    .get(function_state.current_function.as_str())
                    .unwrap();
                
                let param_index = if function.return_type.is_structure() {
                    *param_index + 1 // Skip the first parameter for the return type
                } else {
                    *param_index
                };
                
                let param = function_val
                    .get_nth_param(param_index)
                    .unwrap();
                
                CodegenValue::Value(param.as_any_value_enum())
            },
            
            VirtualInstruction::Store { value, type_, memory } => {
                let any_value = function_state
                    .get_val_ref(value)
                    .unwrap()
                    .get_value();
                let any_type = bc_llvm_type(global_state, type_).unwrap();

                let basic_val = any_to_basic_val(any_value)
                    .expect(format!("Failed to convert value {any_value:?} to basic value").as_str());
                let basic_type = any_to_basic_type(any_type).unwrap();
                
                let memory_val = function_state
                    .get_val_ref(memory)?
                    .get_value()
                    .into_pointer_value();
                
                if basic_type.is_struct_type() {
                    function_state
                        .builder
                        .build_memcpy(
                            memory_val,
                            1,
                            basic_val.into_pointer_value(),
                            1,
                            basic_type.size_of()
                                .expect("Failed to get size of type")
                        )
                        .ok()?;
                } else {
                    function_state
                        .builder
                        .build_store(memory_val, basic_val)
                        .unwrap();
                }
                
                CodegenValue::NULL
            },
            
            VirtualInstruction::Load { value } => {
                let any_value = function_state
                    .get_val_ref(value)?
                    .get_value()
                    .into_pointer_value();
                
                let loaded_type = bc_llvm_type(
                    global_state, 
                    &block_instruction.value.type_,
                ).unwrap();
                let basic_type = any_to_basic_type(loaded_type).unwrap();

                let val = function_state
                    .builder
                    .build_load(basic_type, any_value, inst_num().as_str())
                    .ok()?;
                
                CodegenValue::Value(val.as_any_value_enum())
            },

            VirtualInstruction::PointerBinOp { left, ptr_type, right, op } => {
                let left_value = function_state
                    .get_val_ref(left)
                    .unwrap()
                    .get_value();

                let right_value = function_state
                    .get_val_ref(right)
                    .unwrap()
                    .get_value();

                generate_ptr_binop(
                    global_state, function_state,
                    ptr_type,
                    left_value, right_value,
                    *op
                )?
            },
            
            VirtualInstruction::IntegerUnOp { value, op } => {
                let value = function_state
                    .get_val_ref(value)?
                    .get_value()
                    .into_int_value();
                
                CodegenValue::Value(
                    match op {
                        BCIntUnOp::NEG => function_state.builder
                            .build_int_neg(value, inst_num().as_str())
                            .ok()?
                            .as_any_value_enum(),
                        BCIntUnOp::BNOT => function_state.builder
                            .build_not(value, inst_num().as_str())
                            .ok()?
                            .as_any_value_enum(),
                        BCIntUnOp::LNOT => function_state.builder
                            .build_int_compare(
                                inkwell::IntPredicate::EQ,
                                value, 
                                value.get_type().const_int(0, false),
                                inst_num().as_str()
                            )
                            .ok()?
                            .as_any_value_enum(),
                        _ => return None, // Unsupported operation for IntegerUnOp
                    }
                )
            },
            
            VirtualInstruction::IntegerBinOp { left, right, op } => {
                let left = function_state
                    .get_val_ref(left)?
                    .get_value();
                
                let right = function_state
                    .get_val_ref(right)?
                    .get_value();

                generate_int_binop(global_state, function_state, left, right, *op)?
            },
            
            VirtualInstruction::FloatUnOp { value, op } => {
                let value = function_state
                    .get_val_ref(value)?
                    .get_value()
                    .into_float_value();
                
                CodegenValue::Value(
                    match op {
                        BCFloatUnOp::NEG => function_state.builder
                            .build_float_neg(value, inst_num().as_str())
                            .ok()?
                            .as_any_value_enum(),
                    }
                )
            },
            
            VirtualInstruction::FloatBinOp { left, right, op } => {
                let left_value = function_state
                    .get_val_ref(left)?
                    .get_value()
                    .into_float_value();
                
                let right_value = function_state
                    .get_val_ref(right)?
                    .get_value()
                    .into_float_value();
                
                CodegenValue::Value(
                    match op {
                        BCFloatBinOp::ADD => function_state.builder
                            .build_float_add(left_value, right_value, inst_num().as_str())
                            .ok()?
                            .as_any_value_enum(),
                        
                        BCFloatBinOp::SUB => function_state.builder
                            .build_float_sub(left_value, right_value, inst_num().as_str())
                            .ok()?
                            .as_any_value_enum(),
                        
                        BCFloatBinOp::FMUL => function_state.builder
                            .build_float_mul(left_value, right_value, inst_num().as_str())
                            .ok()?
                            .as_any_value_enum(),
                        
                        BCFloatBinOp::FDIV => function_state.builder
                            .build_float_div(left_value, right_value, inst_num().as_str())
                            .ok()?
                            .as_any_value_enum(),
                    }
                )
            },
            
            VirtualInstruction::Branch { condition, true_block, false_block } => {
                let mut condition_value = function_state
                    .get_val_ref(condition)?
                    .get_value()
                    .into_int_value();
                
                if condition_value.get_type().get_bit_width() > 1 {
                    condition_value = function_state.builder
                        .build_int_truncate(
                            condition_value, 
                            global_state.context.bool_type(), 
                            inst_num().as_str()
                        )
                        .ok()?;
                }
                
                let true_block_val = function_val
                    .get_basic_blocks()
                    .get(*true_block as usize)
                    .unwrap()
                    .clone();
                
                let false_block_val = function_val
                    .get_basic_blocks()
                    .get(*false_block as usize)
                    .unwrap()
                    .clone();
                
                function_state.builder
                    .build_conditional_branch(
                        condition_value,
                        true_block_val,
                        false_block_val,
                    )
                    .ok()?;
                
                CodegenValue::NULL
            },
            
            VirtualInstruction::JumpTable { value, targets, default } => {
                let value = function_state
                    .get_val_ref(value)?
                    .get_value()
                    .into_int_value();
                
                let targets = targets
                    .iter()
                    .map(|(value, block)| {
                        let value = global_state.context.i32_type()
                            .const_int(*value as u64, false);
                        let block = function_val
                            .get_basic_blocks()
                            .get(*block as usize)
                            .unwrap()
                            .clone();
                        
                        (value, block)
                    })
                    .collect::<Vec<_>>();
                
                function_state.builder
                    .build_switch(
                        value,
                        function_val.get_basic_blocks().get(*default as usize)
                            .unwrap()
                            .clone(),
                        targets.as_slice()
                    )
                    .ok()?;
                
                CodegenValue::NULL
            },
            
            VirtualInstruction::ZExtend { value } => {
                let value = function_state
                    .get_val_ref(value)?
                    .get_value()
                    .into_int_value();
                let to_type = bc_llvm_type(
                    global_state, 
                    &block_instruction.value.type_
                )?.into_int_type();
                
                CodegenValue::Value(
                    function_state.builder
                        .build_int_z_extend(value, to_type, inst_num().as_str())
                        .ok()?
                        .as_any_value_enum()
                )
            },
            
            VirtualInstruction::SExtend { value } => {
                let value = function_state
                    .get_val_ref(value)?
                    .get_value()
                    .into_int_value();
                let to_type = bc_llvm_type(
                    global_state, 
                    &block_instruction.value.type_
                )?.into_int_type();
                
                CodegenValue::Value(
                    function_state.builder
                        .build_int_s_extend(value, to_type, inst_num().as_str())
                        .ok()?
                        .as_any_value_enum()
                )
            },
            
            VirtualInstruction::StructAccess { struct_, struct_type, field_index, .. } => {
                let struct_ptr = function_state
                    .get_val_ref(struct_)?
                    .get_value()
                    .into_pointer_value();
                
                let struct_type = bc_llvm_type(
                    global_state, 
                    struct_type
                )?.into_struct_type();
                
                let field_ptr = function_state.builder
                    .build_struct_gep(
                        struct_type,
                        struct_ptr,
                        *field_index as u32,
                        inst_num().as_str()
                    )
                    .map_err(|e| panic!("{}", e))
                    .unwrap();
                
                CodegenValue::Value(field_ptr.as_any_value_enum())
            },
            
            VirtualInstruction::Trunc { value } => {
                let value = function_state    
                    .get_val_ref(value)?
                    .get_value()
                    .into_int_value();
                
                let to_type = bc_llvm_type(
                    global_state, 
                    &block_instruction.value.type_
                )?.into_int_type();
                
                CodegenValue::Value(
                    function_state.builder
                        .build_int_truncate(value, to_type, inst_num().as_str())
                        .ok()?
                        .as_any_value_enum()
                )
            },
            
            VirtualInstruction::GetFunctionAddr { func_name } => {
                let function_name = function_state
                    .get_val_ref(func_name)?
                    .get_function_ref();
                
                let function_val = global_state
                    .module
                    .get_function(function_name)
                    .unwrap()
                    .as_global_value()
                    .as_pointer_value();
                
                // This might be a bug, but calling as_any_value_enum() on a pointer to
                // a function returns a FunctionValue instead of a PointerValue.
                let any_value_enum = AnyValueEnum::PointerValue(function_val);
                
                CodegenValue::Value(any_value_enum)
            },
            
            VirtualInstruction::IntToFloat { from, value } => {
                let value = function_state
                    .get_val_ref(value)?
                    .get_value()
                    .into_int_value();
                
                let to_type = bc_llvm_type(
                    global_state, 
                    &block_instruction.value.type_
                )?.into_float_type();
                
                CodegenValue::Value(
                    match from.kind {
                        BCTypeKind::Signed { .. } =>
                            function_state.builder
                                .build_signed_int_to_float(value, to_type, inst_num().as_str())
                                .ok()?
                                .as_any_value_enum(),
                        
                        BCTypeKind::Unsigned { .. } =>
                            function_state.builder
                                .build_unsigned_int_to_float(value, to_type, inst_num().as_str())
                                .ok()?
                                .as_any_value_enum(),

                        _ => unreachable!()
                    }
                )
            },
            
            VirtualInstruction::FloatToInt { value, .. } => {
                let value = function_state
                    .get_val_ref(value)?
                    .get_value()
                    .into_float_value();
                
                let to_type = bc_llvm_type(
                    global_state, 
                    &block_instruction.value.type_
                )?.into_int_type();
                
                CodegenValue::Value(
                    match block_instruction.value.type_.kind {
                        BCTypeKind::Signed { .. } =>
                            function_state.builder
                                .build_float_to_signed_int(value, to_type, inst_num().as_str())
                                .ok()?
                                .as_any_value_enum(),
                        
                        BCTypeKind::Unsigned { .. } =>
                            function_state.builder
                                .build_float_to_unsigned_int(value, to_type, inst_num().as_str())
                                .ok()?
                                .as_any_value_enum(),
                        
                        _ => unreachable!()
                    }
                )
            },
            
            VirtualInstruction::PtrToInt { value } => {
                let value = function_state
                    .get_val_ref(value)?
                    .get_value()
                    .into_pointer_value();
                
                let to_type = bc_llvm_type(
                    global_state, 
                    &block_instruction.value.type_
                )?.into_int_type();
                
                CodegenValue::Value(
                    function_state.builder
                        .build_ptr_to_int(value, to_type, inst_num().as_str())
                        .ok()?
                        .as_any_value_enum()
                )
            },
            
            VirtualInstruction::FloatCast { value } => {
                let value = function_state
                    .get_val_ref(value)?
                    .get_value()
                    .into_float_value();
                
                let to_type = bc_llvm_type(
                    global_state, 
                    &block_instruction.value.type_
                )?.into_float_type();
                
                CodegenValue::Value(
                    function_state.builder
                        .build_float_cast(value, to_type, inst_num().as_str())
                        .ok()?
                        .as_any_value_enum()
                )
            },
            
            VirtualInstruction::NOP => {
                // NOP instruction does nothing, just return NULL
                CodegenValue::NULL
            },
        }
    )
}