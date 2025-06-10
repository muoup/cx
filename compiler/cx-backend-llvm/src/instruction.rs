use crate::arithmetic::generate_int_binop;
use crate::attributes::noundef;
use crate::mangling::string_literal_name;
use crate::typing::{any_to_basic_type, any_to_basic_val, bc_llvm_prototype, cx_llvm_type};
use crate::{CodegenValue, FunctionState, GlobalState};
use cx_data_bytecode::types::BCTypeKind;
use cx_data_bytecode::{BCFloatBinOp, BCFloatUnOp, BCIntUnOp, BlockInstruction, VirtualInstruction};
use inkwell::attributes::AttributeLoc;
use inkwell::types::BasicType;
use inkwell::values::{AnyValue, FunctionValue, IntMathValue};
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
                let val = function_state.value_map
                    .get(value)
                    .cloned()?
                    .get_value();
                let basic_val = any_to_basic_val(val)?;
                
                let bit_cast_type = cx_llvm_type(
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

            VirtualInstruction::Immediate { value } => {
                let imm_type = cx_llvm_type(
                    global_state, 
                    &block_instruction.value.type_
                )?.into_int_type();
                
                CodegenValue::Value(imm_type.const_int(*value as u64, false).as_any_value_enum())
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
            
            VirtualInstruction::FunctionParameter { param_index } => {
                let param = function_val
                    .get_nth_param(*param_index as u32)
                    .unwrap();
                
                CodegenValue::Value(param.as_any_value_enum())
            },
            
            VirtualInstruction::Store { value, type_, memory } => {
                let any_value = function_state
                    .get_val_ref(value)?
                    .get_value();
                let any_type = cx_llvm_type(global_state, type_).unwrap();

                let basic_val = any_to_basic_val(any_value)?;
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
                        .ok()?;
                }
                
                CodegenValue::NULL
            },
            
            VirtualInstruction::Load { value } => {
                let any_value = function_state
                    .get_val_ref(value)?
                    .get_value()
                    .into_pointer_value();
                
                let loaded_type = cx_llvm_type(
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
            
            VirtualInstruction::AddressOf { value } => {
                CodegenValue::Value(
                    function_state
                        .get_val_ref(value)?
                        .get_value()
                        .as_any_value_enum()
                )
            },
            
            VirtualInstruction::ZExtend { value } => {
                let value = function_state
                    .get_val_ref(value)?
                    .get_value()
                    .into_int_value();
                let to_type = cx_llvm_type(
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
                let to_type = cx_llvm_type(
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
                
                let struct_type = cx_llvm_type(
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
                
                let to_type = cx_llvm_type(
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
                    .unwrap();
                
                CodegenValue::Value(
                    function_val.as_any_value_enum()
                )
            },
            
            VirtualInstruction::IntToFloat { from, value } => {
                let value = function_state
                    .get_val_ref(value)?
                    .get_value()
                    .into_int_value();
                
                let to_type = cx_llvm_type(
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
                
                let to_type = cx_llvm_type(
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
            
            VirtualInstruction::FloatCast { value } => {
                let value = function_state
                    .get_val_ref(value)?
                    .get_value()
                    .into_float_value();
                
                let to_type = cx_llvm_type(
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