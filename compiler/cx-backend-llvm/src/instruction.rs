use crate::arithmetic::{generate_int_binop, generate_ptr_binop};
use crate::attributes::attr_noundef;
use crate::routines::get_function;
use crate::typing::{any_to_basic_type, any_to_basic_val, bc_llvm_prototype, bc_llvm_type};
use crate::{CodegenValue, FunctionState, GlobalState};
use cx_mir_data::types::{MIRTypeKind, MIRTypeSize};
use cx_mir_data::{
    BCFloatBinOp, BCFloatUnOp, BCIntUnOp, BlockID, BlockInstruction, VirtualInstruction,
};
use cx_util::log_error;
use inkwell::attributes::AttributeLoc;
use inkwell::values::{AnyValue, AnyValueEnum};
use inkwell::{AddressSpace, Either};
use std::sync::Mutex;

static NUM: Mutex<usize> = Mutex::new(0);

pub(crate) fn reset_num() {
    let mut num = NUM.lock().unwrap();
    *num = 0;
}

pub(crate) fn inst_num() -> String {
    let mut num = NUM.lock().unwrap();
    *num += 1;

    format!("inst_{}", *num)
}

pub(crate) fn generate_instruction<'a, 'b>(
    global_state: &GlobalState<'a>,
    function_state: &FunctionState<'a, 'b>,
    block_instruction: &BlockInstruction,
) -> Option<CodegenValue<'a>> {
    Some(match &block_instruction.instruction {
        VirtualInstruction::Temp { value } => function_state.get_value(value)?,

        VirtualInstruction::Allocate { _type, alignment } => {
            let inst = match _type.size() {
                MIRTypeSize::Fixed(_) => function_state
                    .builder
                    .build_alloca(
                        any_to_basic_type(bc_llvm_type(global_state.context, _type)?)?,
                        inst_num().as_str(),
                    )
                    .unwrap()
                    .as_any_value_enum(),
                MIRTypeSize::Variable(size) => {
                    let size = function_state
                        .get_value(&size)?
                        .get_value()
                        .into_int_value();

                    function_state
                        .builder
                        .build_array_alloca(
                            global_state.context.i8_type(),
                            size,
                            inst_num().as_str(),
                        )
                        .unwrap()
                        .as_any_value_enum()
                }
            };

            function_state
                .builder
                .get_insert_block()?
                .get_last_instruction()?
                .set_alignment(*alignment as u32)
                .unwrap();

            CodegenValue::Value(inst)
        }

        VirtualInstruction::DirectCall { args, method_sig } => {
            let Some(function_val) = get_function(global_state, method_sig) else {
                log_error!("Function not found in module: {}", method_sig.name);
            };

            let arg_vals = args
                .iter()
                .map(|arg| {
                    let val = function_state.get_value(arg)?.get_value();

                    let basic_val = any_to_basic_val(val)?;

                    Some(basic_val.into())
                })
                .collect::<Option<Vec<_>>>()?;

            let val = function_state
                .builder
                .build_direct_call(function_val, arg_vals.as_slice(), inst_num().as_str())
                .unwrap();

            for i in 0..args.len() {
                val.add_attribute(
                    AttributeLoc::Param(i as u32),
                    attr_noundef(global_state.context),
                )
            }

            match val.try_as_basic_value() {
                Either::Left(val) => CodegenValue::Value(val.as_any_value_enum()),
                Either::Right(_) => CodegenValue::NULL,
            }
        }

        VirtualInstruction::IndirectCall {
            func_ptr,
            args,
            method_sig,
        } => {
            let ptr = function_state.get_value(func_ptr)?.get_value();
            let fn_type = bc_llvm_prototype(global_state, method_sig).unwrap();
            let args = args
                .iter()
                .map(|arg| {
                    let val = function_state.get_value(arg)?.get_value();

                    let basic_val = any_to_basic_val(val)?;

                    Some(basic_val.into())
                })
                .collect::<Option<Vec<_>>>()?;

            let val = function_state
                .builder
                .build_indirect_call(
                    fn_type,
                    ptr.into_pointer_value(),
                    args.as_slice(),
                    inst_num().as_str(),
                )
                .unwrap();

            match val.try_as_basic_value() {
                Either::Left(val) => CodegenValue::Value(val.as_any_value_enum()),
                Either::Right(_) => CodegenValue::NULL,
            }
        }

        VirtualInstruction::BitCast { value } => {
            let val = function_state.get_value(value)?.get_value();
            let basic_val = any_to_basic_val(val)?;

            let bit_cast_type = bc_llvm_type(global_state.context, &block_instruction.value_type)?;
            let basic_type = any_to_basic_type(bit_cast_type).unwrap();

            CodegenValue::Value(
                function_state
                    .builder
                    .build_bit_cast(basic_val, basic_type, inst_num().as_str())
                    .unwrap()
                    .as_any_value_enum(),
            )
        }

        VirtualInstruction::IntToPtrDiff { value, .. } => function_state.get_value(value)?.clone(),

        VirtualInstruction::IntToPtr { value } => {
            let value = function_state
                .get_value(value)?
                .get_value()
                .into_int_value();

            let to_type = global_state.context.ptr_type(AddressSpace::from(0));

            CodegenValue::Value(
                function_state
                    .builder
                    .build_int_to_ptr(value, to_type, inst_num().as_str())
                    .unwrap()
                    .as_any_value_enum(),
            )
        }

        VirtualInstruction::GotoDefer => {
            let defer_block = function_state.get_block(BlockID::DeferredBlock(0)).unwrap();

            function_state
                .builder
                .build_unconditional_branch(defer_block)
                .unwrap();

            CodegenValue::NULL
        }

        VirtualInstruction::Jump { target } => {
            function_state
                .builder
                .build_unconditional_branch(function_state.get_block(*target).unwrap())
                .unwrap();

            CodegenValue::NULL
        }

        VirtualInstruction::Return { value } => {
            let Some(value) = value else {
                function_state.builder.build_return(None).unwrap();

                return Some(CodegenValue::NULL);
            };

            let value = function_state.get_value(value).unwrap();

            let basic_val = any_to_basic_val(value.get_value())?;

            function_state
                .builder
                .build_return(Some(&basic_val))
                .unwrap();

            CodegenValue::NULL
        }

        VirtualInstruction::Store {
            value,
            type_,
            memory,
        } => {
            let any_value = function_state.get_value(value).unwrap().get_value();
            let basic_val = any_to_basic_val(any_value)
                .unwrap_or_else(|| panic!("Failed to convert value {any_value:?} to basic value"));

            let memory_val = function_state
                .get_value(memory)?
                .get_value()
                .into_pointer_value();

            if type_.is_structure() {
                function_state
                    .builder
                    .build_memcpy(
                        memory_val,
                        1,
                        basic_val.into_pointer_value(),
                        1,
                        global_state
                            .context
                            .i64_type()
                            .const_int(type_.fixed_size() as u64, false),
                    )
                    .unwrap();
            } else {
                function_state
                    .builder
                    .build_store(memory_val, basic_val)
                    .unwrap();
            }

            CodegenValue::NULL
        }

        VirtualInstruction::ZeroMemory { memory, _type } => {
            let any_value = function_state
                .get_value(memory)?
                .get_value()
                .into_pointer_value();

            let zero = global_state.context.i8_type().const_zero();

            match _type.size() {
                MIRTypeSize::Fixed(size) => {
                    let size_value = global_state
                        .context
                        .i32_type()
                        .const_int(size as u64, false);

                    function_state
                        .builder
                        .build_memset(any_value, 1, zero, size_value)
                        .unwrap();
                }
                MIRTypeSize::Variable(size) => {
                    let size_value = function_state
                        .get_value(&size)?
                        .get_value()
                        .into_int_value();

                    function_state
                        .builder
                        .build_memset(any_value, 1, zero, size_value)
                        .unwrap();
                }
            }

            CodegenValue::NULL
        }

        VirtualInstruction::PointerBinOp {
            left,
            ptr_type,
            right,
            op,
        } => {
            let left_value = function_state.get_value(left).unwrap().get_value();

            let right_value = function_state.get_value(right).unwrap().get_value();

            generate_ptr_binop(
                global_state,
                function_state,
                ptr_type,
                left_value,
                right_value,
                *op,
            )?
        }

        VirtualInstruction::IntegerUnOp { value, op } => {
            let value = function_state
                .get_value(value)?
                .get_value()
                .into_int_value();

            let signed = match block_instruction.value_type.kind {
                MIRTypeKind::Signed { .. } => true,

                MIRTypeKind::Bool | MIRTypeKind::Unsigned { .. } => false,

                _ => unreachable!("Invalid type for IntegerUnOp"), // Unsupported type for IntegerUnOp
            };

            CodegenValue::Value(match op {
                BCIntUnOp::NEG if signed => function_state
                    .builder
                    .build_int_nsw_neg(value, inst_num().as_str())
                    .unwrap()
                    .as_any_value_enum(),
                BCIntUnOp::NEG => function_state
                    .builder
                    .build_int_neg(value, inst_num().as_str())
                    .unwrap()
                    .as_any_value_enum(),
                BCIntUnOp::BNOT => function_state
                    .builder
                    .build_not(value, inst_num().as_str())
                    .unwrap()
                    .as_any_value_enum(),
                BCIntUnOp::LNOT => function_state
                    .builder
                    .build_int_compare(
                        inkwell::IntPredicate::EQ,
                        value,
                        value.get_type().const_int(0, false),
                        inst_num().as_str(),
                    )
                    .unwrap()
                    .as_any_value_enum(),
            })
        }

        VirtualInstruction::IntegerBinOp { left, right, op } => {
            let left = function_state.get_value(left)?.get_value().into_int_value();

            let right = function_state
                .get_value(right)?
                .get_value()
                .into_int_value();

            let signed = match block_instruction.value_type.kind {
                MIRTypeKind::Signed { .. } => true,
                MIRTypeKind::Unsigned { .. } => false,
                MIRTypeKind::Bool => false,

                _ => log_error!("Unsupported type for IntegerBinOp"),
            };

            generate_int_binop(global_state, function_state, left, right, *op, signed)?
        }

        VirtualInstruction::FloatUnOp { value, op } => {
            let value = function_state
                .get_value(value)?
                .get_value()
                .into_float_value();

            CodegenValue::Value(match op {
                BCFloatUnOp::NEG => function_state
                    .builder
                    .build_float_neg(value, inst_num().as_str())
                    .unwrap()
                    .as_any_value_enum(),
            })
        }

        VirtualInstruction::FloatBinOp { left, right, op } => {
            let left_value = function_state
                .get_value(left)?
                .get_value()
                .into_float_value();

            let right_value = function_state
                .get_value(right)?
                .get_value()
                .into_float_value();

            CodegenValue::Value(match op {
                BCFloatBinOp::ADD => function_state
                    .builder
                    .build_float_add(left_value, right_value, inst_num().as_str())
                    .unwrap()
                    .as_any_value_enum(),

                BCFloatBinOp::SUB => function_state
                    .builder
                    .build_float_sub(left_value, right_value, inst_num().as_str())
                    .unwrap()
                    .as_any_value_enum(),

                BCFloatBinOp::FMUL => function_state
                    .builder
                    .build_float_mul(left_value, right_value, inst_num().as_str())
                    .unwrap()
                    .as_any_value_enum(),

                BCFloatBinOp::FDIV => function_state
                    .builder
                    .build_float_div(left_value, right_value, inst_num().as_str())
                    .unwrap()
                    .as_any_value_enum(),
            })
        }

        VirtualInstruction::Phi { predecessors: from } => {
            let val_type = bc_llvm_type(global_state.context, &block_instruction.value_type)?;
            let as_basic_type =
                any_to_basic_type(val_type).expect("Failed to convert value type to basic type");

            let phi_node = function_state
                .builder
                .build_phi(as_basic_type, inst_num().as_str())
                .unwrap();

            for (value_id, block_id) in from {
                let value = function_state.get_value(value_id)?.get_value();
                let value =
                    any_to_basic_val(value).expect("Failed to convert value to basic value");

                let block = function_state.get_block(*block_id).unwrap();

                phi_node.add_incoming(&[(&value, block)]);
            }

            CodegenValue::Value(phi_node.as_any_value_enum())
        }

        VirtualInstruction::Branch {
            condition,
            true_block,
            false_block,
        } => {
            let mut condition_value = function_state
                .get_value(condition)?
                .get_value()
                .into_int_value();

            if condition_value.get_type().get_bit_width() > 1 {
                condition_value = function_state
                    .builder
                    .build_int_truncate(
                        condition_value,
                        global_state.context.bool_type(),
                        inst_num().as_str(),
                    )
                    .unwrap();
            }

            let true_block_val = function_state.get_block(*true_block).unwrap();

            let false_block_val = function_state.get_block(*false_block).unwrap();

            function_state
                .builder
                .build_conditional_branch(condition_value, true_block_val, false_block_val)
                .unwrap();

            CodegenValue::NULL
        }

        VirtualInstruction::JumpTable {
            value,
            targets,
            default,
        } => {
            let value = function_state
                .get_value(value)?
                .get_value()
                .into_int_value();

            let targets = targets
                .iter()
                .map(|(value, block)| {
                    let value = global_state.context.i32_type().const_int(*value, false);
                    let block = function_state.get_block(*block).unwrap();

                    (value, block)
                })
                .collect::<Vec<_>>();

            function_state
                .builder
                .build_switch(
                    value,
                    function_state.get_block(*default).unwrap(),
                    targets.as_slice(),
                )
                .unwrap();

            CodegenValue::NULL
        }

        VirtualInstruction::BoolExtend { value } | VirtualInstruction::ZExtend { value } => {
            let value = function_state
                .get_value(value)?
                .get_value()
                .into_int_value();
            let to_type =
                bc_llvm_type(global_state.context, &block_instruction.value_type)?.into_int_type();

            CodegenValue::Value(
                function_state
                    .builder
                    .build_int_z_extend(value, to_type, inst_num().as_str())
                    .unwrap()
                    .as_any_value_enum(),
            )
        }

        VirtualInstruction::SExtend { value } => {
            let value = function_state
                .get_value(value)?
                .get_value()
                .into_int_value();
            let to_type =
                bc_llvm_type(global_state.context, &block_instruction.value_type)?.into_int_type();

            CodegenValue::Value(
                function_state
                    .builder
                    .build_int_s_extend(value, to_type, inst_num().as_str())
                    .unwrap()
                    .as_any_value_enum(),
            )
        }

        VirtualInstruction::StructAccess {
            struct_,
            struct_type,
            field_index,
            ..
        } => {
            let struct_ptr = function_state
                .get_value(struct_)?
                .get_value()
                .into_pointer_value();

            let struct_type = bc_llvm_type(global_state.context, struct_type)?.into_struct_type();

            let field_ptr = function_state
                .builder
                .build_struct_gep(
                    struct_type,
                    struct_ptr,
                    *field_index as u32,
                    inst_num().as_str(),
                )
                .map_err(|e| panic!("{}", e))
                .unwrap();

            CodegenValue::Value(field_ptr.as_any_value_enum())
        }

        VirtualInstruction::Trunc { value } => {
            let value = function_state
                .get_value(value)?
                .get_value()
                .into_int_value();

            let to_type =
                bc_llvm_type(global_state.context, &block_instruction.value_type)?.into_int_type();

            CodegenValue::Value(
                function_state
                    .builder
                    .build_int_truncate(value, to_type, inst_num().as_str())
                    .unwrap()
                    .as_any_value_enum(),
            )
        }

        VirtualInstruction::GetFunctionAddr { func } => {
            let function_val = global_state
                .module
                .get_function(func)
                .unwrap()
                .as_global_value()
                .as_pointer_value();

            // This might be a bug, but calling as_any_value_enum() on a pointer to
            // a function returns a FunctionValue instead of a PointerValue.
            let any_value_enum = AnyValueEnum::PointerValue(function_val);

            CodegenValue::Value(any_value_enum)
        }

        VirtualInstruction::IntToFloat { from, value } => {
            let value = function_state
                .get_value(value)?
                .get_value()
                .into_int_value();

            let to_type = bc_llvm_type(global_state.context, &block_instruction.value_type)?
                .into_float_type();

            CodegenValue::Value(match from.kind {
                MIRTypeKind::Signed { .. } => function_state
                    .builder
                    .build_signed_int_to_float(value, to_type, inst_num().as_str())
                    .unwrap()
                    .as_any_value_enum(),

                MIRTypeKind::Unsigned { .. } => function_state
                    .builder
                    .build_unsigned_int_to_float(value, to_type, inst_num().as_str())
                    .unwrap()
                    .as_any_value_enum(),

                _ => unreachable!(),
            })
        }

        VirtualInstruction::FloatToInt { value, .. } => {
            let value = function_state
                .get_value(value)?
                .get_value()
                .into_float_value();

            let to_type =
                bc_llvm_type(global_state.context, &block_instruction.value_type)?.into_int_type();

            CodegenValue::Value(match block_instruction.value_type.kind {
                MIRTypeKind::Signed { .. } => function_state
                    .builder
                    .build_float_to_signed_int(value, to_type, inst_num().as_str())
                    .unwrap()
                    .as_any_value_enum(),

                MIRTypeKind::Unsigned { .. } => function_state
                    .builder
                    .build_float_to_unsigned_int(value, to_type, inst_num().as_str())
                    .unwrap()
                    .as_any_value_enum(),

                _ => unreachable!(),
            })
        }

        VirtualInstruction::PtrToInt { value } => {
            let value = function_state
                .get_value(value)?
                .get_value()
                .into_pointer_value();

            let to_type =
                bc_llvm_type(global_state.context, &block_instruction.value_type)?.into_int_type();

            CodegenValue::Value(
                function_state
                    .builder
                    .build_ptr_to_int(value, to_type, inst_num().as_str())
                    .unwrap()
                    .as_any_value_enum(),
            )
        }

        VirtualInstruction::FloatCast { value } => {
            let value = function_state
                .get_value(value)?
                .get_value()
                .into_float_value();

            let to_type = bc_llvm_type(global_state.context, &block_instruction.value_type)?
                .into_float_type();

            CodegenValue::Value(
                function_state
                    .builder
                    .build_float_cast(value, to_type, inst_num().as_str())
                    .unwrap()
                    .as_any_value_enum(),
            )
        }

        VirtualInstruction::NOP => {
            // NOP instruction does nothing, just return NULL
            CodegenValue::NULL
        }
    })
}
