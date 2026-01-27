use crate::arithmetic::{generate_int_binop, generate_ptr_binop};
use crate::attributes::attr_noundef;
use crate::routines::get_function;
use crate::typing::{any_to_basic_type, any_to_basic_val, bc_llvm_prototype, bc_llvm_type};
use crate::{CodegenValue, FunctionState, GlobalState};
use cx_lmir::{
    LMIRCoercionType, LMIRFloatBinOp, LMIRFloatUnOp, LMIRInstruction, LMIRInstructionKind, LMIRIntUnOp,
};
use cx_util::log_error;
use inkwell::AddressSpace;
use inkwell::attributes::AttributeLoc;
use inkwell::values::{AnyValue, AnyValueEnum, ValueKind};
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
    block_instruction: &LMIRInstruction,
) -> Option<CodegenValue<'a>> {
    Some(match &block_instruction.kind {
        LMIRInstructionKind::Alias { value } => function_state.get_value(value)?,

        LMIRInstructionKind::Allocate { _type, alignment } => {
            let inst = function_state
                .builder
                .build_alloca(
                    any_to_basic_type(bc_llvm_type(global_state.context, _type)?)?,
                    inst_num().as_str(),
                )
                .unwrap()
                .as_any_value_enum();

            function_state
                .builder
                .get_insert_block()?
                .get_last_instruction()?
                .set_alignment(*alignment as u32)
                .unwrap();

            CodegenValue::Value(inst)
        }

        LMIRInstructionKind::DirectCall { args, method_sig } => {
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
                ValueKind::Basic(val) => CodegenValue::Value(val.as_any_value_enum()),
                ValueKind::Instruction(_) => CodegenValue::NULL,
            }
        }

        LMIRInstructionKind::IndirectCall {
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
                ValueKind::Basic(val) => CodegenValue::Value(val.as_any_value_enum()),
                ValueKind::Instruction(_) => CodegenValue::NULL,
            }
        }

        LMIRInstructionKind::Coercion {
            value,
            coercion_type: LMIRCoercionType::BitCast,
        } => {
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

        LMIRInstructionKind::Coercion {
            value,
            coercion_type: LMIRCoercionType::IntToPtr { .. },
        } => {
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

        LMIRInstructionKind::Jump { target } => {
            function_state
                .builder
                .build_unconditional_branch(function_state.get_block(target).unwrap())
                .unwrap();

            CodegenValue::NULL
        }

        LMIRInstructionKind::Return { value } => {
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

        LMIRInstructionKind::Store {
            value,
            _type,
            memory,
        } => {
            let any_value = function_state.get_value(value).unwrap().get_value();
            let basic_val = any_to_basic_val(any_value)
                .unwrap_or_else(|| panic!("Failed to convert value {any_value:?} to basic value"));

            let memory_val = function_state
                .get_value(memory)?
                .get_value()
                .into_pointer_value();

            if _type.is_structure() {
            } else {
                function_state
                    .builder
                    .build_store(memory_val, basic_val)
                    .unwrap();
            }

            CodegenValue::NULL
        }

        LMIRInstructionKind::Memcpy {
            dest,
            src,
            size,
            alignment: _,
        } => {
            let src_val = function_state
                .get_value(src)?
                .get_value()
                .into_pointer_value();
            let dest_val = function_state
                .get_value(dest)?
                .get_value()
                .into_pointer_value();
            let size_val = function_state.get_value(size)?.get_value().into_int_value();

            function_state
                .builder
                .build_memcpy(dest_val, 1, src_val, 1, size_val)
                .unwrap();

            CodegenValue::NULL
        }

        LMIRInstructionKind::Load { memory, _type } => {
            let memory_val = function_state
                .get_value(memory)?
                .get_value()
                .into_pointer_value();

            let loaded_value = function_state
                .builder
                .build_load(
                    any_to_basic_type(bc_llvm_type(global_state.context, _type)?)?,
                    memory_val,
                    inst_num().as_str(),
                )
                .unwrap()
                .as_any_value_enum();

            CodegenValue::Value(loaded_value)
        }

        LMIRInstructionKind::ZeroMemory { memory, _type } => {
            let any_value = function_state
                .get_value(memory)?
                .get_value()
                .into_pointer_value();

            let zero = global_state.context.i8_type().const_zero();

            let size = _type.size();
            let size_value = global_state
                .context
                .i32_type()
                .const_int(size as u64, false);

            function_state
                .builder
                .build_memset(any_value, 1, zero, size_value)
                .unwrap();
            CodegenValue::NULL
        }

        LMIRInstructionKind::PointerBinOp {
            left,
            ptr_type: _,
            type_padded_size,
            right,
            op,
            ..
        } => {
            let left_value = function_state.get_value(left).unwrap().get_value();
            let right_value = function_state.get_value(right).unwrap().get_value();

            generate_ptr_binop(
                global_state,
                function_state,
                *type_padded_size,
                left_value,
                right_value,
                *op,
            )?
        }

        LMIRInstructionKind::IntegerUnOp { value, op } => {
            let value = function_state
                .get_value(value)?
                .get_value()
                .into_int_value();

            CodegenValue::Value(match op {
                // LMIRIntUnOp::NEG if signed => function_state
                //     .builder
                //     .build_int_nsw_neg(value, inst_num().as_str())
                //     .unwrap()
                //     .as_any_value_enum(),
                LMIRIntUnOp::NEG => function_state
                    .builder
                    .build_int_neg(value, inst_num().as_str())
                    .unwrap()
                    .as_any_value_enum(),
                LMIRIntUnOp::BNOT => function_state
                    .builder
                    .build_not(value, inst_num().as_str())
                    .unwrap()
                    .as_any_value_enum(),
                LMIRIntUnOp::LNOT => function_state
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

        LMIRInstructionKind::IntegerBinOp { left, right, op } => {
            let left = function_state.get_value(left)?.get_value().into_int_value();

            let right = function_state
                .get_value(right)?
                .get_value()
                .into_int_value();

            generate_int_binop(global_state, function_state, left, right, *op)?
        }

        LMIRInstructionKind::FloatUnOp { value, op } => {
            let value = function_state
                .get_value(value)?
                .get_value()
                .into_float_value();

            CodegenValue::Value(match op {
                LMIRFloatUnOp::NEG => function_state
                    .builder
                    .build_float_neg(value, inst_num().as_str())
                    .unwrap()
                    .as_any_value_enum(),
            })
        }

        LMIRInstructionKind::FloatBinOp { left, right, op } => {
            let left_value = function_state
                .get_value(left)?
                .get_value()
                .into_float_value();

            let right_value = function_state
                .get_value(right)?
                .get_value()
                .into_float_value();

            CodegenValue::Value(match op {
                LMIRFloatBinOp::ADD => function_state
                    .builder
                    .build_float_add(left_value, right_value, inst_num().as_str())
                    .unwrap()
                    .as_any_value_enum(),

                LMIRFloatBinOp::SUB => function_state
                    .builder
                    .build_float_sub(left_value, right_value, inst_num().as_str())
                    .unwrap()
                    .as_any_value_enum(),

                LMIRFloatBinOp::FMUL => function_state
                    .builder
                    .build_float_mul(left_value, right_value, inst_num().as_str())
                    .unwrap()
                    .as_any_value_enum(),

                LMIRFloatBinOp::FDIV => function_state
                    .builder
                    .build_float_div(left_value, right_value, inst_num().as_str())
                    .unwrap()
                    .as_any_value_enum(),

                LMIRFloatBinOp::EQ
                | LMIRFloatBinOp::NEQ
                | LMIRFloatBinOp::FLT
                | LMIRFloatBinOp::FLE
                | LMIRFloatBinOp::FGT
                | LMIRFloatBinOp::FGE => {
                    let predicate = match op {
                        LMIRFloatBinOp::EQ => inkwell::FloatPredicate::OEQ,
                        LMIRFloatBinOp::NEQ => inkwell::FloatPredicate::ONE,
                        LMIRFloatBinOp::FLT => inkwell::FloatPredicate::OLT,
                        LMIRFloatBinOp::FLE => inkwell::FloatPredicate::OLE,
                        LMIRFloatBinOp::FGT => inkwell::FloatPredicate::OGT,
                        LMIRFloatBinOp::FGE => inkwell::FloatPredicate::OGE,
                        _ => unreachable!(),
                    };

                    function_state
                        .builder
                        .build_float_compare(
                            predicate,
                            left_value,
                            right_value,
                            inst_num().as_str(),
                        )
                        .unwrap()
                        .as_any_value_enum()
                }
            })
        }

        LMIRInstructionKind::Phi { predecessors: from } => {
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

                let block = function_state.get_block(block_id).unwrap();

                phi_node.add_incoming(&[(&value, block)]);
            }

            CodegenValue::Value(phi_node.as_any_value_enum())
        }

        LMIRInstructionKind::Branch {
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

            let true_block_val = function_state.get_block(true_block).unwrap();
            let false_block_val = function_state.get_block(false_block).unwrap();

            function_state
                .builder
                .build_conditional_branch(condition_value, true_block_val, false_block_val)
                .unwrap();

            CodegenValue::NULL
        }

        LMIRInstructionKind::JumpTable {
            value,
            targets,
            default,
        } => {
            let value = function_state
                .get_value(value)?
                .get_value()
                .into_int_value();
            let value_type = value.get_type();

            let targets = targets
                .iter()
                .map(|(value, block)| {
                    let value = value_type.const_int(*value, false);
                    let block = function_state.get_block(block).unwrap();

                    (value, block)
                })
                .collect::<Vec<_>>();

            function_state
                .builder
                .build_switch(
                    value,
                    function_state.get_block(default).unwrap(),
                    targets.as_slice(),
                )
                .unwrap();

            CodegenValue::NULL
        }

        LMIRInstructionKind::Coercion {
            value,
            coercion_type: LMIRCoercionType::ZExtend,
        } => {
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

        LMIRInstructionKind::Coercion {
            value,
            coercion_type: LMIRCoercionType::SExtend,
        } => {
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

        LMIRInstructionKind::StructAccess {
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
                .expect("Failed to build struct GEP");

            CodegenValue::Value(field_ptr.as_any_value_enum())
        }

        LMIRInstructionKind::Coercion {
            value,
            coercion_type: LMIRCoercionType::Trunc,
        } => {
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

        LMIRInstructionKind::GetFunctionAddr { func } => {
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

        LMIRInstructionKind::Coercion {
            value,
            coercion_type: LMIRCoercionType::IntToFloat { from: _, sextend },
        } => {
            let value = function_state
                .get_value(value)?
                .get_value()
                .into_int_value();

            let to_type = bc_llvm_type(global_state.context, &block_instruction.value_type)?
                .into_float_type();

            CodegenValue::Value(match sextend {
                true => function_state
                    .builder
                    .build_signed_int_to_float(value, to_type, inst_num().as_str())
                    .unwrap()
                    .as_any_value_enum(),

                false => function_state
                    .builder
                    .build_unsigned_int_to_float(value, to_type, inst_num().as_str())
                    .unwrap()
                    .as_any_value_enum(),
            })
        }

        LMIRInstructionKind::Coercion {
            value,
            coercion_type: LMIRCoercionType::FloatToInt { from: _, sextend },
        } => {
            let value = function_state
                .get_value(value)?
                .get_value()
                .into_float_value();

            let to_type =
                bc_llvm_type(global_state.context, &block_instruction.value_type)?.into_int_type();

            CodegenValue::Value(match sextend {
                true => function_state
                    .builder
                    .build_float_to_signed_int(value, to_type, inst_num().as_str())
                    .unwrap()
                    .as_any_value_enum(),

                false => function_state
                    .builder
                    .build_float_to_unsigned_int(value, to_type, inst_num().as_str())
                    .unwrap()
                    .as_any_value_enum(),
            })
        }

        LMIRInstructionKind::Coercion {
            value,
            coercion_type: LMIRCoercionType::PtrToInt,
        } => {
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

        LMIRInstructionKind::Coercion {
            value,
            coercion_type: LMIRCoercionType::FloatCast { .. },
        } => {
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

        LMIRInstructionKind::CompilerAssumption { condition: _ } => {
            // TODO: Implement assumptions in LLVM

            CodegenValue::NULL
        }
    })
}
