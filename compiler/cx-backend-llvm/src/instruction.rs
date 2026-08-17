mod calls;
mod control_flow;
mod memory;
mod operations;
mod variadic;

use std::cell::Cell;

use crate::error::LLVMResult;
use crate::{CodegenValue, FunctionState, GlobalState};
use cx_lmir::{LMIRInstruction, LMIRInstructionKind};

thread_local! {
    // Modules are compiled single-threaded, but multiple modules can be compiled
    // in parallel, so this counter is thread-local and cannot collide across modules.
    static NUM: Cell<usize> = const { Cell::new(0) };
}

pub(crate) fn reset_num() {
    NUM.with(|num| num.set(0));
}

pub(crate) fn inst_num() -> String {
    NUM.with(|num| {
        let current = num.get();
        num.set(current + 1);
        format!("inst_{current}")
    })
}

pub(crate) fn generate_instruction<'a, 'b>(
    global_state: &GlobalState<'a>,
    function_state: &FunctionState<'a, 'b>,
    instruction: &LMIRInstruction,
) -> LLVMResult<CodegenValue<'a>> {
    match &instruction.kind {
        LMIRInstructionKind::Alias { value } => function_state.get_value(value),
        LMIRInstructionKind::GetFunctionAddr { func } => {
            calls::generate_get_function_addr(global_state, func)
        }
        LMIRInstructionKind::Allocate { _type, alignment } => {
            memory::generate_allocate(function_state, _type, *alignment)
        }
        LMIRInstructionKind::StructAccess {
            struct_,
            field_index,
            struct_type,
            ..
        } => memory::generate_struct_access(function_state, struct_, struct_type, *field_index),
        LMIRInstructionKind::Store {
            value,
            _type,
            memory,
        } => memory::generate_store(global_state, function_state, memory, value, _type),
        LMIRInstructionKind::Memcpy {
            dest,
            src,
            size,
            alignment,
        } => memory::generate_memcpy(function_state, dest, src, size, *alignment),
        LMIRInstructionKind::Load { memory, _type } => {
            memory::generate_load(global_state, function_state, memory, _type)
        }
        LMIRInstructionKind::ZeroMemory { memory, _type } => {
            memory::generate_zero_memory(global_state, function_state, memory, _type)
        }
        LMIRInstructionKind::DirectCall {
            func,
            args,
            method_sig,
        } => calls::generate_direct_call(global_state, function_state, func, args, method_sig),
        LMIRInstructionKind::IndirectCall {
            func_ptr,
            args,
            method_sig,
        } => {
            calls::generate_indirect_call(global_state, function_state, func_ptr, args, method_sig)
        }
        LMIRInstructionKind::VaStart { list, .. } => {
            variadic::generate_va_start(global_state, function_state, list)
        }
        LMIRInstructionKind::VaEnd { list } => {
            variadic::generate_va_end(global_state, function_state, list)
        }
        LMIRInstructionKind::VaArg { list, _type } => {
            variadic::generate_va_arg(global_state, function_state, list, _type)
        }
        LMIRInstructionKind::Coercion {
            value,
            coercion_type,
        } => match coercion_type {
            cx_lmir::LMIRCoercionType::BitCast => operations::generate_bit_cast(
                global_state,
                function_state,
                value,
                &instruction.value_type,
            ),
            cx_lmir::LMIRCoercionType::IntToPtr { .. } => {
                operations::generate_int_to_ptr(global_state, function_state, value)
            }
            cx_lmir::LMIRCoercionType::ZExtend => operations::generate_zextend(
                global_state,
                function_state,
                value,
                &instruction.value_type,
            ),
            cx_lmir::LMIRCoercionType::SExtend => operations::generate_sextend(
                global_state,
                function_state,
                value,
                &instruction.value_type,
            ),
            cx_lmir::LMIRCoercionType::Trunc => operations::generate_trunc(
                global_state,
                function_state,
                value,
                &instruction.value_type,
            ),
            cx_lmir::LMIRCoercionType::IntToFloat { sextend, .. } => {
                operations::generate_int_to_float(
                    global_state,
                    function_state,
                    value,
                    &instruction.value_type,
                    *sextend,
                )
            }
            cx_lmir::LMIRCoercionType::FloatToInt { sextend, .. } => {
                operations::generate_float_to_int(
                    global_state,
                    function_state,
                    value,
                    &instruction.value_type,
                    *sextend,
                )
            }
            cx_lmir::LMIRCoercionType::PtrToInt => operations::generate_ptr_to_int(
                global_state,
                function_state,
                value,
                &instruction.value_type,
            ),
            cx_lmir::LMIRCoercionType::FloatCast { .. } => operations::generate_float_cast(
                global_state,
                function_state,
                value,
                &instruction.value_type,
            ),
        },
        LMIRInstructionKind::PointerBinOp {
            left,
            type_size,
            right,
            op,
            ..
        } => operations::generate_pointer_binop(
            global_state,
            function_state,
            left,
            right,
            *type_size,
            *op,
        ),
        LMIRInstructionKind::IntegerBinOp { left, right, op } => {
            operations::generate_integer_binop(global_state, function_state, left, right, *op)
        }
        LMIRInstructionKind::IntegerUnOp { value, op } => {
            operations::generate_integer_unop(function_state, value, *op)
        }
        LMIRInstructionKind::FloatBinOp { left, right, op } => {
            operations::generate_float_binop(function_state, left, right, *op)
        }
        LMIRInstructionKind::FloatUnOp { value, op } => {
            operations::generate_float_unop(function_state, value, *op)
        }
        LMIRInstructionKind::Branch {
            condition,
            true_target,
            false_target,
        } => control_flow::generate_branch(
            global_state,
            function_state,
            condition,
            true_target,
            false_target,
        ),
        LMIRInstructionKind::Jump { target } => control_flow::generate_jump(function_state, target),
        LMIRInstructionKind::JumpTable {
            value,
            targets,
            default,
        } => {
            control_flow::generate_jump_table(global_state, function_state, value, targets, default)
        }
        LMIRInstructionKind::Return { value } => {
            control_flow::generate_return(global_state, function_state, value.as_ref())
        }
        LMIRInstructionKind::CompilerAssumption { .. } => {
            // TODO: Implement assumptions in LLVM.
            Ok(CodegenValue::Null)
        }
        LMIRInstructionKind::Unreachable => control_flow::generate_unreachable(function_state),
    }
}
