use super::inst_num;
use crate::attributes::{attr_alignment, attr_byval, attr_sret};
use crate::routines::get_function;
use crate::typing::{any_to_basic_type, any_to_basic_val, bc_llvm_signature, bc_llvm_type};
use crate::{CodegenValue, FunctionState, GlobalState};
use cx_lmir::{LMIRFunctionSignature, LMIRParameterABI, LMIRReturnABI, LMIRValue};
use cx_util::identifier::CXIdent;
use inkwell::attributes::AttributeLoc;
use inkwell::values::{AnyValue, AnyValueEnum, BasicValue, ValueKind};

pub(super) fn generate_direct_call<'a, 'b>(
    global_state: &GlobalState<'a>,
    function_state: &FunctionState<'a, 'b>,
    func: &CXIdent,
    args: &[LMIRValue],
    method_sig: &LMIRFunctionSignature,
) -> Option<CodegenValue<'a>> {
    let function_val = get_function(global_state, func.as_str(), method_sig)?;
    let Some(arg_vals) = args
        .iter()
        .map(|arg| {
            let val = function_state.get_value(arg)?.get_value();
            let basic_val = match val {
                AnyValueEnum::FunctionValue(value) => {
                    value.as_global_value().as_pointer_value().into()
                }
                value => any_to_basic_val(value)?,
            };
            Some(basic_val.into())
        })
        .collect::<Option<Vec<_>>>()
    else {
        return None;
    };

    let call = function_state
        .builder
        .build_direct_call(function_val, arg_vals.as_slice(), inst_num().as_str())
        .unwrap();
    apply_call_abi_attributes(global_state, &call, method_sig);

    codegen_call_return(function_state, method_sig, &call)
}

pub(super) fn generate_indirect_call<'a, 'b>(
    global_state: &GlobalState<'a>,
    function_state: &FunctionState<'a, 'b>,
    func_ptr: &LMIRValue,
    args: &[LMIRValue],
    method_sig: &LMIRFunctionSignature,
) -> Option<CodegenValue<'a>> {
    let ptr = function_state.get_value(func_ptr)?.get_value();
    let fn_type = bc_llvm_signature(global_state, method_sig)?;
    let arg_vals = args
        .iter()
        .map(|arg| {
            let val = function_state.get_value(arg)?.get_value();
            let basic_val = any_to_basic_val(val)?;
            Some(basic_val.into())
        })
        .collect::<Option<Vec<_>>>()?;

    let call = function_state
        .builder
        .build_indirect_call(
            fn_type,
            ptr.into_pointer_value(),
            arg_vals.as_slice(),
            inst_num().as_str(),
        )
        .unwrap();
    apply_call_abi_attributes(global_state, &call, method_sig);

    codegen_call_return(function_state, method_sig, &call)
}

pub(super) fn codegen_call_return<'a, 'b>(
    function_state: &FunctionState<'a, 'b>,
    method_sig: &LMIRFunctionSignature,
    call: &inkwell::values::CallSiteValue<'a>,
) -> Option<CodegenValue<'a>> {
    let basic = match call.try_as_basic_value() {
        ValueKind::Basic(value) => value,
        ValueKind::Instruction(_) => return Some(CodegenValue::Null),
    };

    match &method_sig.return_abi {
        LMIRReturnABI::Direct { slots } if slots.len() > 1 => {
            let aggregate = basic.into_struct_value();
            let mut values = Vec::new();
            for (index, slot) in slots.iter().enumerate() {
                let value = function_state
                    .builder
                    .build_extract_value(aggregate, index as u32, inst_num().as_str())
                    .unwrap();
                values.push((slot.clone(), value));
            }
            Some(CodegenValue::AggregateSlots(values))
        }
        _ => Some(CodegenValue::Value(basic.as_any_value_enum())),
    }
}

pub(super) fn apply_call_abi_attributes<'a>(
    global_state: &GlobalState<'a>,
    call: &inkwell::values::CallSiteValue<'a>,
    method_sig: &LMIRFunctionSignature,
) -> Option<()> {
    if let LMIRReturnABI::IndirectSret { .. } = &method_sig.return_abi {
        let pointee = bc_llvm_type(global_state.context, &method_sig.return_type)?;
        call.add_attribute(
            AttributeLoc::Param(0),
            attr_sret(global_state.context, pointee),
        );
    }

    let mut index = usize::from(method_sig.return_abi.has_indirect_return_param());
    for parameter in &method_sig.params {
        match &parameter.abi {
            LMIRParameterABI::Direct { slots } => index += slots.len(),
            LMIRParameterABI::Indirect { .. } => index += 1,
            LMIRParameterABI::ByValue { alignment } => {
                let pointee = bc_llvm_type(global_state.context, &parameter._type)?;
                call.add_attribute(
                    AttributeLoc::Param(index as u32),
                    attr_byval(global_state.context, pointee),
                );
                call.add_attribute(
                    AttributeLoc::Param(index as u32),
                    attr_alignment(global_state.context, *alignment),
                );
                index += 1;
            }
        }
    }
    Some(())
}

pub(super) fn build_direct_return_from_memory<'a, 'b>(
    global_state: &GlobalState<'a>,
    function_state: &FunctionState<'a, 'b>,
    memory: inkwell::values::AnyValueEnum<'a>,
) -> Option<inkwell::values::BasicValueEnum<'a>> {
    let LMIRReturnABI::Direct { slots } = &function_state.signature.return_abi else {
        return any_to_basic_val(memory);
    };

    let memory = memory.into_pointer_value();
    if slots.len() == 1 {
        let ty = any_to_basic_type(bc_llvm_type(global_state.context, &slots[0]._type)?)?;
        let loaded = function_state
            .builder
            .build_load(ty, memory, inst_num().as_str())
            .unwrap();
        loaded
            .as_instruction_value()
            .unwrap()
            .set_alignment(slots[0]._type.alignment() as u32)
            .unwrap();
        return Some(loaded);
    }

    let fields = slots
        .iter()
        .map(|slot| any_to_basic_type(bc_llvm_type(global_state.context, &slot._type)?))
        .collect::<Option<Vec<_>>>()?;
    let struct_type = global_state.context.struct_type(fields.as_slice(), false);
    let mut aggregate = struct_type.const_zero();
    let usize_type = global_state.pointer_int_type;
    let base = function_state
        .builder
        .build_ptr_to_int(memory, usize_type, inst_num().as_str())
        .unwrap();

    for (index, slot) in slots.iter().enumerate() {
        let offset = usize_type.const_int(slot.offset as u64, false);
        let ptr_int = function_state
            .builder
            .build_int_add(base, offset, inst_num().as_str())
            .unwrap();
        let field_ptr = function_state
            .builder
            .build_int_to_ptr(
                ptr_int,
                global_state
                    .context
                    .ptr_type(inkwell::AddressSpace::from(0)),
                inst_num().as_str(),
            )
            .unwrap();
        let field_ty = any_to_basic_type(bc_llvm_type(global_state.context, &slot._type)?)?;
        let field = function_state
            .builder
            .build_load(field_ty, field_ptr, inst_num().as_str())
            .unwrap();
        field
            .as_instruction_value()
            .unwrap()
            .set_alignment(slot._type.alignment() as u32)
            .unwrap();
        aggregate = function_state
            .builder
            .build_insert_value(aggregate, field, index as u32, inst_num().as_str())
            .unwrap()
            .into_struct_value();
    }

    Some(aggregate.as_basic_value_enum())
}

pub(super) fn generate_get_function_addr<'a>(
    global_state: &GlobalState<'a>,
    func: &str,
) -> Option<CodegenValue<'a>> {
    let function_val = global_state
        .module
        .get_function(func)
        .unwrap()
        .as_global_value()
        .as_pointer_value();

    // Calling as_any_value_enum() on a function pointer produces a
    // FunctionValue instead of the pointer value LLVM expects here.
    let any_value_enum = AnyValueEnum::PointerValue(function_val);

    Some(CodegenValue::Value(any_value_enum))
}
