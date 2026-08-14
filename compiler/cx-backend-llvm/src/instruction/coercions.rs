use super::support::inst_num;
use crate::typing::{any_to_basic_type, any_to_basic_val, bc_llvm_type};
use crate::{CodegenValue, FunctionState, GlobalState};
use cx_lmir::LMIRValue;
use cx_lmir::types::LMIRType;
use inkwell::AddressSpace;
use inkwell::values::AnyValue;

pub(super) fn generate_bit_cast<'a, 'b>(
    global_state: &GlobalState<'a>,
    function_state: &FunctionState<'a, 'b>,
    value: &LMIRValue,
    target_type: &LMIRType,
) -> Option<CodegenValue<'a>> {
    let value = any_to_basic_val(function_state.get_value(value)?.get_value())?;
    let target = any_to_basic_type(bc_llvm_type(global_state.context, target_type)?)?;
    let value = function_state
        .builder
        .build_bit_cast(value, target, inst_num().as_str())
        .unwrap();
    Some(CodegenValue::Value(value.as_any_value_enum()))
}

pub(super) fn generate_int_to_ptr<'a, 'b>(
    global_state: &GlobalState<'a>,
    function_state: &FunctionState<'a, 'b>,
    value: &LMIRValue,
) -> Option<CodegenValue<'a>> {
    let value = function_state
        .get_value(value)?
        .get_value()
        .into_int_value();
    let value = function_state
        .builder
        .build_int_to_ptr(
            value,
            global_state.context.ptr_type(AddressSpace::from(0)),
            inst_num().as_str(),
        )
        .unwrap();
    Some(CodegenValue::Value(value.as_any_value_enum()))
}

pub(super) fn generate_zextend<'a, 'b>(
    global_state: &GlobalState<'a>,
    function_state: &FunctionState<'a, 'b>,
    value: &LMIRValue,
    target_type: &LMIRType,
) -> Option<CodegenValue<'a>> {
    let value = function_state
        .get_value(value)?
        .get_value()
        .into_int_value();
    let target = bc_llvm_type(global_state.context, target_type)?.into_int_type();
    let value = function_state
        .builder
        .build_int_z_extend(value, target, inst_num().as_str())
        .unwrap();
    Some(CodegenValue::Value(value.as_any_value_enum()))
}

pub(super) fn generate_sextend<'a, 'b>(
    global_state: &GlobalState<'a>,
    function_state: &FunctionState<'a, 'b>,
    value: &LMIRValue,
    target_type: &LMIRType,
) -> Option<CodegenValue<'a>> {
    let value = function_state
        .get_value(value)?
        .get_value()
        .into_int_value();
    let target = bc_llvm_type(global_state.context, target_type)?.into_int_type();
    let value = function_state
        .builder
        .build_int_s_extend(value, target, inst_num().as_str())
        .unwrap();
    Some(CodegenValue::Value(value.as_any_value_enum()))
}

pub(super) fn generate_trunc<'a, 'b>(
    global_state: &GlobalState<'a>,
    function_state: &FunctionState<'a, 'b>,
    value: &LMIRValue,
    target_type: &LMIRType,
) -> Option<CodegenValue<'a>> {
    let value = function_state
        .get_value(value)?
        .get_value()
        .into_int_value();
    let target = bc_llvm_type(global_state.context, target_type)?.into_int_type();
    let value = function_state
        .builder
        .build_int_truncate(value, target, inst_num().as_str())
        .unwrap();
    Some(CodegenValue::Value(value.as_any_value_enum()))
}

pub(super) fn generate_int_to_float<'a, 'b>(
    global_state: &GlobalState<'a>,
    function_state: &FunctionState<'a, 'b>,
    value: &LMIRValue,
    target_type: &LMIRType,
    sextend: bool,
) -> Option<CodegenValue<'a>> {
    let value = function_state
        .get_value(value)?
        .get_value()
        .into_int_value();
    let target = bc_llvm_type(global_state.context, target_type)?.into_float_type();
    let value = if sextend {
        function_state
            .builder
            .build_signed_int_to_float(value, target, inst_num().as_str())
            .unwrap()
    } else {
        function_state
            .builder
            .build_unsigned_int_to_float(value, target, inst_num().as_str())
            .unwrap()
    };
    Some(CodegenValue::Value(value.as_any_value_enum()))
}

pub(super) fn generate_float_to_int<'a, 'b>(
    global_state: &GlobalState<'a>,
    function_state: &FunctionState<'a, 'b>,
    value: &LMIRValue,
    target_type: &LMIRType,
    sextend: bool,
) -> Option<CodegenValue<'a>> {
    let value = function_state
        .get_value(value)?
        .get_value()
        .into_float_value();
    let target = bc_llvm_type(global_state.context, target_type)?.into_int_type();
    let value = if sextend {
        function_state
            .builder
            .build_float_to_signed_int(value, target, inst_num().as_str())
            .unwrap()
    } else {
        function_state
            .builder
            .build_float_to_unsigned_int(value, target, inst_num().as_str())
            .unwrap()
    };
    Some(CodegenValue::Value(value.as_any_value_enum()))
}

pub(super) fn generate_ptr_to_int<'a, 'b>(
    global_state: &GlobalState<'a>,
    function_state: &FunctionState<'a, 'b>,
    value: &LMIRValue,
    target_type: &LMIRType,
) -> Option<CodegenValue<'a>> {
    let value = function_state
        .get_value(value)?
        .get_value()
        .into_pointer_value();
    let target = bc_llvm_type(global_state.context, target_type)?.into_int_type();
    let value = function_state
        .builder
        .build_ptr_to_int(value, target, inst_num().as_str())
        .unwrap();
    Some(CodegenValue::Value(value.as_any_value_enum()))
}

pub(super) fn generate_float_cast<'a, 'b>(
    global_state: &GlobalState<'a>,
    function_state: &FunctionState<'a, 'b>,
    value: &LMIRValue,
    target_type: &LMIRType,
) -> Option<CodegenValue<'a>> {
    let value = function_state
        .get_value(value)?
        .get_value()
        .into_float_value();
    let target = bc_llvm_type(global_state.context, target_type)?.into_float_type();
    let value = function_state
        .builder
        .build_float_cast(value, target, inst_num().as_str())
        .unwrap();
    Some(CodegenValue::Value(value.as_any_value_enum()))
}
