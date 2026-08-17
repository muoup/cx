use crate::typing::{any_to_basic_type, bc_llvm_type};
use crate::{CodegenValue, FunctionState, GlobalState};
use cx_lmir::{LMIRValue, types::LMIRType};
use inkwell::intrinsics::Intrinsic;
use inkwell::values::AnyValue;

fn list_value<'a>(state: &FunctionState<'a, '_>, list: &LMIRValue) -> Option<inkwell::values::PointerValue<'a>> {
    Some(state.get_value(list)?.as_basic_value()?.into_pointer_value())
}

fn intrinsic<'a>(
    global: &GlobalState<'a>,
    name: &str,
    parameter_type: inkwell::types::BasicTypeEnum<'a>,
) -> Option<inkwell::values::FunctionValue<'a>> {
    Intrinsic::find(name)?.get_declaration(&global.module, &[parameter_type])
}

pub(super) fn generate_va_start<'a, 'b>(
    global: &GlobalState<'a>,
    state: &FunctionState<'a, 'b>,
    list: &LMIRValue,
) -> Option<CodegenValue<'a>> {
    let list = list_value(state, list)?;
    let function = intrinsic(global, "llvm.va_start", list.get_type().into())?;
    state.builder.build_call(function, &[list.into()], "").ok()?;
    Some(CodegenValue::Null)
}

pub(super) fn generate_va_end<'a, 'b>(
    global: &GlobalState<'a>,
    state: &FunctionState<'a, 'b>,
    list: &LMIRValue,
) -> Option<CodegenValue<'a>> {
    let list = list_value(state, list)?;
    let function = intrinsic(global, "llvm.va_end", list.get_type().into())?;
    state.builder.build_call(function, &[list.into()], "").ok()?;
    Some(CodegenValue::Null)
}

pub(super) fn generate_va_arg<'a, 'b>(
    global: &GlobalState<'a>,
    state: &FunctionState<'a, 'b>,
    list: &LMIRValue,
    ty: &LMIRType,
) -> Option<CodegenValue<'a>> {
    let list = list_value(state, list)?;
    let llvm_type = any_to_basic_type(bc_llvm_type(global.context, ty)?)?;
    let value = state
        .builder
        .build_va_arg(list, llvm_type, "")
        .ok()?;
    Some(CodegenValue::Value(value.as_any_value_enum()))
}
