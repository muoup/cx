use crate::error::{LLVMError, LLVMResult};
use crate::typing::{any_to_basic_type, bc_llvm_type};
use crate::{CodegenValue, FunctionState, GlobalState};
use cx_lmir::{LMIRValue, types::LMIRType};
use inkwell::intrinsics::Intrinsic;
use inkwell::values::AnyValue;

fn list_value<'a>(
    state: &FunctionState<'a, '_>,
    list: &LMIRValue,
) -> LLVMResult<inkwell::values::PointerValue<'a>> {
    Ok(state
        .get_value(list)?
        .as_basic_value()?
        .into_pointer_value())
}

fn intrinsic<'a>(
    global: &GlobalState<'a>,
    name: &str,
    parameter_type: inkwell::types::BasicTypeEnum<'a>,
) -> LLVMResult<inkwell::values::FunctionValue<'a>> {
    let intrinsic = Intrinsic::find(name)
        .ok_or_else(|| LLVMError::new(format!("LLVM intrinsic {name} was not found")))?;
    intrinsic
        .get_declaration(&global.module, &[parameter_type])
        .ok_or_else(|| LLVMError::new(format!("LLVM intrinsic {name} was not declared")))
}

pub(super) fn generate_va_start<'a, 'b>(
    global: &GlobalState<'a>,
    state: &FunctionState<'a, 'b>,
    list: &LMIRValue,
) -> LLVMResult<CodegenValue<'a>> {
    let list = list_value(state, list)?;
    let function = intrinsic(global, "llvm.va_start", list.get_type().into())?;
    state
        .builder
        .build_call(function, &[list.into()], "")
        .map_err(LLVMError::from_error)?;
    Ok(CodegenValue::Null)
}

pub(super) fn generate_va_end<'a, 'b>(
    global: &GlobalState<'a>,
    state: &FunctionState<'a, 'b>,
    list: &LMIRValue,
) -> LLVMResult<CodegenValue<'a>> {
    let list = list_value(state, list)?;
    let function = intrinsic(global, "llvm.va_end", list.get_type().into())?;
    state
        .builder
        .build_call(function, &[list.into()], "")
        .map_err(LLVMError::from_error)?;
    Ok(CodegenValue::Null)
}

pub(super) fn generate_va_arg<'a, 'b>(
    global: &GlobalState<'a>,
    state: &FunctionState<'a, 'b>,
    list: &LMIRValue,
    ty: &LMIRType,
) -> LLVMResult<CodegenValue<'a>> {
    let list = list_value(state, list)?;
    let llvm_type = any_to_basic_type(bc_llvm_type(global.context, ty)?)?;
    let value = state
        .builder
        .build_va_arg(list, llvm_type, "")
        .map_err(LLVMError::from_error)?;
    Ok(CodegenValue::Value(value.as_any_value_enum()))
}
