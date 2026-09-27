use crate::log::{LLVMError, LLVMResult};
use crate::typing::{any_to_basic_type, bc_llvm_type};
use crate::{CodegenValue, FunctionState, GlobalState};
use cx_lmir::{LMIRValue, types::LMIRType};
use cx_log::catalogue::backend as catalogue;
use inkwell::intrinsics::Intrinsic;
use inkwell::types::BasicTypeEnum;
use inkwell::values::{AnyValue, FunctionValue, PointerValue};

fn list_value<'a>(
    state: &FunctionState<'a, '_>,
    list: &LMIRValue,
) -> LLVMResult<PointerValue<'a>> {
    Ok(state
        .get_value(list)?
        .as_basic_value()?
        .into_pointer_value())
}

fn intrinsic<'a>(
    global: &GlobalState<'a>,
    name: &str,
    parameter_type: BasicTypeEnum<'a>,
) -> LLVMResult<FunctionValue<'a>> {
    let intrinsic = Intrinsic::find(name).ok_or_else(|| {
        LLVMError::new(
            &catalogue::MISSING_ENTITY,
            (format!("intrinsic '{name}'"), "LLVM intrinsic table".into()),
        )
    })?;
    intrinsic
        .get_declaration(&global.module, &[parameter_type])
        .ok_or_else(|| {
            LLVMError::new(
                &catalogue::MISSING_ENTITY,
                (
                    format!("declaration for intrinsic '{name}'"),
                    "LLVM module".into(),
                ),
            )
        })
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
