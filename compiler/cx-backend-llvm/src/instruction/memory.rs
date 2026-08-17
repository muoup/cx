use super::inst_num;
use crate::error::{LLVMError, LLVMResult};
use crate::typing::{any_to_basic_type, any_to_basic_val, bc_llvm_type};
use crate::{CodegenValue, FunctionState, GlobalState};
use cx_lmir::LMIRValue;
use cx_lmir::types::LMIRType;
use cx_util::identifier::CXIdent;
use inkwell::AddressSpace;
use inkwell::types::AnyTypeEnum;
use inkwell::values::{AnyValue, AnyValueEnum, BasicValue};

pub(super) fn generate_allocate<'a, 'b>(
    function_state: &FunctionState<'a, 'b>,
    _type: &LMIRType,
    alignment: u8,
) -> LLVMResult<CodegenValue<'a>> {
    let basic_ty = function_state
        .context
        .i8_type()
        .array_type(usize::from(_type.size()) as u32);
    let previous_block = function_state
        .builder
        .get_insert_block()
        .ok_or_else(|| LLVMError::new("No LLVM insertion block while allocating memory"))?;
    let entry = function_state.get_block(&CXIdent::from("entry"))?;
    function_state.builder.position_before(
        entry
            .get_first_instruction()
            .as_ref()
            .ok_or_else(|| LLVMError::new("LLVM entry block has no instruction"))?,
    );

    let value = function_state
        .builder
        .build_alloca(basic_ty, inst_num().as_str())
        .map_err(LLVMError::from_error)?;
    value
        .as_instruction()
        .ok_or_else(|| LLVMError::new("LLVM alloca did not produce an instruction"))?
        .set_alignment(alignment as u32)
        .map_err(LLVMError::from_error)?;
    function_state.builder.position_at_end(previous_block);

    Ok(CodegenValue::Value(value.as_any_value_enum()))
}

pub(super) fn generate_struct_access<'a, 'b>(
    function_state: &FunctionState<'a, 'b>,
    struct_: &LMIRValue,
    struct_type: &LMIRType,
    field_index: usize,
) -> LLVMResult<CodegenValue<'a>> {
    let struct_type = match bc_llvm_type(function_state.context, struct_type)? {
        AnyTypeEnum::StructType(struct_type) => struct_type,
        llvm_type => {
            return Err(LLVMError::new(format!(
                "Expected struct type for struct access, found {llvm_type:?}"
            )));
        }
    };
    let struct_ptr = function_state
        .get_value(struct_)?
        .get_value()?
        .into_pointer_value();
    let gep = function_state
        .builder
        .build_struct_gep(
            struct_type,
            struct_ptr,
            field_index as u32,
            inst_num().as_str(),
        )
        .map_err(LLVMError::from_error)?;
    Ok(CodegenValue::Value(gep.as_any_value_enum()))
}

pub(super) fn generate_store<'a, 'b>(
    global_state: &GlobalState<'a>,
    function_state: &FunctionState<'a, 'b>,
    memory: &LMIRValue,
    value: &LMIRValue,
    _type: &LMIRType,
) -> LLVMResult<CodegenValue<'a>> {
    let codegen_value = function_state.get_value(value)?;
    let memory = function_state
        .get_value(memory)?
        .get_value()?
        .into_pointer_value();

    match codegen_value {
        CodegenValue::AggregateSlots(values) => {
            let usize_type = global_state.pointer_int_type;
            let base = function_state
                .builder
                .build_ptr_to_int(memory, usize_type, inst_num().as_str())
                .map_err(LLVMError::from_error)?;
            for (slot, value) in values {
                let offset = usize_type.const_int(slot.offset as u64, false);
                let ptr_int = function_state
                    .builder
                    .build_int_add(base, offset, inst_num().as_str())
                    .map_err(LLVMError::from_error)?;
                let field_ptr = function_state
                    .builder
                    .build_int_to_ptr(
                        ptr_int,
                        global_state.context.ptr_type(AddressSpace::from(0)),
                        inst_num().as_str(),
                    )
                    .map_err(LLVMError::from_error)?;
                let store = function_state
                    .builder
                    .build_store(field_ptr, value)
                    .map_err(LLVMError::from_error)?;
                store
                    .set_alignment(slot._type.alignment() as u32)
                    .map_err(LLVMError::from_error)?;
            }
        }
        CodegenValue::Value(any_value) => {
            let basic_value = any_to_basic_val(any_value)?;
            let store = function_state
                .builder
                .build_store(memory, basic_value)
                .map_err(LLVMError::from_error)?;
            store
                .set_alignment(_type.alignment() as u32)
                .map_err(LLVMError::from_error)?;
        }
        CodegenValue::Null => {}
    }

    Ok(CodegenValue::Null)
}

pub(super) fn generate_memcpy<'a, 'b>(
    function_state: &FunctionState<'a, 'b>,
    dest: &LMIRValue,
    src: &LMIRValue,
    size: &LMIRValue,
    alignment: u8,
) -> LLVMResult<CodegenValue<'a>> {
    let src = match function_state.get_value(src)?.get_value()? {
        AnyValueEnum::PointerValue(value) => value,
        value => {
            let value = any_to_basic_val(value)?;
            let temporary = function_state
                .builder
                .build_alloca(value.get_type(), inst_num().as_str())
                .map_err(LLVMError::from_error)?;
            function_state
                .builder
                .build_store(temporary, value)
                .map_err(LLVMError::from_error)?;
            temporary
        }
    };
    let dest = function_state
        .get_value(dest)?
        .get_value()?
        .into_pointer_value();
    let size = function_state
        .get_value(size)?
        .get_value()?
        .into_int_value();
    function_state
        .builder
        .build_memcpy(dest, alignment as u32, src, alignment as u32, size)
        .map_err(LLVMError::from_error)?;
    Ok(CodegenValue::Null)
}

pub(super) fn generate_load<'a, 'b>(
    global_state: &GlobalState<'a>,
    function_state: &FunctionState<'a, 'b>,
    memory: &LMIRValue,
    _type: &LMIRType,
) -> LLVMResult<CodegenValue<'a>> {
    let memory = function_state
        .get_value(memory)?
        .get_value()?
        .into_pointer_value();
    let loaded = function_state
        .builder
        .build_load(
            any_to_basic_type(bc_llvm_type(global_state.context, _type)?)?,
            memory,
            inst_num().as_str(),
        )
        .map_err(LLVMError::from_error)?;
    loaded
        .as_instruction_value()
        .ok_or_else(|| LLVMError::new("LLVM load did not produce an instruction"))?
        .set_alignment(_type.alignment() as u32)
        .map_err(LLVMError::from_error)?;
    Ok(CodegenValue::Value(loaded.as_any_value_enum()))
}

pub(super) fn generate_zero_memory<'a, 'b>(
    global_state: &GlobalState<'a>,
    function_state: &FunctionState<'a, 'b>,
    memory: &LMIRValue,
    _type: &LMIRType,
) -> LLVMResult<CodegenValue<'a>> {
    let memory = function_state
        .get_value(memory)?
        .get_value()?
        .into_pointer_value();
    let zero = global_state.context.i8_type().const_zero();
    let size = global_state
        .pointer_int_type
        .const_int(usize::from(_type.size()) as u64, false);
    function_state
        .builder
        .build_memset(memory, _type.alignment() as u32, zero, size)
        .map_err(LLVMError::from_error)?;
    Ok(CodegenValue::Null)
}
