use super::inst_num;
use crate::typing::{any_to_basic_type, any_to_basic_val, bc_llvm_type};
use crate::{CodegenValue, FunctionState, GlobalState};
use cx_lmir::LMIRValue;
use cx_lmir::types::LMIRType;
use cx_util::identifier::CXIdent;
use inkwell::AddressSpace;
use inkwell::types::AnyTypeEnum;
use inkwell::values::{AnyValue, BasicValue};

pub(super) fn generate_allocate<'a, 'b>(
    function_state: &FunctionState<'a, 'b>,
    _type: &LMIRType,
    alignment: u8,
) -> Option<CodegenValue<'a>> {
    let basic_ty = function_state
        .context
        .i8_type()
        .array_type(usize::from(_type.size()) as u32);
    let previous_block = function_state.builder.get_insert_block()?;
    let entry = function_state
        .get_block(&CXIdent::from("entry"))
        .expect("failed to get entry block");
    function_state.builder.position_before(
        entry
            .get_first_instruction()
            .as_ref()
            .expect("entry block must contain an instruction"),
    );

    let value = function_state
        .builder
        .build_alloca(basic_ty, inst_num().as_str())
        .unwrap();
    value
        .as_instruction()
        .unwrap()
        .set_alignment(alignment as u32)
        .unwrap();
    function_state.builder.position_at_end(previous_block);

    Some(CodegenValue::Value(value.as_any_value_enum()))
}

pub(super) fn generate_struct_access<'a, 'b>(
    function_state: &FunctionState<'a, 'b>,
    struct_: &LMIRValue,
    struct_type: &LMIRType,
    field_index: usize,
) -> Option<CodegenValue<'a>> {
    let Some(AnyTypeEnum::StructType(struct_type)) =
        bc_llvm_type(function_state.context, struct_type)
    else {
        unreachable!("expected struct type for struct access, got: {struct_type:?}");
    };
    let struct_ptr = function_state
        .get_value(struct_)?
        .get_value()
        .into_pointer_value();
    let gep = function_state
        .builder
        .build_struct_gep(
            struct_type,
            struct_ptr,
            field_index as u32,
            inst_num().as_str(),
        )
        .unwrap();
    Some(CodegenValue::Value(gep.as_any_value_enum()))
}

pub(super) fn generate_store<'a, 'b>(
    global_state: &GlobalState<'a>,
    function_state: &FunctionState<'a, 'b>,
    memory: &LMIRValue,
    value: &LMIRValue,
    _type: &LMIRType,
) -> Option<CodegenValue<'a>> {
    let codegen_value = function_state.get_value(value)?;
    let memory = function_state
        .get_value(memory)?
        .get_value()
        .into_pointer_value();

    match codegen_value {
        CodegenValue::AggregateSlots(values) => {
            let usize_type = global_state.pointer_int_type;
            let base = function_state
                .builder
                .build_ptr_to_int(memory, usize_type, inst_num().as_str())
                .unwrap();
            for (slot, value) in values {
                let offset = usize_type.const_int(slot.offset as u64, false);
                let ptr_int = function_state
                    .builder
                    .build_int_add(base, offset, inst_num().as_str())
                    .unwrap();
                let field_ptr = function_state
                    .builder
                    .build_int_to_ptr(
                        ptr_int,
                        global_state.context.ptr_type(AddressSpace::from(0)),
                        inst_num().as_str(),
                    )
                    .unwrap();
                let store = function_state
                    .builder
                    .build_store(field_ptr, value)
                    .unwrap();
                store.set_alignment(slot._type.alignment() as u32).unwrap();
            }
        }
        CodegenValue::Value(any_value) => {
            let basic_value = any_to_basic_val(any_value)?;
            let store = function_state
                .builder
                .build_store(memory, basic_value)
                .unwrap();
            store.set_alignment(_type.alignment() as u32).unwrap();
        }
        CodegenValue::Null => {}
    }

    Some(CodegenValue::Null)
}

pub(super) fn generate_memcpy<'a, 'b>(
    function_state: &FunctionState<'a, 'b>,
    dest: &LMIRValue,
    src: &LMIRValue,
    size: &LMIRValue,
    alignment: u8,
) -> Option<CodegenValue<'a>> {
    let src = function_state
        .get_value(src)?
        .get_value()
        .into_pointer_value();
    let dest = function_state
        .get_value(dest)?
        .get_value()
        .into_pointer_value();
    let size = function_state.get_value(size)?.get_value().into_int_value();
    function_state
        .builder
        .build_memcpy(dest, alignment as u32, src, alignment as u32, size)
        .unwrap();
    Some(CodegenValue::Null)
}

pub(super) fn generate_load<'a, 'b>(
    global_state: &GlobalState<'a>,
    function_state: &FunctionState<'a, 'b>,
    memory: &LMIRValue,
    _type: &LMIRType,
) -> Option<CodegenValue<'a>> {
    let memory = function_state
        .get_value(memory)?
        .get_value()
        .into_pointer_value();
    let loaded = function_state
        .builder
        .build_load(
            any_to_basic_type(bc_llvm_type(global_state.context, _type)?)?,
            memory,
            inst_num().as_str(),
        )
        .unwrap();
    loaded
        .as_instruction_value()
        .unwrap()
        .set_alignment(_type.alignment() as u32)
        .unwrap();
    Some(CodegenValue::Value(loaded.as_any_value_enum()))
}

pub(super) fn generate_zero_memory<'a, 'b>(
    global_state: &GlobalState<'a>,
    function_state: &FunctionState<'a, 'b>,
    memory: &LMIRValue,
    _type: &LMIRType,
) -> Option<CodegenValue<'a>> {
    let memory = function_state
        .get_value(memory)?
        .get_value()
        .into_pointer_value();
    let zero = global_state.context.i8_type().const_zero();
    let size = global_state
        .pointer_int_type
        .const_int(usize::from(_type.size()) as u64, false);
    function_state
        .builder
        .build_memset(memory, _type.alignment() as u32, zero, size)
        .unwrap();
    Some(CodegenValue::Null)
}
