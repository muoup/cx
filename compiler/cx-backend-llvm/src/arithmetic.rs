use inkwell::values::{AnyValue, AnyValueEnum, IntMathValue};
use cx_data_bytecode::{BCIntBinOp, BCPtrBinOp};
use cx_data_bytecode::types::BCType;
use crate::{CodegenValue, FunctionState, GlobalState};
use crate::typing::{any_to_basic_type, cx_llvm_type};

pub(crate) fn generate_ptr_binop<'a>(
    global_state: &GlobalState<'a>, function_state: &FunctionState<'a>,
    ptr_type: &BCType, left_value: AnyValueEnum<'a>, right_value: AnyValueEnum<'a>, op: BCPtrBinOp
) -> Option<CodegenValue<'a>> {
    let ptr_type = cx_llvm_type(global_state, ptr_type)?;

    Some(
        CodegenValue::Value(
            match op {
                BCPtrBinOp::ADD => unsafe {
                    let basic_type = any_to_basic_type(ptr_type)
                        .expect("Expected a basic type for pointer addition");

                    function_state.builder
                        .build_in_bounds_gep(basic_type,
                                             left_value.into_pointer_value(), &[right_value.into_int_value()],
                                             crate::instruction::inst_num().as_str())
                        .ok()?
                        .as_any_value_enum()
                }
                BCPtrBinOp::SUB => unsafe {
                    let basic_type = any_to_basic_type(ptr_type)
                        .expect("Expected a basic type for pointer subtraction");

                    let negative = function_state.builder
                        .build_int_neg(right_value.into_pointer_value(), crate::instruction::inst_num().as_str())
                        .ok()?
                        .as_any_value_enum();

                    function_state.builder
                        .build_in_bounds_gep(basic_type,
                                             left_value.into_pointer_value(), &[negative.into_int_value()],
                                             crate::instruction::inst_num().as_str())
                        .ok()?
                        .as_any_value_enum()
                }
                BCPtrBinOp::EQ =>
                    function_state.builder
                        .build_int_compare(
                            inkwell::IntPredicate::EQ,
                            left_value.into_pointer_value(), right_value.into_pointer_value(),
                            crate::instruction::inst_num().as_str()
                        )
                        .ok()
                        .unwrap()
                        .as_any_value_enum(),
                BCPtrBinOp::NE => 
                    function_state.builder
                        .build_int_compare(
                            inkwell::IntPredicate::NE,
                            left_value.into_pointer_value(), right_value.into_pointer_value(),
                            crate::instruction::inst_num().as_str()
                        )
                        .ok()?
                        .as_any_value_enum(),
                BCPtrBinOp::LT =>
                    function_state.builder
                        .build_int_compare(
                            inkwell::IntPredicate::ULT,
                            left_value.into_pointer_value(), right_value.into_pointer_value(),
                            crate::instruction::inst_num().as_str()
                        )
                        .ok()?
                        .as_any_value_enum(),
                BCPtrBinOp::LE =>
                    function_state.builder
                        .build_int_compare(
                            inkwell::IntPredicate::ULE,
                            left_value.into_pointer_value(), right_value.into_pointer_value(),
                            crate::instruction::inst_num().as_str()
                        )
                        .ok()?
                        .as_any_value_enum(),
                BCPtrBinOp::GT => 
                    function_state.builder
                        .build_int_compare(
                            inkwell::IntPredicate::UGT,
                            left_value.into_pointer_value(), right_value.into_pointer_value(),
                            crate::instruction::inst_num().as_str()
                        )
                        .ok()?
                        .as_any_value_enum(),
                BCPtrBinOp::GE =>
                    function_state.builder
                        .build_int_compare(
                            inkwell::IntPredicate::UGE,
                            left_value.into_pointer_value(), right_value.into_pointer_value(),
                            crate::instruction::inst_num().as_str()
                        )
                        .ok()?
                        .as_any_value_enum(),
            }
        )
    )
}

pub(crate) fn generate_int_binop<'a>(
    _: &GlobalState<'a>,
    function_state: &FunctionState<'a>,
    left_value: AnyValueEnum<'a>, right_value: AnyValueEnum<'a>, op: BCIntBinOp
) -> Option<CodegenValue<'a>> {
    let left_value = left_value.into_int_value();
    let right_value = right_value.into_int_value();
    
    Some(
        CodegenValue::Value(
            match op {
                BCIntBinOp::ADD => function_state.builder
                    .build_int_add(left_value, right_value, crate::instruction::inst_num().as_str())
                    .ok()?
                    .as_any_value_enum(),
                BCIntBinOp::SUB => function_state.builder
                    .build_int_sub(left_value, right_value, crate::instruction::inst_num().as_str())
                    .ok()?
                    .as_any_value_enum(),
                BCIntBinOp::MUL => function_state.builder
                    .build_int_mul(left_value, right_value, crate::instruction::inst_num().as_str())
                    .ok()?
                    .as_any_value_enum(),
                BCIntBinOp::IDIV => function_state.builder
                    .build_int_signed_div(left_value, right_value, crate::instruction::inst_num().as_str())
                    .ok()?
                    .as_any_value_enum(),
                BCIntBinOp::UDIV => function_state.builder
                    .build_int_unsigned_div(left_value, right_value, crate::instruction::inst_num().as_str())
                    .ok()?
                    .as_any_value_enum(),
                BCIntBinOp::IREM => function_state.builder
                    .build_int_signed_rem(left_value, right_value, crate::instruction::inst_num().as_str())
                    .ok()?
                    .as_any_value_enum(),
                BCIntBinOp::UREM => function_state.builder
                    .build_int_unsigned_rem(left_value, right_value, crate::instruction::inst_num().as_str())
                    .ok()?
                    .as_any_value_enum(),
                BCIntBinOp::IGT => function_state.builder
                    .build_int_compare(
                        inkwell::IntPredicate::SGT,
                        left_value,
                        right_value,
                        crate::instruction::inst_num().as_str()
                    )
                    .ok()?
                    .as_any_value_enum(),
                BCIntBinOp::UGT => function_state.builder
                    .build_int_compare(
                        inkwell::IntPredicate::UGT,
                        left_value,
                        right_value,
                        crate::instruction::inst_num().as_str()
                    )
                    .ok()?
                    .as_any_value_enum(),
                BCIntBinOp::IGE => function_state.builder
                    .build_int_compare(
                        inkwell::IntPredicate::SGE,
                        left_value,
                        right_value,
                        crate::instruction::inst_num().as_str()
                    )
                    .ok()?
                    .as_any_value_enum(),
                BCIntBinOp::UGE => function_state.builder
                    .build_int_compare(
                        inkwell::IntPredicate::UGE,
                        left_value,
                        right_value,
                        crate::instruction::inst_num().as_str()
                    )
                    .ok()?
                    .as_any_value_enum(),
                BCIntBinOp::ILT => function_state.builder
                    .build_int_compare(
                        inkwell::IntPredicate::SLT,
                        left_value,
                        right_value,
                        crate::instruction::inst_num().as_str()
                    )
                    .ok()?
                    .as_any_value_enum(),
                BCIntBinOp::ULT => function_state.builder
                    .build_int_compare(
                        inkwell::IntPredicate::ULT,
                        left_value,
                        right_value,
                        crate::instruction::inst_num().as_str()
                    )
                    .ok()?
                    .as_any_value_enum(),
                BCIntBinOp::ILE => function_state.builder
                    .build_int_compare(
                        inkwell::IntPredicate::SLE,
                        left_value,
                        right_value,
                        crate::instruction::inst_num().as_str()
                    )
                    .ok()?
                    .as_any_value_enum(),

                BCIntBinOp::ASHR => function_state.builder
                    .build_right_shift(
                        left_value,
                        right_value,
                        true,
                        crate::instruction::inst_num().as_str()
                    )
                    .ok()?
                    .as_any_value_enum(),
                BCIntBinOp::LSHR => function_state.builder
                    .build_right_shift(
                        left_value,
                        right_value,
                        false,
                        crate::instruction::inst_num().as_str()
                    )
                    .ok()?
                    .as_any_value_enum(),

                BCIntBinOp::SHL => function_state.builder
                    .build_left_shift(
                        left_value,
                        right_value,
                        crate::instruction::inst_num().as_str()
                    )
                    .ok()?
                    .as_any_value_enum(),

                BCIntBinOp::BAND => function_state.builder
                    .build_and(left_value, right_value, crate::instruction::inst_num().as_str())
                    .ok()?
                    .as_any_value_enum(),
                BCIntBinOp::BOR => function_state.builder
                    .build_or(left_value, right_value, crate::instruction::inst_num().as_str())
                    .ok()?
                    .as_any_value_enum(),
                BCIntBinOp::BXOR => function_state.builder
                    .build_xor(left_value, right_value, crate::instruction::inst_num().as_str())
                    .ok()?
                    .as_any_value_enum(),
                BCIntBinOp::LAND => function_state.builder
                    .build_int_compare(
                        inkwell::IntPredicate::NE,
                        left_value,
                        right_value,
                        crate::instruction::inst_num().as_str()
                    )
                    .ok()?
                    .as_any_value_enum(),
                BCIntBinOp::LOR => function_state.builder
                    .build_int_compare(
                        inkwell::IntPredicate::NE,
                        left_value,
                        right_value,
                        crate::instruction::inst_num().as_str()
                    )
                    .ok()?
                    .as_any_value_enum(),
                BCIntBinOp::EQ => function_state.builder
                    .build_int_compare(
                        inkwell::IntPredicate::EQ,
                        left_value,
                        right_value,
                        crate::instruction::inst_num().as_str()
                    )
                    .ok()?
                    .as_any_value_enum(),
                BCIntBinOp::NE => function_state.builder
                    .build_int_compare(
                        inkwell::IntPredicate::NE,
                        left_value,
                        right_value,
                        crate::instruction::inst_num().as_str()
                    )
                    .ok()?
                    .as_any_value_enum(),
                BCIntBinOp::ULE => function_state.builder
                    .build_int_compare(
                        inkwell::IntPredicate::ULE,
                        left_value,
                        right_value,
                        crate::instruction::inst_num().as_str()
                    )
                    .ok()?
                    .as_any_value_enum(),
            }
        )
    )
}