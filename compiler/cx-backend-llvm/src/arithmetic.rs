use inkwell::values::{AnyValue, AnyValueEnum, IntMathValue};
use cx_data_bytecode::BCIntBinOp;
use crate::{CodegenValue, FunctionState, GlobalState};

pub(crate) fn generate_int_binop<'a>(
    global_state: &GlobalState<'a>,
    function_state: &FunctionState<'a>,
    left_value: AnyValueEnum<'a>, right_value: AnyValueEnum<'a>, op: BCIntBinOp
) -> Option<CodegenValue<'a>> {
    if left_value.is_pointer_value() || right_value.is_pointer_value() {
        let (ptr, int) = if left_value.is_pointer_value() {
            (left_value.into_pointer_value(), right_value.into_int_value())
        } else {
            (right_value.into_pointer_value(), left_value.into_int_value())
        };
        
        return match op {
            BCIntBinOp::ADD => unsafe {
                Some(CodegenValue::Value(
                    function_state.builder
                        .build_in_bounds_gep(global_state.context.i8_type(), ptr, &[int], crate::instruction::inst_num().as_str())
                        .ok()?
                        .as_any_value_enum()
                ))
            }
            BCIntBinOp::SUB => unsafe {
                let negative = function_state.builder
                    .build_int_neg(int, crate::instruction::inst_num().as_str())
                    .ok()?
                    .as_any_value_enum();
                
                Some(CodegenValue::Value(
                    function_state.builder
                        .build_in_bounds_gep(global_state.context.i8_type(), ptr, &[negative.into_int_value()], crate::instruction::inst_num().as_str())
                        .ok()?
                        .as_any_value_enum()
                ))
            }
            
            _ => None,
        }
    }
    
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